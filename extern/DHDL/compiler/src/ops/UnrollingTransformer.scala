package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.transform.MultiPassTransformer

import dhdl.compiler._
import dhdl.compiler.ops._

trait UnrollingExp extends PipeStageToolsExp with LoweredOpsExp { this: DHDLExp => }

trait UnrollingTransformer extends MultiPassTransformer with PipeStageTools {
  val IR: UnrollingExp
  import IR.{infix_until => _, _}

  def unrollInnerMap[A:Manifest](cchain: Exp[CounterChain], func: Block[A], inds: List[Exp[Index]]) = {
    val Ps = parOf(cchain)
    val P = Ps.reduce(_*_)
    val N = Ps.length
    val prods = List.tabulate(N){i => Ps.slice(i+1,N).reduce(_*_) }
    val indices = Ps.map{p => List.fill(p){ fresh[Index] } }

    val out = (0 until P).map{p =>
      val inds2 = indices.zipWithIndex.map{case (vec,d) => vec((p / prods(d)) % Ps(d)) }
      withSubstScope(inds -> inds2){ inlineBlock(func) }
    }
    (indices, out.toList)
  }

  def unrollOuterMap[A:Manifest](cchain: Exp[CounterChain], func: Block[A], inds: List[Exp[Index]]) = {
    val Ps = parOf(cchain)
    val P = Ps.reduce(_*_)
    val N = Ps.length
    val prods = List.tabulate(N){i => Ps.slice(i+1,N).reduce(_*_) }
    val indices = Ps.map{p => List.fill(p){ fresh[Index] } }

    var out: List[Exp[A]] = Nil
    var dupSubst = Array.tabulate(P){i => subst}

    def duplicateStage(s: Exp[Any], d: Exp[Any], i: Int) {
      val outerSubst = subst

      (0 until N).foreach{i =>
        subst = dupSubst(i)
        val mirroredStage = self_mirror(s,d)
        subst += s -> mirroredStage
        if (i == stms.length - 1) out ::= mirroredStage
        dupSubst(i) = subst
      }

      subst = outerSubst
    }

    focusBlock(func){
      focusExactScope(func){ stms =>
        stms.zipWithIndex.foreach {
          case (TP(s,d),i) if isControlNode(s) && P > 1 => Parallel { duplicateStage(s,d,i) }
          case (TP(s,d),i) => duplicateStage(s,d,i)
        }
      }
    }
    (indices, out)
  }

  def unrollInnerForeach(cchain: Exp[CounterChain], func: Block[Unit], inds: List[Exp[Index]]) = {
    var indices: List[List[Sym[Index]]] = Nil
    val blk = reifyBlock {
      val (unrolledInds, _) = unrollInnerMap(cchain, func, inds)
      indices :::= unrolledInds
    }
    (indices, blk)
  }

  def unrollOuterForeach(cchain: Exp[CounterChain], func: Block[Unit], inds: List[Exp[Index]]): List[Exp[A]] = {
    var indices: List[List[Sym[Index]]] = Nil
    val blk = reifyBlock {
      val (unrolledInds, _) = unrollOuterMap(cchain, func, inds)
      indices :::= unrolledInds
    }
    (indices, blk)
  }

  def unrollReduce[A:Manifest](mapBus: List[Exp[A]], rFunc: Block[A], iFunc: Block[Index], ld: Block[A], st: Block[Unit], idx: Exp[Index], res: Exp[A], rV: (Sym[A],Sym[A])) = {
    def reduce(x: Exp[A], y: Exp[A]) = withSubstScope(rV._1 -> x, rV._2 -> y){ inlineBlock(rFunc) }

    val treeResult = reductionTree(mapBus, {(x,y) => reduce(x,y) }).head
    val newIdx = inlineBlock(iFunc)
    val accumLoad = withSubstScope(idx -> newIdx){ inlineBlock(ld) }
    val newRes = reduce(treeResult, accumLoad)
    withSubstScope(res -> newRes, idx -> newIdx){ inlineBlock(st) }
  }

  // Create a single block with map + reduce + load + reduce + store
  // Still have to keep acc separate to make sure load isn't lifted out of the block (e.g. for registers)
  def unrollPipeFold[A:Manifest](cc: Exp[CounterChain], func: Block[A], rFunc: Block[A], iFunc: Block[Index], ld: Block[A], st: Block[Unit], idx: Exp[Index], res: Exp[A], rV: (Sym[A],Sym[A]), innerLoop: Boolean): Block[Unit] = {
    val P = parOf(cc).reduce(_*_)

    if (innerLoop) reifyBlock {
      val (unrolledInds, mapRes) = unrollInnerMap(cchain, func, inds)
      unrollReduce(mapRes, rFunc, iFunc, ld, st, idx, res, rV)
    }
    else reifyBlock {
      val (unrolledInds, mapRes) = unrollOuterMap(cc, func, inds)
      Pipe {
        unrollReduce(mapRes, rFunc, iFunc, ld, st, idx, res, rV)
      }
    }
  }

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = rhs match {
    case Reflect(Pipe_foreach(cchain, func, inds), u, es) if isOuterLoop(lhs) =>
      val cc = f(cchain)
      val unrolledFunc = reifyBlock{ unrollOuterForeach(cc, func, inds) }
      val newPipe = reflectMirrored(Reflect(Pipe_foreach(cc,unrolledFunc,inds)(ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
      setProps(newPipe, getProps(lhs))
      Some(newPipe)

    case Reflect(e@Pipe_fold(cchain,accum,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV), u, es) if isInnerLoop(lhs) =>
      val cc = f(cchain)
      val accum2 = f(accum)
      val unrolledMapReduce = unrollInnerMapReduce(cc, func, rFunc, iFunc, ld, st, idx, res, rV)
      val newPipe = reflectMirrored(Reflect(Pipe_fold(cc, accum2, unrolledMapReduce, inds, acc)(ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
      setProps(newPipe, getProps(lhs))
      Some(newPipe)

    case Reflect(e@Pipe_reduce(cchain,accum,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV), u, es) if isOuterLoop(lhs) =>
    case Reflect(e@Block_reduce(c1,c2,a,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV), u, es) if isOuterLoop(lhs) =>

    case _ => None
  }
}
