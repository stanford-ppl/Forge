package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.transform.MultiPassTransformer

import dhdl.compiler._
import dhdl.compiler.ops._

trait UnrollingExp extends PipeStageToolsExp with LoweredOpsExp { this: DHDLExp => }

trait UnrollingTransformer extends MultiPassTransformer with PipeStageTools {
  val IR: UnrollingExp
  import IR.{infix_until => _, _}

  def unrollParallel[A](cchain: Exp[CounterChain], inds: List[Exp[Index]])(func: List[Sym[Idx]] => A): List[A] = {
    val Ps = parOf(cchain)
    val P = Ps.reduce(_*_)
    val N = Ps.length

    val prods = List.tabulate(N){i => Ps.slice(i+1,N).reduce(_*_) }

    // Can actually have this in an outer reifyBlock since it has no defs or effects
    val indices = Ps.map{p => List.fill(p){ fresh[Idx] } }

    val vectors = indices.map{inds => Vector(inds:_*) }
    (0 until P).foreach{p =>
      val is = vectors.zipWithIndex.map{case (vec,d) => vec((p / prods(d)) % Ps(d)) }
      f(is)
    }
  }

  def unrollOuterForeach[A:Manifest](cchain: Exp[CounterChain], func: Block[A], inds: List[Exp[Index]]): Exp[A] = {
    val dims = parOf(cchain)
    val N = dims.length
    val prods = List.tabulate(N){i => dims.slice(i+1,N).reduce{_*_}}

    val P = dims.reduce(_*_)
    if (P > 1) {
      val stages = getControlNodes(func)

      val indVectors = inds.map{i => expandBus(i) }

      val newStages = stages.map{stage =>
        Parallel {
          (0 until P).foreach{p =>
            val slicedInds = indVectors.zipWithIndex.map{case (bus,d) => bus(((p / prods(d)) % dims(d)) }
            withSubstScope(inds.zip(slicedInds):_*){ mirrorExp(stage) }
          }
        }
      }

      // Traverse the block to add the transformed versions to the IR
      withSubstScope(stages.zip(newStages):_*){ inlineBlock(func) }
    }
    else inlineBlock(func)
  }

  def unrollReduce[A:Manifest](mapBus: Exp[A], rFunc: Block[A], iFunc: Block[Index], ld: Block[A], st: Block[Unit], idx: Exp[Index], res: Exp[A], rV: (Sym[A],Sym[A])) = {
    def reduce(x: Exp[A], y: Exp[A]) = withSubstScope(rV._1 -> x, rV._2 -> y){ inlineBlock(rFunc) }

    val treeResult = reductionTree(mapBus, {(x,y) => reduce(x,y) }).head
    val newIdx = inlineBlock(iFunc)
    val accumLoad = withSubstScope(idx -> newIdx){ inlineBlock(ld) }
    val newRes = reduce(treeResult, accumLoad)
    withSubstScope(res -> newRes, idx -> newIdx){ inlineBlock(st) }
  }

  // Create a single block with map + reduce + load + reduce + store
  // Still have to keep acc separate to make sure load isn't lifted out of the block (e.g. for registers)
  def unrollMapReduce[A:Manifest](cc: Exp[CounterChain], func: Block[A], rFunc: Block[A], iFunc: Block[Index], ld: Block[A], st: Block[Unit], idx: Exp[Index], res: Exp[A], rV: (Sym[A],Sym[A]), innerLoop: Boolean): Block[Unit] = {
    val P = parOf(cc).reduce(_*_)

    if (innerLoop) reifyBlock {
      val funcRes = inlineBlock(func)
      val mapBus = expandBus(funcRes)
      unrollReduce(mapBus, rFunc, iFunc, ld, st, idx, res, rV)
    }
    else reifyBlock {
      unrollForeach(cc, func, inds) // Unroll map
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
