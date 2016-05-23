package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.transform.MultiPassTransformer
import scala.collection.mutable.HashMap

import dhdl.compiler._
import dhdl.compiler.ops._

trait UnrollingTransformExp extends PipeStageToolsExp with LoweredPipeOpsExp { this: DHDLExp => }

// TODO: Need to handle edge cases - currently par factor must be a divider of number of iterations
trait UnrollingTransformer extends MultiPassTransformer with PipeStageTools {
  val IR: UnrollingTransformExp with DHDLExp
  import IR.{infix_until => _, _}

  debugMode = true
  override val name = "Unrolling Transformer"

  def unrollInnerMap[A:Manifest](cchain: Exp[CounterChain], func: Block[A], inds: List[Exp[Index]]) = {
    val Ps = parsOf(cchain)
    val P = Ps.reduce(_*_)
    val N = Ps.length
    val prods = List.tabulate(N){i => Ps.slice(i+1,N).fold(1)(_*_) }
    val indices = Ps.map{p => List.fill(p){ fresh[Index] } }

    val out = (0 until P).map{p =>
      val inds2 = indices.zipWithIndex.map{case (vec,d) => vec((p / prods(d)) % Ps(d)) }
      withSubstScope(inds.zip(inds2):_*){ inlineBlock(func) }
    }
    (indices, out.toList)
  }

  def unrollOuterMap[A:Manifest](cchain: Exp[CounterChain], func: Block[A], inds: List[Exp[Index]]) = {
    val Ps = parsOf(cchain)
    val P = Ps.reduce(_*_)
    val N = Ps.length
    val prods = List.tabulate(N){i => Ps.slice(i+1,N).fold(1)(_*_) }
    val indices = Ps.map{p => List.fill(p){ fresh[Index] } }

    var out: List[Exp[A]] = Nil
    val dupSubst = HashMap[Int,Map[Exp[Any],Exp[Any]]]()
    (0 until P).foreach{p =>
      val inds2 = indices.zipWithIndex.map{case (vec,d) => vec((p / prods(d)) % Ps(d)) }
      withSubstScope(inds.zip(inds2):_*){ dupSubst(p) = subst }
    }

    def duplicateStage(s: Sym[Any], d: Def[Any], isResult: Boolean) {
      val outerSubst = subst

      (0 until P).foreach{p =>
        subst = dupSubst(p)
        val mirroredStage = self_mirror(s,d)
        subst += s -> mirroredStage
        if (isResult) out ::= mirroredStage.asInstanceOf[Exp[A]]
        dupSubst(p) = subst
      }

      subst = outerSubst
    }

    val origResult = getBlockResult(func)

    focusBlock(func){
      focusExactScope(func){ stms =>
        stms.foreach {
          case TP(s,d) if isControlNode(s) && P > 1 => Parallel { duplicateStage(s,d, s==origResult) }
          case TP(s,d) => duplicateStage(s,d, s==origResult)
        }
      }
    }
    (indices, out.reverse)
  }
  def unrollForeach(lhs: Exp[Any], rhs: Pipe_foreach) = {
    debug(s"Unrolling $lhs = $rhs")

    val Pipe_foreach(cchain, func, inds) = rhs
    val cc2 = f(cchain)
    var inds2: List[List[Sym[Index]]] = Nil
    val blk = reifyBlock {
      val (unrolledInds, _) = if (isOuterLoop(lhs)) unrollOuterMap(cc2, f(func), inds)
                              else                  unrollInnerMap(cc2, func, inds)
      inds2 :::= unrolledInds
    }
    val newPipe = reflectEffect(ParPipeForeach(cc2, blk, inds2), summarizeEffects(blk).star andAlso Simple())
    setProps(newPipe, getProps(lhs))
    newPipe
  }

  def unrollReduce[A:Manifest](inputs: List[Exp[A]], rFunc: Block[A], iFunc: Block[Index], ld: Block[A], st: Block[Unit], idx: Exp[Index], res: Exp[A], rV: (Sym[A],Sym[A])) = {
    def reduce(x: Exp[A], y: Exp[A]) = withSubstScope(rV._1 -> x, rV._2 -> y){ inlineBlock(rFunc) }
    val treeResult = reduceTree(inputs){(x,y) => reduce(x,y) }
    val newIdx = inlineBlock(iFunc)
    val accumLoad = withSubstScope(idx -> newIdx){ inlineBlock(ld) }
    val newRes = reduce(treeResult, accumLoad)
    withSubstScope(res -> newRes, idx -> newIdx){ inlineBlock(st) }
  }

  // Create a single block with map + reduce + load + reduce + store
  // Still have to keep acc separate to make sure load isn't lifted out of the block (e.g. for registers)
  def unrollPipeFold[T,C[T]](lhs: Exp[Any], rhs: Pipe_fold[T,C])(implicit ctx: SourceContext, mT: Manifest[T], mC: Manifest[C[T]]) = {
    debug(s"Unrolling $lhs = $rhs")

    val Pipe_fold(cchain,accum,foldAccum,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) = rhs
    val cc = f(cchain)
    val accum2 = f(accum)
    val func2 = f(func)
    var inds2: List[List[Sym[Index]]] = Nil

    val blk = if (isInnerLoop(lhs)) reifyBlock {
      val (unrolledInds, mapRes) = unrollInnerMap(cc, func2, inds)(mT)
      inds2 :::= unrolledInds
      unrollReduce(mapRes, rFunc, iFunc, ld, st, idx, res, rV)(mT)
    }
    else reifyBlock {
      val (unrolledInds, mapRes) = unrollOuterMap(cc, func2, inds)(mT)
      inds2 :::= unrolledInds
      Pipe {
        unrollReduce(mapRes, rFunc, iFunc, ld, st, idx, res, rV)(mT)
      }
    }
    val newPipe = reflectEffect(ParPipeReduce(cc, accum2, blk, inds2, acc)(ctx,mT,mC))
    setProps(newPipe, getProps(lhs))
    newPipe
  }

  def unrollAccumFold[T,C[T]](lhs: Exp[Any], rhs: Accum_fold[T,C])(implicit ctx: SourceContext, mT: Manifest[T], mC: Manifest[C[T]]) = {
    debug(s"Unrolling $lhs = $rhs")

    val Accum_fold(ccOuter,ccInner,accum,foldAccum,iFunc,func,ld1,ld2,rFunc,st,indsOuter,indsInner,idx,part,acc,res,rV) = rhs
    val ccO2 = f(ccOuter)
    val ccI2 = f(ccInner)
    val accum2 = f(accum)
    val func2 = f(func)
    var indsO2: List[List[Sym[Index]]] = Nil

    def reduce(x: Exp[T], y: Exp[T]) = withSubstScope(rV._1 -> x, rV._2 -> y){ inlineBlock(rFunc)(mT) }
    def unrollLoadReduce(inputs: List[Exp[C[T]]]) = {
      val newIdx = inlineBlock(iFunc) // Should be zero or always unused
      val mapReads = inputs.map{partial => withSubstScope(part -> partial, idx -> newIdx){inlineBlock(ld1)(mT) } }
      val accRead  = withSubstScope(idx -> newIdx){ inlineBlock(ld2)(mT) }
      val treeResult = reduceTree(mapReads){(x,y) => reduce(x,y)}
      val newRes = reduce(treeResult, accRead)
      withSubstScope(res -> newRes, idx -> newIdx){ inlineBlock(st) }
    }

    val blk = reifyBlock {
      val (unrolledInds, mapRes) = unrollOuterMap(ccO2, func2, indsOuter)
      indsO2 :::= unrolledInds

      if (isUnitCounterChain(ccI2)) Pipe { unrollLoadReduce(mapRes) }
      else {
        // Unroll the reduction loop
        val Ps = parsOf(ccI2)
        val P = Ps.reduce(_*_)
        val N = Ps.length
        val prods = List.tabulate(N){i => Ps.slice(i+1,N).reduce(_*_) }
        val indices = Ps.map{p => List.fill(p){ fresh[Index] } }

        val innerBlk = reifyBlock {
          (0 until P).foreach{p =>
            val inds2 = indices.zipWithIndex.map{case (vec,d) => vec((p / prods(d)) % Ps(d)) }
            withSubstScope(indsInner.zip(inds2):_*){ unrollLoadReduce(mapRes) }
          }
        }
        val innerPipe = reflectEffect(ParPipeForeach(ccI2, innerBlk, indices), summarizeEffects(innerBlk).star andAlso Simple())
        styleOf(innerPipe) = Fine
      }
    }
    val outerPipe = reflectEffect(ParPipeReduce(ccO2, accum2, blk, indsO2, acc), summarizeEffects(blk).star andAlso Simple() andAlso Write(List(accum.asInstanceOf[Sym[C[T]]])))

    // Not completely correct - reduction is now an explicit child
    setProps(outerPipe, getProps(outerPipe))
    outerPipe
  }

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = rhs match {
    case EatReflect(e: Pipe_foreach) => Some( unrollForeach(lhs, e) )
    case EatReflect(e: Pipe_fold[_,_]) => Some( unrollPipeFold(lhs, e)(e.ctx,e.mT,e.mC) )
    case EatReflect(e: Accum_fold[_,_]) => Some( unrollAccumFold(lhs, e)(e.ctx,e.mT,e.mC) )
    case _ => None
  }
}
