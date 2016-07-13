package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.transform.MultiPassTransformer
import scala.collection.mutable.HashMap

import dhdl.compiler._
import dhdl.compiler.ops._

trait UnrollingTransformExp extends PipeStageToolsExp with LoweredPipeOpsExp { this: DHDLExp => }

trait UnrollingTransformer extends MultiPassTransformer with PipeStageTools {
  val IR: UnrollingTransformExp with DHDLExp
  import IR.{infix_until => _, _}

  debugMode = false
  override val name = "Unrolling Transformer"

  /**
   * Check edge case
   * NOTE: Use within a reify scope (creates comparison nodes)
   **/
  def valids(cchain: Exp[CounterChain], inds: List[List[Exp[Index]]]) = {
    val ccMax = ccMaxes(cchain)
    val maxes = inds.zip(ccMax).map{case (ind,max) => List.fill(ind.length)(max) }

    inds.flatten.zip(maxes.flatten).map{case (i,max) => i < max }
  }

  /*
    Unrolls purely independent loop iterations
    NOTE: The func block should already have been mirrored to update dependencies prior to unrolling
  */
  def unrollMap[A:Manifest](cchain: Exp[CounterChain], func: Block[A], inds: List[Exp[Index]]) = {
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
        val mirroredStage = clone(s,d)
        subst += s -> mirroredStage
        if (isResult) out ::= mirroredStage.asInstanceOf[Exp[A]]
        dupSubst(p) = subst
      }
      subst = outerSubst
    }

    // Make later stages depend on the given substitution across all duplicates
    // NOTE: This assumes that the node has no meaningful return value (i.e. all are Pipeline or Unit)
    // Bad things can happen here if you're not careful!
    def unifySubst(s: Sym[Any], s2: Exp[Any]) {
      (0 until P).foreach{p =>
        var sub = dupSubst(p)
        sub += s -> s2
        dupSubst(p) = sub
      }
    }

    def groupDuplicates[T:Manifest](orig: Exp[T]): Exp[Vector[T]] = {
      val values = (0 until P).map{p => dupSubst(p)(orig).asInstanceOf[Exp[T]] }.toList
      vector_create_from_list(values)
    }
    def splitVector[T:Manifest](orig: Sym[Any], vec: Exp[Vector[T]], isResult: Boolean) = {
      val outerSubst = subst

      (0 until P).foreach{p =>
        subst = dupSubst(p)
        val mirroredStage = vec(p)
        subst += orig -> mirroredStage
        if (isResult) out ::= mirroredStage.asInstanceOf[Exp[A]]
        dupSubst(p) = subst
      }
      subst = outerSubst
    }

    val origResult = getBlockResult(func)

    /** Create duplicates of the given node or special case, vectorized version **/
    def unroll[T](s: Sym[T], d: Def[T], isResult: Boolean)(implicit ctx: SourceContext) = d match {
      // Account for the edge case with FIFO writing
      case EatReflect(e@Push_fifo(fifo, value, en)) =>
        val values = groupDuplicates(value)
        val enable = (0 until P).map{p => dupSubst(p)(en).asInstanceOf[Exp[Bit]]}.toList
        val valid  = valids(cchain, indices)
        val validEnable = enable.zip(valid).map{case (en,v) => en && v}
        val en2 = vector_create_from_list(validEnable)
        val requiresShuffle = en2 match { case Deff(ConstBit(_)) => false case _ => true}
        val parPush = par_push_fifo(f(fifo), values, en2, requiresShuffle)(e._mT,e.__pos)
        unifySubst(s, parPush)

      case EatReflect(e@Pop_fifo(fifo)) =>
        val parPop = par_pop_fifo(f(fifo), P)(e._mT,e.__pos)
        dimsOf(parPop) = List(P.as[Index])
        splitVector(s, parPop, isResult)

      case EatReflect(e: Cam_load[_,_]) =>
        if (P > 1) stageError("Cannot parallelize CAM operations")(mpos(s.pos))
        duplicateStage(s, d, isResult)
      case EatReflect(e: Cam_store[_,_]) =>
        if (P > 1) stageError("Cannot parallelize CAM operations")(mpos(s.pos))
        duplicateStage(s, d, isResult)

      case EatReflect(e@Bram_store(bram,addr,value)) =>
        val values = groupDuplicates(value)
        val addrs  = groupDuplicates(addr)
        val parStore = par_bram_store(f(bram), addrs, values)(e._mT, e.__pos)
        unifySubst(s, parStore)

      case EatReflect(e@Bram_load(bram,addr)) =>
        val addrs = groupDuplicates(addr)
        val parLoad = par_bram_load(f(bram), addrs)(e._mT, e.__pos)
        dimsOf(parLoad) = List(P.as[Index])
        splitVector(s, parLoad, isResult)

      case d if isControlNode(s) && P > 1 =>
        val parBlk = reifyBlock { duplicateStage(s, d, isResult) }
        val parStage = reflectEffect(Pipe_parallel(parBlk), summarizeEffects(parBlk) andAlso Simple())
        styleOf(parStage) = ForkJoin
        unifySubst(s, parStage)

      case _ => duplicateStage(s, d, isResult)
    }

    focusBlock(func){
      focusExactScope(func){ stms =>
        stms.foreach { case TP(s,d) => unroll(s, d, s == origResult)(mpos(s.pos)) }
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
      val (unrolledInds, _) = unrollMap(cc2, f(func), inds)
      inds2 :::= unrolledInds
    }
    val newPipe = reflectEffect(ParPipeForeach(cc2, blk, inds2), summarizeEffects(blk).star andAlso Simple())

    val Def(d) = newPipe
    debug(s"$newPipe = $d")

    setProps(newPipe, getProps(lhs))
    newPipe
  }

  // TODO: General (more expensive) case for when no zero is given
  def unrollReduce[A:Manifest](inputs: List[Exp[A]], rFunc: Block[A], iFunc: Block[Index], ld: Block[A], st: Block[Unit], idx: Exp[Index], res: Exp[A], rV: (Sym[A],Sym[A])) = {
    def reduce(x: Exp[A], y: Exp[A]) = withSubstScope(rV._1 -> x, rV._2 -> y){ inlineBlock(rFunc) }
    val treeResult = reduceTree(inputs){(x,y) => reduce(x,y) }
    val newIdx = inlineBlock(iFunc)
    val accumLoad = withSubstScope(idx -> newIdx){ inlineBlock(ld) }
    val newRes = reduce(treeResult, accumLoad)
    withSubstScope(res -> newRes, idx -> newIdx){ inlineBlock(st) }
  }

  object PipeIf {
    def apply(cond: Boolean)(x: => Rep[Unit]) = if (cond) Pipe { x } else x
  }

  // Create a single block with map + reduce + load + reduce + store
  // Still have to keep acc separate to make sure load isn't lifted out of the block (e.g. for registers)
  def unrollPipeFold[T,C[T]](lhs: Exp[Any], rhs: Pipe_fold[T,C])(implicit ctx: SourceContext, numT: Num[T], mT: Manifest[T], mC: Manifest[C[T]]) = {
    debug(s"Unrolling $lhs = $rhs")

    val Pipe_fold(cchain,accum,zero,foldAccum,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) = rhs
    val cc = f(cchain)
    val accum2 = f(accum)
    val func2 = f(func)
    var inds2: List[List[Sym[Index]]] = Nil

    val blk = reifyBlock {
      val (unrolledInds, mapRes) = unrollMap(cc, func2, inds)(mT)
      inds2 :::= unrolledInds

      val validMapRes = if (zero.isDefined) {
        mapRes.zip(valids(cc,inds2)).map{case (res,v) => mux(v,res,zero.get) }
      }
      else {
        stageError("Reduction without zero is not yet supported")
        mapRes
      }

      PipeIf(isOuterLoop(lhs)){ unrollReduce(validMapRes, rFunc, iFunc, ld, st, idx, res, rV)(mT) }
    }
    val newPipe = reflectEffect(ParPipeReduce(cc, accum2, blk, f(rFunc), inds2, acc, rV)(ctx,mT,mC))

    val Def(d) = newPipe
    debug(s"$newPipe = $d")

    setProps(newPipe, getProps(lhs))
    newPipe
  }

  def unrollAccumFold[T,C[T]](lhs: Exp[Any], rhs: Accum_fold[T,C])(implicit ctx: SourceContext, numT: Num[T], mT: Manifest[T], mC: Manifest[C[T]]) = {
    debug(s"Unrolling $lhs = $rhs")

    val Accum_fold(ccOuter,ccInner,accum,zero,foldAccum,iFunc,func,ld1,ld2,rFunc,st,indsOuter,indsInner,idx,part,acc,res,rV) = rhs
    val ccO2 = f(ccOuter)
    val ccI2 = f(ccInner)
    val accum2 = f(accum)
    val func2 = f(func)
    var indsO2: List[List[Sym[Index]]] = Nil

    def reduce(x: Exp[T], y: Exp[T]) = withSubstScope(rV._1 -> x, rV._2 -> y){ inlineBlock(rFunc)(mT) }

    def unrollLoadReduce(inputs: List[Exp[C[T]]], cchain: Option[Exp[CounterChain]] = None, inds: Option[List[List[Exp[Index]]]] = None) = {
      val newIdx = inlineBlock(iFunc) // Should be zero or always unused
      val mapReads = inputs.map{partial => withSubstScope(part -> partial, idx -> newIdx){inlineBlock(ld1)(mT) } }

      val valid = if (cchain.isDefined && inds.isDefined) {
        valids(ccO2,indsO2).zip(valids(cchain.get,inds.get)).map{case(v1,v2) => v1 && v2}
      }
      else {
        valids(ccO2,indsO2)
      }

      val validMapReads = if (zero.isDefined) {
        mapReads.zip(valid).map{case (res,v) => mux(v, res, zero.get) }
      }
      else {
        stageError("Reduction without zero is not yet supported")
        mapReads
      }

      val accRead  = withSubstScope(idx -> newIdx){ inlineBlock(ld2)(mT) }
      val treeResult = reduceTree(validMapReads){(x,y) => reduce(x,y)}
      val newRes = reduce(treeResult, accRead)
      withSubstScope(res -> newRes, idx -> newIdx){ inlineBlock(st) }
    }

    val blk = reifyBlock {
      val (unrolledInds, mapRes) = unrollMap(ccO2, func2, indsOuter)
      indsO2 :::= unrolledInds

      if (isUnitCounterChain(ccI2)) withSubstScope(acc -> accum2){ Pipe { unrollLoadReduce(mapRes) } }
      else {
        // Unroll the reduction loop
        val Ps = parsOf(ccI2)
        val P = Ps.reduce(_*_)
        val N = Ps.length
        val prods = List.tabulate(N){i => Ps.slice(i+1,N).fold(1)(_*_) }
        val indices = Ps.map{p => List.fill(p){ fresh[Index] } }

        val innerBlk = reifyBlock {
          (0 until P).foreach{p =>
            val inds2 = indices.zipWithIndex.map{case (vec,d) => vec((p / prods(d)) % Ps(d)) }
            withSubstScope(indsInner.zip(inds2):_*){ unrollLoadReduce(mapRes, Some(ccI2), Some(indices)) }
          }
        }
        val innerPipe = reflectEffect(ParPipeReduce(ccI2, accum2, innerBlk, rFunc, indices, acc, rV), summarizeEffects(innerBlk).star andAlso Simple() andAlso Write(List(accum2.asInstanceOf[Sym[C[T]]])) )
        styleOf(innerPipe) = InnerPipe
      }
    }
    val newPipe = reflectEffect(ParPipeForeach(ccO2, blk, indsO2), summarizeEffects(blk).star andAlso Simple() )

    val Def(d) = newPipe
    debug(s"$newPipe = $d")

    // Not completely correct - reduction is now an explicit child
    setProps(newPipe, getProps(lhs))
    newPipe
  }

  // TODO: Move clone to IR?
  // Similar to self_mirror, but also duplicates bound vars
  def clone[A](sym: Sym[A], rhs : Def[A]): Exp[A] = {
    val sym2 = clone(rhs)(mtype(sym.tp), mpos(sym.pos))
    setProps(sym2, getProps(sym))
    sym2
  }

  def cloneInds[I:Manifest](inds: List[List[Sym[I]]]) = inds.map{is => is.map{i => fresh[I] }}

  def clone[A:Manifest](rhs: Def[A])(implicit pos: SourceContext): Exp[A] = (rhs match {
    case Reflect(e@ParPipeForeach(cc,b,i), u, es) =>
      val i2 = cloneInds(i)
      val b2 = withSubstScope(i.flatten.zip(i2.flatten):_*){ f(b) }
      reflectMirrored(Reflect(ParPipeForeach(f(cc), b2, i2)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case Reflect(e@ParPipeReduce(cc,a,b,rF,i,acc,rV), u, es) =>
      val a2 = f(a)
      val i2 = cloneInds(i)
      val acc2 = reflectMutableSym(fresh(List(e.ctx))(e.mC))
      val rV2 = (fresh(List(e.ctx))(e.mT), fresh(List(e.ctx))(e.mT))
      val b2 = withSubstScope( (i.flatten.zip(i2.flatten) ++ List(acc -> acc2)):_*) { f(b) }
      val rF2 = withSubstScope(rV._1 -> rV2._1, rV._2 -> rV2._2){ f(rF) }
      reflectMirrored(Reflect(ParPipeReduce(f(cc),a2,b2,rF2,i2,acc2,rV2)(e.ctx,e.mT,e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => mirror(rhs, f.asInstanceOf[Transformer])(mtype(manifest[A]), pos)
  }).asInstanceOf[Exp[A]]

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = rhs match {
    case EatReflect(e: Pipe_foreach) => Some( unrollForeach(lhs, e) )
    case EatReflect(e: Pipe_fold[_,_]) => Some( unrollPipeFold(lhs, e)(e.ctx,e.numT,e.mT,e.mC) )
    case EatReflect(e: Accum_fold[_,_]) => Some( unrollAccumFold(lhs, e)(e.ctx,e.numT,e.mT,e.mC) )
    case _ => None
  }
}
