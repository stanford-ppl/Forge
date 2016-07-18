package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.transform.MultiPassTransformer
import scala.collection.mutable.HashMap

import dhdl.compiler._
import dhdl.compiler.ops._

trait UnrollingTransformExp extends PipeStageToolsExp with LoweredPipeOpsExp { this: DHDLExp => }

trait UnrollingTransformer extends MultiPassTransformer with PipeStageTools {
  val IR: UnrollingTransformExp with DHDLExp
  import IR.{infix_until => _, Array => _, assert => _, _}

  debugMode = true
  override val name = "Unrolling Transformer"

  /**
   * Helper class for unrolling
   * Tracks multiple substitution contexts in 'laneSubst' array
   **/
  case class Unroller(cchain: Exp[CounterChain], inds: List[Exp[Index]]) {
    val Ps = parsOf(cchain)
    val P = Ps.reduce(_*_)
    val N = Ps.length
    val prods = List.tabulate(N){i => Ps.slice(i+1,N).fold(1)(_*_) }
    val indices = Ps.map{p => List.fill(p){ fresh[Index] } }

    def length = P // TODO: better term for this

    // Substitution for each duplication "lane"
    val laneSubst = Array.tabulate(P){p =>
      val inds2 = indices.zipWithIndex.map{case (vec,d) => vec((p / prods(d)) % Ps(d)) }
      withSubstScope(inds.zip(inds2):_*){ subst }
    }

    def map[A](block: Int => A): List[A] = {
      val outerSubst = subst
      val out = (0 until P).map{p =>
        subst = laneSubst(p)
        val result = block(p)
        laneSubst(p) = subst
        result
      }
      subst = outerSubst
      out.toList
    }
    def foreach(block: Int => Unit) { map(block) }

    def vectorize[T:Manifest](block: Int => Exp[T]): Exp[Vector[T]] = vector_create_from_list(map(block))

    // --- Each unrolling rule should do at least one of three things:
    // 1. Split a given vector as the substitution for the single original symbol
    def duplicate(s: Sym[Any], d: Def[Any]): List[Exp[Any]] = map{p =>
      val s2 = self_clone(s,d)
      subst += s -> s2
      s2
    }
    // 2. Make later stages depend on the given substitution across all lanes
    // NOTE: This assumes that the node has no meaningful return value (i.e. all are Pipeline or Unit)
    // Bad things can happen here if you're not careful!
    def split[T:Manifest](orig: Sym[Any], vec: Exp[Vector[T]]): List[Exp[T]] = map{p =>
      val element = vec(p)
      subst += orig -> element
      element
    }
    // 3. Create an unrolled clone of symbol s for each lane
    def unify(orig: Exp[Any], unrolled: Exp[Any]): List[Exp[Any]] = {
      foreach{p => subst += orig -> unrolled}
      List(unrolled)
    }

    // If this is a memory that is being unrolled in this scope, the symbol will be the un-mirrored version prior to duplication
    // If it is not being unrolled in this scope, the symbol will be the already mirrored version
    def isUnrolled(e: Exp[Any]) = {
      val inScope = laneSubst.exists(_ contains e)
      assert(!inScope || laneSubst.forall(_ contains e), s"Symbol $e only exists in some duplicated lanes!")
      inScope
    }
  }

  /**
   * Create index bound checks
   * NOTE: Only can be used within reify scope
   **/
  def boundChecks2D(cchain: Exp[CounterChain], inds: List[List[Exp[Index]]]): List[List[Exp[Bit]]] = {
    val ccMax = ccMaxes(cchain)
    inds.zip(ccMax).map{case (ind,max) => ind.map{i => i < max}}
  }
  def boundChecks(cchain: Exp[CounterChain], inds: List[List[Exp[Index]]]): List[Exp[Bit]] = boundChecks2D(cchain,inds).flatten

  /**
   * Create duplicates of the given node or special case, vectorized version
   * NOTE: Only can be used within reify scope
   **/
  // FIXME: Assumes FIFO and BRAM are outside of this unrolling scope!
  def unroll[T](s: Sym[T], d: Def[T], lanes: Unroller)(implicit ctx: SourceContext): List[Exp[Any]] = d match {
    // Account for the edge case with FIFO writing
    case EatReflect(e@Push_fifo(fifo, value, en)) if !lanes.isUnrolled(fifo) =>
      val values  = lanes.vectorize{p => f(value)}
      val valids  = boundChecks(lanes.cchain, lanes.indices)
      val enables = lanes.vectorize{p => f(en) && valids(p) }
      val parPush = par_push_fifo(f(fifo), values, enables, true)(e._mT,e.__pos)
      lanes.unify(s, parPush)

    case EatReflect(e@Pop_fifo(fifo)) if !lanes.isUnrolled(fifo) =>
      val parPop = par_pop_fifo(f(fifo), lanes.length)(e._mT,e.__pos)
      dimsOf(parPop) = List(lanes.length.as[Index])
      lanes.split(s, parPop)

    case EatReflect(e: Cam_load[_,_]) =>
      if (lanes.length > 1) stageError("Cannot parallelize CAM operations")(mpos(s.pos))
      lanes.duplicate(s, d)
    case EatReflect(e: Cam_store[_,_]) =>
      if (lanes.length > 1) stageError("Cannot parallelize CAM operations")(mpos(s.pos))
      lanes.duplicate(s, d)

    case EatReflect(e@Bram_store(bram,addr,value)) if !lanes.isUnrolled(bram) =>
      val values = lanes.vectorize{p => f(value)}
      val addrs  = lanes.vectorize{p => f(addr)}
      val parStore = par_bram_store(f(bram), addrs, values)(e._mT, e.__pos)
      lanes.unify(s, parStore)

    case EatReflect(e@Bram_load(bram,addr)) if !lanes.isUnrolled(bram) =>
      val addrs = lanes.vectorize{p => f(addr)}
      val parLoad = par_bram_load(f(bram), addrs)(e._mT, e.__pos)
      dimsOf(parLoad) = List(lanes.length.as[Index])
      lanes.split(s, parLoad)

    case d if isControlNode(s) && lanes.length > 1 =>
      val parBlk = reifyBlock { lanes.duplicate(s, d); () }
      val parStage = reflectEffect(Pipe_parallel(parBlk), summarizeEffects(parBlk) andAlso Simple())
      styleOf(parStage) = ForkJoin
      lanes.unify(s, parStage)

    case _ => lanes.duplicate(s, d)
  }

  /*
    Unrolls purely independent loop iterations
    NOTE: The func block should already have been mirrored to update dependencies prior to unrolling
  */
  def unrollMap[A:Manifest](func: Block[A], lanes: Unroller): List[Exp[A]] = {
    val origResult = getBlockResult(func)

    focusBlock(func){
      focusExactScope(func){ stms =>
        stms.foreach { case TP(s,d) => unroll(s, d, lanes)(mpos(s.pos)) }
      }
    }
    // Get the list of duplicates for the original result of this block
    lanes.map{p => f(origResult).asInstanceOf[Exp[A]] }
  }
  def unrollForeach(lhs: Exp[Any], rhs: Pipe_foreach) = {
    val Pipe_foreach(cchain, func, inds) = rhs
    scrubSym(lhs.asInstanceOf[Sym[Any]])

    val lanes = Unroller(cchain, inds)
    val blk = reifyBlock { unrollMap(func, lanes); () }
    val inds2 = lanes.indices

    val newPipe = reflectEffect(ParPipeForeach(cchain, blk, inds2), summarizeEffects(blk).star andAlso Simple())
    setProps(newPipe, getProps(lhs))
    newPipe
  }

  // TODO: General (more expensive) case for when no zero is given
  def unrollReduce[A:Manifest:Num](inputs: List[Exp[A]], valids: List[Exp[Bit]], zero: Option[Exp[A]], rFunc: Block[A], newIdx: Exp[Index], ld: Block[A], st: Block[Unit], idx: Exp[Index], res: Exp[A], rV: (Sym[A],Sym[A])) = {
    def reduce(x: Exp[A], y: Exp[A]) = withSubstScope(rV._1 -> x, rV._2 -> y){ inlineBlock(rFunc) }

    val validInputs = if (zero.isDefined) {
      inputs.zip(valids).map{case (res,v) => mux(v, res, zero.get) }
    }
    else {
      stageError("Reduction without explicit zero is not yet supported")
    }

    val treeResult = reduceTree(inputs){(x,y) => reduce(x,y) }
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
    val Pipe_fold(cchain,accum,zero,foldAccum,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) = rhs
    scrubSym(lhs.asInstanceOf[Sym[Any]])

    val lanes = Unroller(cchain, inds)
    val inds2 = lanes.indices

    val blk = reifyBlock {
      val mapResults = unrollMap(func, lanes)(mT)
      val valids = boundChecks(cchain, inds2)

      PipeIf(isOuterLoop(lhs)){
        val newIdx = inlineBlock(iFunc)
        unrollReduce(mapResults, valids, zero, rFunc, newIdx, ld, st, idx, res, rV)(mT,numT)
      }
    }
    val newPipe = reflectEffect(ParPipeReduce(cchain, accum, blk, rFunc, inds2, acc, rV)(ctx,mT,mC))

    val Def(d) = newPipe
    debug(s"$newPipe = $d")

    setProps(newPipe, getProps(lhs))
    newPipe
  }

  def unrollAccumFold[T,C[T]](lhs: Exp[Any], rhs: Accum_fold[T,C])(implicit ctx: SourceContext, numT: Num[T], mT: Manifest[T], mC: Manifest[C[T]]) = {
    val Accum_fold(ccMap,ccRed,accum,zero,foldAccum,iFunc,func,ld1,ld2,rFunc,st,indsMap,indsRed,idx,part,acc,res,rV) = rhs
    scrubSym(lhs.asInstanceOf[Sym[Any]])

    def reduce(x: Exp[T], y: Exp[T]) = withSubstScope(rV._1 -> x, rV._2 -> y){ inlineBlock(rFunc)(mT) }

    val mapLanes = Unroller(ccMap, indsMap)
    val indsMap2 = mapLanes.indices

    val blk = reifyBlock {
      val mapResults = unrollMap(func, mapLanes)
      val validsMap = boundChecks(ccMap, indsMap2)

      if (isUnitCounterChain(ccRed)) {
        withSubstScope(acc -> accum){
          Pipe {
            val newIdx = inlineBlock(iFunc)
            val loads = mapResults.map{mem => withSubstScope(part -> mem, idx -> newIdx){inlineBlock(ld1)(mT)} }
            unrollReduce(loads, validsMap, zero, rFunc, newIdx, ld2, st, idx, res, rV)(mT,numT)
          }
        }
      }
      else {
        val reduceLanes = Unroller(ccRed, indsRed)
        val indsRed2 = reduceLanes.indices

        val innerBlk = reifyBlock {
          val validsReduce = boundChecks2D(ccRed, indsRed2)

          reduceLanes.foreach{mem =>
            val newIdx = inlineBlock(iFunc)                // Inline index calculation for each parallel lane
            subst += idx -> newIdx                         // Save address substitution rule for this lane
          }
          val loads = mapResults.map{mem =>                // Calculate list of loaded results for each memory
            reduceLanes.foreach{p => subst += part -> mem} // Set partial result to be this memory
            unrollMap(ld1, reduceLanes)(mT)                // Unroll the load of the partial result
          }
          val results = reduceLanes.map{p =>
            val loadsT  = loads.map(_.apply(p))                               // Pth "column" of loads
            val validsLane = validsReduce.map{valids => valids(p) }           // Valids for just this reduce lane
            val valids = validsMap.zip(validsLane).map{case (a,b) => a && b}  // Valid map index & valid reduce index

            val validLoads = if (zero.isDefined) {
              loadsT.zip(valids).map{case (res,v) => mux(v,res,zero.get) }
            }
            else {
              stageError("Reduction without explicit zero is not yet supported")
            }
            val accRead = inlineBlock(ld2)(mT)
            val treeResult = reduceTree(validLoads){(x,y) => reduce(x,y) }
            val newRes = reduce(treeResult, accRead)
            subst += res -> newRes
          }
          unrollMap(st, reduceLanes)    // Parallel store. Already have substitutions for idx and res
          ()
        }
        val innerPipe = reflectEffect(ParPipeReduce(ccRed, accum, innerBlk, rFunc, indsRed2, acc, rV), summarizeEffects(innerBlk).star andAlso Simple() andAlso Write(List(accum.asInstanceOf[Sym[C[T]]])) )
        styleOf(innerPipe) = InnerPipe
      }
    }
    val newPipe = reflectEffect(ParPipeForeach(ccMap, blk, indsMap2), summarizeEffects(blk).star andAlso Simple() )

    val Def(d) = newPipe
    debug(s"$newPipe = $d")

    // Not completely correct - reduction is now an explicit child
    setProps(newPipe, getProps(lhs))
    newPipe
  }

  // Similar to self_mirror, but also duplicates bound vars
  def self_clone[A](sym: Sym[A], rhs : Def[A]): Exp[A] = {
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

  // Mirrors first prior to attempting to transform
  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = self_mirror(lhs, rhs) match {
    case lhs2@Deff(e: Pipe_foreach) => Some( unrollForeach(lhs2, e) )
    case lhs2@Deff(e: Pipe_fold[_,_]) => Some( unrollPipeFold(lhs2, e)(e.ctx,e.numT,e.mT,e.mC) )
    case lhs2@Deff(e: Accum_fold[_,_]) => Some( unrollAccumFold(lhs2, e)(e.ctx,e.numT,e.mT,e.mC) )
    case lhs2 => Some(lhs2) // Just use mirrored symbol by default
  }
}
