package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.transform.MultiPassTransformer
import scala.collection.mutable.HashMap

import dhdl.compiler._
import dhdl.compiler.ops._

trait UnrollingTransformExp extends ReductionAnalysisExp with LoweredPipeOpsExp with MemoryAnalysisExp {
  this: DHDLExp =>

  case class UnrolledResult(isIt: Boolean) extends Metadata
  object isReduceResult {
    def update(e: Exp[Any], isIt: Boolean) { setMetadata(e, UnrolledResult(isIt)) }
    def apply(e: Exp[Any]) = meta[UnrolledResult](e).map(_.isIt).getOrElse(false)
  }

  case class ReduceStarter(isIt: Boolean) extends Metadata
  object isReduceStarter {
    def update(e: Exp[Any], isIt: Boolean) { setMetadata(e, ReduceStarter(isIt)) }
    def apply(e: Exp[Any]) = meta[UnrolledResult](e).map(_.isIt).getOrElse(false)
  }

}

trait UnrollingTransformer extends MultiPassTransformer {
  val IR: UnrollingTransformExp with DHDLExp
  import IR.{infix_until => _, Array => _, assert => _, _}

  override val name = "Unrolling Transformer"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose


  var cloneFuncs: List[Exp[Any] => Unit] = Nil
  def duringClone[T](func: Exp[Any] => Unit)(blk: => T): T = {
    val prevCloneFuncs = cloneFuncs
    cloneFuncs ::= func
    debug("Adding upon-clone function!")
    val result = blk
    cloneFuncs = prevCloneFuncs
    result
  }
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

    def parAddr(p: Int) = List.tabulate(N){d => (p / prods(d)) % Ps(d) }

    // Substitution for each duplication "lane"
    val laneSubst = Array.tabulate(P){p =>
      val inds2 = indices.zip(parAddr(p)).map{case (vec, i) => vec(i) }
      withSubstScope(inds.zip(inds2):_*){ subst }
    }

    def map[A](block: Int => A): List[A] = List.tabulate(P){p =>
      withSubstRules(laneSubst(p)) {
        val result = block(p)
        laneSubst(p) = subst
        result
      }
    }

    def foreach(block: Int => Unit) { map(block) }

    def vectorize[T:Manifest](block: Int => Exp[T]): Exp[Vector[T]] = vector_create_from_list(map(block))

    // --- Each unrolling rule should do at least one of three things:
    // 1. Split a given vector as the substitution for the single original symbol
    def duplicate(s: Sym[Any], d: Def[Any]): List[Exp[Any]] = map{p =>
      val s2 = self_clone(s,d)
      register(s -> s2)
      s2
    }
    // 2. Make later stages depend on the given substitution across all lanes
    // NOTE: This assumes that the node has no meaningful return value (i.e. all are Pipeline or Unit)
    // Bad things can happen here if you're not careful!
    def split[T:Manifest](orig: Sym[Any], vec: Exp[Vector[T]]): List[Exp[T]] = map{p =>
      val element = vec_apply[T](vec, p)
      register(orig -> element)
      element
    }
    // 3. Create an unrolled clone of symbol s for each lane
    def unify(orig: Exp[Any], unrolled: Exp[Any]): List[Exp[Any]] = {
      foreach{p => register(orig -> unrolled) }
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
  def unroll[T](s: Sym[T], d: Def[T], lanes: Unroller)(implicit ctx: SourceContext): List[Exp[Any]] = d match {
    // Account for the edge case with FIFO writing
    case EatReflect(e@Push_fifo(fifo, value, en)) if !lanes.isUnrolled(fifo) =>
      debug(s"Unrolling $s = $d")
      val values  = lanes.vectorize{p => f(value)}
      val valids  = boundChecks(lanes.cchain, lanes.indices)
      val enables = lanes.vectorize{p => f(en) && valids(p) }
      val parPush = par_push_fifo(f(fifo), values, enables, true)(e._mT,e.__pos)
      if (SpatialConfig.genCGRA) {
        setProps(parPush, mirror(getProps(s), f.asInstanceOf[Transformer]))
      }
      cloneFuncs.foreach{func => func(parPush) }
      lanes.unify(s, parPush)

    case EatReflect(e@Pop_fifo(fifo)) if !lanes.isUnrolled(fifo) =>
      debug(s"Unrolling $s = $d")
      val parPop = par_pop_fifo(f(fifo), lanes.length)(e._mT,e.__pos)
      dimsOf(parPop) = List(lanes.length.as[Index])
      lenOf(parPop) = lanes.length
      instanceIndexOf(parPop, f(fifo)) = instanceIndexOf(s, fifo)
      if (SpatialConfig.genCGRA) {
        setProps(parPop, mirror(getProps(s), f.asInstanceOf[Transformer]))
      }
      cloneFuncs.foreach{func => func(parPop) }
      lanes.split(s, parPop)(e._mT)

    case EatReflect(e: Cam_load[_,_]) =>
      if (lanes.length > 1) stageError("Cannot parallelize CAM operations")(mpos(s.pos))
      lanes.duplicate(s, d)
    case EatReflect(e: Cam_store[_,_]) =>
      if (lanes.length > 1) stageError("Cannot parallelize CAM operations")(mpos(s.pos))
      lanes.duplicate(s, d)

    case EatReflect(e@Bram_store(bram,addr,value)) if !lanes.isUnrolled(bram) =>
      debug(s"Unrolling $s = $d")
      getProps(s).foreach{m => debug("  " + makeString(m)) }

      val values = lanes.vectorize{p => f(value)}
      val addrs  = lanes.vectorize{p => f(addr)}
      val parStore = par_bram_store(f(bram), addrs, values)(e._mT, e.__pos)
      parIndicesOf(parStore) = lanes.map{i => accessIndicesOf(s).map(f(_)) }
      if (SpatialConfig.genCGRA) {
        setProps(parStore, mirror(getProps(s), f.asInstanceOf[Transformer]))
      }
      cloneFuncs.foreach{func => func(parStore) }
      lanes.unify(s, parStore)

    case EatReflect(e@Bram_load(bram,addr)) if !lanes.isUnrolled(bram) =>
      debug(s"Unrolling $s = $d")
      getProps(s).foreach{m => debug("  " + makeString(m)) }

      val addrs = lanes.vectorize{p => f(addr)}
      val parLoad = par_bram_load(f(bram), addrs)(e._mT, e.__pos)
      dimsOf(parLoad) = List(lanes.length.as[Index])
      lenOf(parLoad) = lanes.length
      parIndicesOf(parLoad) = lanes.map{i => accessIndicesOf(s).map(f(_)) }
      instanceIndexOf(parLoad, f(bram)) = instanceIndexOf(s, bram)
      if (SpatialConfig.genCGRA) {
        setProps(parLoad, mirror(getProps(s), f.asInstanceOf[Transformer]))
      }
      cloneFuncs.foreach{func => func(parLoad) }
      lanes.split(s, parLoad)(e._mT)

    // FIXME: Shouldn't be necessary to have duplication rules + metadata propagation
    // for these nodes explicitly. Maybe have two-step mirroring for metadata?
    case EatReflect(e@Bram_store(bram,addr,value)) =>
      debug(s"Duplicated bram store $s = $d")
      getProps(s).foreach{m => debug("  " + makeString(m)) }

      val stores = lanes.duplicate(s, d)
      lanes.foreach{i => parIndicesOf(f(s)) = List(accessIndicesOf(s).map(f(_))) }
      stores

    case EatReflect(e@Bram_load(bram,addr)) =>
      debug(s"Duplicated bram load $s = $d")
      getProps(s).foreach{m => debug("  " + makeString(m)) }

      val loads = lanes.duplicate(s, d)
      loads.foreach{i => parIndicesOf(f(s)) = List(accessIndicesOf(s).map(f(_))) }
      loads

    case d if isControlNode(s) && lanes.length > 1 =>
      val parBlk = reifyBlock { lanes.duplicate(s, d); () }
      val parStage = reflectEffect(Pipe_parallel(parBlk), summarizeEffects(parBlk) andAlso Simple())
      styleOf(parStage) = ForkJoin
      lanes.unify(s, parStage)

    case _ =>
      debug(s"Duplicating $s = $d")
      lanes.duplicate(s, d)
  }

  /*
    Unrolls purely independent loop iterations
    NOTE: The func block should already have been mirrored to update dependencies prior to unrolling
  */
  def unrollMap[A:Manifest](func: Block[A], lanes: Unroller): List[Exp[A]] = {
    val origResult = getBlockResult(func)

    focusBlock(func){
      focusExactScope(func){ stms =>
        stms.foreach { case TP(s,d) =>
          unroll(s, d, lanes)(mpos(s.pos))
        }
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
  def unrollReduce[A:Manifest:Num](node: Exp[Any], accum: Exp[Any], inputs: List[Exp[A]], valids: List[Exp[Bit]], zero: Option[Exp[A]], rFunc: Block[A], newIdx: Exp[Index], ld: Block[A], st: Block[Unit], idx: Exp[Index], res: Exp[A], rV: (Sym[A],Sym[A])) = {
    def reduce(x: Exp[A], y: Exp[A]) = withSubstScope(rV._1 -> x, rV._2 -> y){ inlineBlock(rFunc) }

    val validInputs = if (zero.isDefined) {
      inputs.zip(valids).map{case (res,v) => mux(v, res, zero.get) }
    }
    else {
      stageError("Reduction without explicit zero is not yet supported")
    }

    val treeResult = reduceTree(inputs){(x,y) => reduce(x,y) }
    val accumLoad = duringClone{e =>
      instanceIndexOf(e,accum) = instanceIndexOf(node,accum)
      if (SpatialConfig.genCGRA) reduceType(e) = None
    }{
      withSubstScope(idx -> newIdx){ inlineBlock(ld) }
    }

    val newRes = reduce(treeResult, accumLoad)
    isReduceResult(newRes) = true
    isReduceStarter(accumLoad) = true
    Console.println(s"isStarter $accumLoad ? ${isReduceStarter(accumLoad)}. idx ${idx}")


    duringClone{e => if (SpatialConfig.genCGRA) reduceType(e) = None }{
      withSubstScope(res -> newRes, idx -> newIdx){ inlineBlock(st) }
    }
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
        unrollReduce(lhs, accum, mapResults, valids, zero, rFunc, newIdx, ld, st, idx, res, rV)(mT,numT)
      }
    }
    val newPipe = reflectEffect(ParPipeReduce(cchain, accum, blk, rFunc, inds2, acc, rV)(ctx,mT,mC))
    isInnerAccum(accum) = true
    isInnerAccum(acc) = true
    aliasOf(acc) = accum
    debug(s"Setting alias of $acc to $accum")

    val Def(d) = newPipe
    debug(s"$newPipe = $d")

    setProps(newPipe, getProps(lhs))
    newPipe
  }

  def unrollAccumFold[T,C[T]](lhs: Exp[Any], rhs: Accum_fold[T,C])(implicit ctx: SourceContext, numT: Num[T], mT: Manifest[T], mC: Manifest[C[T]]) = {
    val Accum_fold(ccMap,ccRed,accum,zero,foldAccum,iFunc,func,ld1,ld2,rFunc,st,indsMap,indsRed,idx,part,acc,res,rV) = rhs
    scrubSym(lhs.asInstanceOf[Sym[Any]])

    val partial = getBlockResult(func)

    debug(s"$lhs = $rhs")
    debug(s"partial = $partial")
    getProps(lhs).foreach{m => debug("  " + makeString(m)) }

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
            val loads = mapResults.map{mem =>
              duringClone{e =>
                instanceIndexOf(e, mem) = instanceIndexOf(lhs, partial)
                if (SpatialConfig.genCGRA) reduceType(e) = None
              }{
                withSubstScope(part -> mem, idx -> newIdx){inlineBlock(ld1)(mT)}
              }
            }
            // Set the final unit pipe reduction as non-reduction stages (as they can't be put in a SIMD tree currently)
            duringClone{e => if (SpatialConfig.genCGRA) reduceType(e) = None}{
              unrollReduce(lhs, accum, loads, validsMap, zero, rFunc, newIdx, ld2, st, idx, res, rV)(mT,numT)
            }
          }
        }
      }
      else {
        debug(s"Unrolling block reduce $lhs")
        val reduceLanes = Unroller(ccRed, indsRed)
        val indsRed2 = reduceLanes.indices

        debug(s"  Reduction indices: $indsRed2")

        val innerBlk = reifyBlock {
          val validsReduce = boundChecks2D(ccRed, indsRed2)
          debug(s"  Reduction valids: $validsReduce")

          reduceLanes.foreach{mem =>
            val newIdx = inlineBlock(iFunc)                // Inline index calculation for each parallel lane
            register(idx -> newIdx)                        // Save address substitution rule for this lane
          }
          val loads = mapResults.map{mem =>                  // Calculate list of loaded results for each memory
              reduceLanes.foreach{p => register(part -> mem) } // Set partial result to be this memory
              duringClone{e =>
                instanceIndexOf(e, mem) = instanceIndexOf(lhs, partial)
                if (SpatialConfig.genCGRA) reduceType(e) = None
              }{
                unrollMap(ld1, reduceLanes)(mT)              // Unroll the load of the partial result
              }
          }
          val results = reduceLanes.map{p =>
            val addrs = reduceLanes.parAddr(p)
            debug(s"  Creating reduction lane #$p")
            debug(s"    Addrs: $addrs")
            val loadsT  = loads.map(_.apply(p))                                   // Pth "column" of loads
            val validsLane = validsReduce.zip(addrs).map{case (vec,i) => vec(i) } // Valids for just this reduce lane
            val valids = validsMap.zip(validsLane).map{case (a,b) => a && b}      // Valid map index & valid reduce index

            val validLoads = if (zero.isDefined) {
              loadsT.zip(valids).map{case (res,v) =>
                /*val defaultValue = if (isVector(res.tp)) {
                  vector_create_from_list(List.fill(dimsOf(res).head){zero.get})
                }
                else zero.get*/
                val defaultValue = zero.get
                mux(v,res,defaultValue) // TODO: Types...
              }
            }
            else {
              stageError("Reduction without explicit zero is not yet supported")
            }
            val accRead = duringClone{e =>
              instanceIndexOf(e,accum) = instanceIndexOf(lhs,accum)
              if (SpatialConfig.genCGRA) reduceType(e) = None
            }{
              inlineBlock(ld2)(mT)
            }
            val newRes = duringClone{e => if (SpatialConfig.genCGRA) reduceType(e) = None}{
              val treeResult = reduceTree(validLoads){(x,y) => reduce(x,y) }
              reduce(treeResult, accRead)
            }
            register(res -> newRes)
          }
          duringClone{e => if (SpatialConfig.genCGRA) reduceType(e) = None}{
            unrollMap(st, reduceLanes)    // Parallel store. Already have substitutions for idx and res
          }
          ()
        }
        val innerPipe = reflectEffect(ParPipeReduce(ccRed, accum, innerBlk, rFunc, indsRed2, acc, rV), summarizeEffects(innerBlk).star andAlso Simple() andAlso Write(List(accum.asInstanceOf[Sym[C[T]]])) )
        aliasOf(acc) = accum
        debug(s"Setting alias of $acc to $accum")
        isInnerAccum(accum) = true
        isInnerAccum(acc) = true
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

  override def self_mirror[A](sym: Sym[A], rhs: Def[A]): Exp[A] = self_clone(sym,rhs)

  // Similar to self_mirror, but also duplicates bound vars
  // Mirror metadata on the fly
  def self_clone[A](sym: Sym[A], rhs : Def[A]): Exp[A] = {
    val sym2 = clone(sym, rhs)(mtype(sym.tp), mpos(sym.pos))
    setProps(sym2, mirror(getProps(sym), f.asInstanceOf[Transformer]))
    cloneFuncs.foreach{func => func(sym2)}
    sym2
  }

  def cloneInds[I:Manifest](inds: List[List[Sym[I]]]) = inds.map{is => is.map{i => fresh[I] }}

  def clone[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit pos: SourceContext): Exp[A] = (rhs match {
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
      aliasOf(acc2) = a2
      debug(s"Setting alias of $acc2 to $a2")
      reflectMirrored(Reflect(ParPipeReduce(f(cc),a2,b2,rF2,i2,acc2,rV2)(e.ctx,e.mT,e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case EatReflect(e@Bram_store(bram,addr,value)) =>
      val store = mirror(rhs, f.asInstanceOf[Transformer])(mtype(manifest[A]), pos)
      parIndicesOf(store) = List(accessIndicesOf(lhs).map(f(_)))
      store

    case EatReflect(e@Bram_load(bram,addr)) =>
      val load = mirror(rhs, f.asInstanceOf[Transformer])(mtype(manifest[A]), pos)
      parIndicesOf(load) = List(accessIndicesOf(lhs).map(f(_)))
      load

    case _ => mirror(rhs, f.asInstanceOf[Transformer])(mtype(manifest[A]), pos)
  }).asInstanceOf[Exp[A]]

  def shouldUnroll(lhs: Exp[Any]) = !SpatialConfig.genCGRA || !isInnerControl(lhs)

  // Mirrors first prior to attempting to transform -- need to scrub mirrored symbols if they are not used
  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = self_mirror(lhs, rhs) match {
    case lhs2@Deff(e: Pipe_foreach) => Some( unrollForeach(lhs2, e) )
    case lhs2@Deff(e: Pipe_fold[_,_]) => Some( unrollPipeFold(lhs2, e)(e.ctx,e.numT,e.mT,e.mC) )
    case lhs2@Deff(e: Accum_fold[_,_]) => Some( unrollAccumFold(lhs2, e)(e.ctx,e.numT,e.mT,e.mC) )
    case lhs2 => Some(lhs2) // Just use mirrored symbol by default
  }
}
