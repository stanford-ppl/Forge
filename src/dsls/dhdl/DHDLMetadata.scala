package ppl.dsl.forge
package dsls
package dhdl

trait DHDLMetadata {
  this: DHDLDSL =>

  def importDHDLMetadata() = {
    val T = tpePar("T")

    val RegType     = lookupTpe("RegType", stage=compile)
    val ControlType = lookupTpe("ControlType", stage=compile)
    val Reg         = lookupTpe("Reg")
    val Pipeline    = lookupTpe("Pipeline")
    val Tile        = lookupTpe("Tile")
    val Range       = lookupTpe("Range")
    val Idx         = lookupAlias("Index")

    val DummyMem = metadata("DummyMem", "isDummy" -> SBoolean)
    val isDummy = metadata("isDummy")
    static (isDummy) ("update", Nil, (MAny, SBoolean) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, DummyMem($1)) }
    static (isDummy) ("apply", Nil, (MAny) :: SBoolean) implements
      composite ${ meta[DummyMem]($0).map(_.isDummy).getOrElse(false) }

    /**
      Metadata for determining which memory instance a reader should correspond to.
      Needed to preserve mapping after unrolling
    */
    val MemInstanceIndex = metadata("MemInstanceIndex", "idx" -> SInt)
    val instIdxOps = metadata("instanceIndexOf")
    static (instIdxOps) ("update", Nil, (MAny, SInt) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MemInstanceIndex($1)) }
    static (instIdxOps) ("get", Nil, (MAny) :: SOption(SInt)) implements
      composite ${ meta[MemInstanceIndex]($0).map(_.idx) }
    static (instIdxOps) ("apply", Nil, (MAny) :: SInt) implements
      composite ${ meta[MemInstanceIndex]($0).map(_.idx).get }

    /**
     * Statically determined length
     * Used for tracking lengths of Vectors and CounterChains.
     * User facing: No
     * Set: lenOf(Rep[Any]) = Int
     * Get: lenOf(Rep[Any]) // Returns Int. Error if undefined
     **/
    val MDims = metadata("MLength", "len" -> SInt)
    val lenOps = metadata("lenOf")
    internal.static (lenOps) ("update", Nil, (MAny, SInt) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MLength($1)) }
    internal.static (lenOps) ("apply", Nil, MAny :: SInt) implements composite ${ meta[MLength]($0).get.len }


    /**
     * Staged N-D memory dimensions
     * Used for tracking dimensions of OffChipMems, BRAM, FIFO, etc.
     * User facing: No
     * Set: dimsOf(Rep[Any]) = List[Rep[Index]]
     * Get: dimsOf(Rep[Any])  // Returns List[Rep[Index]]. Error if undefined
     * Get: sizeOf(Rep[Any])  // Returns product of dimensions. Error if undefined
     **/
    val MStagedDims = metadata("MStagedDims", "dims" -> SList(Idx))
    val dimOps = metadata("dimsOf")
    internal.static (dimOps) ("update", Nil, (MAny, SList(Idx)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MStagedDims($1)) }
    internal.static (dimOps) ("apply", Nil, MAny :: SList(Idx)) implements
      composite ${ meta[MStagedDims]($0).get.dims }

    internal (dimOps) ("sizeOf", T, T :: Idx) implements composite ${ productTree(dimsOf($0)) }


    /**
     * Accumulator flag
     * Used to track whether a local memory is an accumulator
     * User facing: No
     * Set: isAccum(Rep[Any]) = Boolean
     * Get: isAccum(Rep[Any])   // Returns Boolean. False if undefined
     **/
    val MAccum = metadata("MAccum", "isAccum" -> SBoolean)
    val accumOps = metadata("isAccum")
    internal.static (accumOps) ("update", Nil, (MAny, SBoolean) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MAccum($1)) }
    internal.static (accumOps) ("apply", Nil, MAny :: SBoolean) implements
      composite ${ meta[MAccum]($0).map(_.isAccum).getOrElse(false) }

    val MInnerAccum = metadata("MInnerAccum", "isInnerAccum" -> SBoolean)
    val innerAccumOps = metadata("isInnerAccum")
    internal.static (innerAccumOps) ("update", Nil, (MAny, SBoolean) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MInnerAccum($1)) }
    internal.static (innerAccumOps) ("apply", Nil, (MAny) :: SBoolean) implements
      composite ${ meta[MInnerAccum]($0).map(_.isInnerAccum).getOrElse(false) }

    /* Is inserted metapipe register */
    val MDelayReg = metadata("MDelayReg", "isDelay" -> SBoolean)
    val delayRegOps = metadata("isDelayReg")
    internal.static (delayRegOps) ("update", T, (T, SBoolean) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MDelayReg($1)) }
    internal.static (delayRegOps) ("apply", T, T :: SBoolean) implements
      composite ${ meta[MDelayReg]($0).map(_.isDelay).getOrElse(false) }


    /**
     * Register type
     * Used to track type of register ("Regular", "ArgumentIn", or "ArgumentOut")
     * User facing: No (but directly set depending on register constructor)
     * Set: regType(Rep[Any]) = RegType
     * Get: regType(Rep[Any])   // Returns RegType. Regular if undefined
     * Get: isArgIn(Rep[Any])   // Returns true if regType of symbol is ArgumentIn, false otherwise
     * Get: isArgOut(Rep[Any])  // Returns true if regType of symbol is ArgumentOut, false otherwise
     **/
    val MRegType = metadata("MRegType", "regType" -> RegType)
    val regTypeOps = metadata("regType")
    internal.static (regTypeOps) ("update", Nil, (MAny, RegType) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MRegType($1)) }
    internal.static (regTypeOps) ("apply", Nil, MAny :: RegType) implements
      composite ${ meta[MRegType]($0).map(_.regType).getOrElse(Regular) }

    internal (regTypeOps) ("isArgIn", Nil, MAny :: SBoolean) implements composite ${ regType($0) == ArgumentIn }
    internal (regTypeOps) ("isArgOut", Nil, MAny :: SBoolean) implements composite ${ regType($0) == ArgumentOut }


    /**
     * Register initial value
     * Used to track the reset value of registers
     * User facing: No (but directly set based on register constructor)
     * Set: resetValue(Rep[Reg[T]]) = Rep[T]
     * Get: resetValue(Rep[Reg[T]])   // Returns Rep[T]. Error if undefined.
     **/
    val MRegInit = metadata("MRegInit", "value" -> MAny)
    val regReset = metadata("resetValue")
    internal.static (regReset) ("update", T, (Reg(T), T) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MRegInit($1)) }
    internal.static (regReset) ("apply", T, Reg(T) :: T) implements
      composite ${ meta[MRegInit]($0).get.value.asInstanceOf[Rep[T]] }


    /**
     * Tile parallelization factor
     * Used to track the parallelization factor associated with tile loads/stores
     * User facing: No (but directly set based on par input when creating Tile)
     * Set: tilePar(Rep[Any]) = Rep[Int]
     * Get: tilePar(Rep[Any])   // Returns Rep[Int]. Error if undefined.
     **/
    val MTilePar = metadata("MTilePar", "par" -> MInt)
    val tileParOps = metadata("tilePar")
    internal.static (tileParOps) ("update", Nil, (MAny, MInt) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MTilePar($1)) }
    internal.static (tileParOps) ("apply", Nil, MAny :: SOption(MInt)) implements
      composite ${ meta[MTilePar]($0).map(_.par) }


    /**
     * Pipeline style
     * Used to track the scheduling type for outer controller nodes
     * User facing: No
     * Set: styleOf(Rep[Any]) = ControlType
     * Get: styleOf(Rep[Any])   // Returns ControlType. Error if undefined
     * Get: styleOption(Rep[Any]) // Returns Option[ControlType]. None if undefined
     **/
    val MControlType = metadata("MControlType", "tpe" -> ControlType)
    val styleOps = metadata("styleOf")
    internal.static (styleOps) ("update", Nil, (MAny, ControlType) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MControlType($1)) }
    internal.static (styleOps) ("apply", Nil, MAny :: ControlType) implements
      composite ${ meta[MControlType]($0).get.tpe }

    internal (styleOps) ("styleOption", Nil, MAny :: SOption(ControlType)) implements
      composite ${ meta[MControlType]($0).map(_.tpe) }


    /**
     * Number of pipeline stages
     * ASSUMPTION: Pipeline stages are always a linear sequence of dependencies
     * User facing: No
     * Set: nStages(Rep[Any]) = Int
     * Get: nStages(Rep[Any])   // Returns Int. Error if undefined.
     **/
    val MNumStages = metadata("MNumStages", "nStages" -> SInt)
    val nstages    = metadata("nStages")
    internal.static (nstages) ("update", Nil, (MAny, SInt) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MNumStages($1)) }
    internal.static (nstages) ("apply", Nil, MAny :: SInt) implements
      composite ${ meta[MNumStages]($0).get.nStages }


    /**
     * Unit range flag
     * Used to track if a Range represents only a single value (e.g. 3::4)
     * User facing: No
     * Set: isUnit(Rep[Any]) = Boolean
     * Get: isUnit(Rep[Any])   // Returns Boolean. Error if undefined.
     **/
    val MUnitRange = metadata("MUnitRange", "isUnit" -> SBoolean)
    val unitOps = metadata("isUnit")
    internal.static (unitOps) ("update", Nil, (MAny, SBoolean) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MUnitRange($1)) }
    internal.static (unitOps) ("apply", Nil, MAny :: SBoolean) implements
      composite ${ meta[MUnitRange]($0).get.isUnit }


    /**
     * Tile ranges
     * Tracks the memory range described by a given Tile
     * User facing: No
     * Set: rangesOf(Rep[Tile[T]]) = List[Rep[Range]]
     * Get: rangesOf(Rep[Tile[T]])   // Returns List[Rep[Range]]. Error if undefined.
     **/
    val MTileRanges = metadata("MTileRanges", "ranges" -> SList(Range))
    val rangesOps = metadata("rangesOf")
    internal.static (rangesOps) ("update", T, (Tile(T), SList(Range)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MTileRanges($1)) }
    internal.static (rangesOps) ("apply", T, Tile(T) :: SList(Range)) implements
      composite ${ meta[MTileRanges]($0).get.ranges }


    /**
     * Global flag
     * Tracks values that are computed at most once (constants or outside all controllers)
     * User facing: No
     * Set: isGlobal(Rep[Any]) = Boolean
     * Get: isGlobal(Rep[Any])   // Returns Boolean. False if undefined.
     **/
    val MGlobal = metadata("MGlobal", "isGlobal" -> SBoolean)
    val globalOps = metadata("isGlobal")
    onMeet (MGlobal) ${ MGlobal(this.isGlobal && that.isGlobal) }
    internal.static (globalOps) ("update", Nil, (MAny, SBoolean) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MGlobal($1)) }
    internal.static (globalOps) ("apply", Nil, MAny :: SBoolean) implements
      composite ${ globalCheck($0) }

    internal (globalOps) ("globalCheck", Nil, MAny :: SBoolean) implements
      composite ${ meta[MGlobal]($0).map(_.isGlobal).getOrElse(false) }


    /**
     * Parameter ranges
     * Tracks minimum, step, and maximum for a given design Param
     * User facing: Yes
     * Set: domainOf(Rep[Any]) = (Int,Int,Int)
     * Get: domainOf(Rep[Any])   // Returns Option[(Int,Int,Int)]. None if undefined.
     */
    val MParamRange = metadata("MParamRange", "minv" -> SInt, "maxv" -> SInt, "stepv" -> SInt)
    val prangeOps = metadata("domainOf")
    static (prangeOps) ("update", Nil, (MAny, CTuple3(SInt,SInt,SInt)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MParamRange($1._1,$1._2+$1._3,$1._3)) }

    static (prangeOps) ("apply", Nil, MAny :: SOption(CTuple3(SInt,SInt,SInt))) implements composite ${
      meta[MParamRange]($0).map(d => (d.minv, d.maxv, d.stepv))
    }


    /**
     * Symbol bounds
     * Tracks the maximum value for a given symbol, along with data about this bound
     * - Fixed = fixed value for all future time (constants or finalized parameters)
     * - Exact = constant value but which may be changed (unfinalized parameters)
     * - Bound = any other upper bound
     * ASSUMPTION: Used only for non-negative size and index calculation
     * User facing: Yes
     * TODO: Should probably change to BigDecimal or something to be accurate
     * Set: bound(Rep[Any]) = Double
     * Set: bound(Rep[Any]) = MBound
     * Set: bound(Rep[Any]) = Option[MBound]
     * Get: bound(Rep[Any])   // Returns Option[Double]. None if undefined.
     *
     * Helper functions:
     * exact(Double)    // Creates an "exact" bound
     * fixed(Double)    // Creates a "fixed" bound
     *
     * Extractors:
     * Bound(Rep[Any])  // Returns Option[Double]. Defined for any bound
     * Exact(Rep[Any])  // Returns Option[Double]. Defined only for fixed or exact bounds
     * Fixed(Rep[Any])  // Returns Option[Double]. Defined only for fixed bounds
     **/
    val MBound = metadata("MBound", "bound" -> SDouble, "exact" -> SBoolean, "locked" -> SBoolean)
    val boundOps = metadata("bound")
    static (boundOps) ("update", Nil, (MAny, SDouble) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MBound($1, false, false)) }
    static (boundOps) ("update", Nil, (MAny, MBound) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, $1) }
    static (boundOps) ("update", Nil, (MAny, SOption(MBound)) :: MUnit, effect = simple) implements
      composite ${ $1.foreach{bnd => setMetadata($0, bnd) } }
    static (boundOps) ("apply", Nil, MAny :: SOption(SDouble)) implements
      composite ${ boundOf($0).map(_.bound) }

    internal (boundOps) ("boundOf", Nil, MAny :: SOption(MBound)) implements composite ${ meta[MBound]($0) }

    internal (boundOps) ("exact", Nil, SDouble :: MBound) implements composite ${ MBound($0, true, false) }
    internal (boundOps) ("fixed", Nil, SDouble :: MBound) implements composite ${ MBound($0, true, true) }

    val boundUnapply = metadata("Bound")
    internal.static (boundUnapply) ("unapply", Nil, MAny :: SOption(SDouble)) implements composite ${
      boundOf($0).map(_.bound)
    }

    val exactUnapply = metadata("Exact")
    internal.static (exactUnapply) ("unapply", Nil, MAny :: SOption(SDouble)) implements composite ${
      boundOf($0) match { case Some(MBound(bnd,true,_   )) => Some(bnd);  case _ => None }
    }
    val fixedUnapply = metadata("Fixed")
    internal.static (fixedUnapply) ("unapply", Nil, MAny :: SOption(SDouble)) implements composite ${
      boundOf($0) match { case Some(MBound(bnd,true,true)) => Some(bnd);  case _ => None }
    }


    /**
     * Unrolling factors
     * The total set of unrolling factors for a given node. Defined such that, given a memory M and accessor A:
     * unrollFactorsOf(M) - unrollFactorsOf(M) = P
     * Where product(P) is the number of parallel accesses to M associated with access A.
     * User facing: No
     * Set: unrollFactorsOf(Rep[Any]) = List[Rep[Int]]
     * Get: unrollFactorsOf(Rep[Any])   // Returns List[Rep[Int]]. Nil if undefined.
     **/
    val MUnrollingFactors = metadata("MUnrollingFactors", "factors" -> SList(MInt))
    val unrollFactorOps = metadata("unrollFactorsOf")
    static (unrollFactorOps) ("update", Nil, (MAny, SList(MInt)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MUnrollingFactors($1)) }
    static (unrollFactorOps) ("apply", Nil, (MAny) :: SList(MInt)) implements
      composite ${ meta[MUnrollingFactors]($0).map(_.factors).getOrElse(Nil) }


    /**
     * Parallelization factors
     * Isolated symbol parallelization factors (TODO: Clarify usage)
     * User facing: No
     * Set: parFactorsOf(Rep[Any]) = List[Rep[Int]]
     * Set: parFactorOf(Rep[Any]) = Rep[Int]
     * Get: parFactorsOf(Rep[Any])  // Returns List[Rep[Int]]. Nil if undefined.
     * Get: parFactorOf(Rep[Any])   // Returns Rep[Int]. Error if undefined.
     **/
    val MParFactors = metadata("MParFactors", "factors" -> SList(MInt))
    val parFactorsOps = metadata("parFactorsOf")
    static (parFactorsOps) ("update", Nil, (MAny, SList(MInt)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MParFactors($1)) }
    static (parFactorsOps) ("apply", Nil, (MAny) :: SList(MInt)) implements
      composite ${ parFactors($0) }

    // Singular version
    val parFactorOps = metadata("parFactorOf")
    static (parFactorOps) ("update", Nil, (MAny, MInt) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MParFactors(List($1))) }
    static (parFactorOps) ("apply", Nil, (MAny) :: MInt) implements
      composite ${ parFactors($0).head }

    internal (parFactorsOps) ("parFactors", Nil, (MAny) :: SList(MInt)) implements
      composite ${ meta[MParFactors]($0).map(_.factors).getOrElse(Nil) }


    /**
     * Memory contention
     * Tracks the number of potentially contending memory accessors with a given offchip memory access.
     * User facing: No
     * Set: contentionOf(Rep[Any]) = Int
     * Get: contentionOf(Rep[Any])   // Returns Int. 1 if undefined.
     **/
    val MContention = metadata("MContention", "contention" -> SInt)
    val contentionOps = metadata("contentionOf")
    internal.static (contentionOps) ("update", Nil, (MAny, SInt) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MContention($1)) }
    internal.static (contentionOps) ("apply", Nil, MAny :: SInt) implements
      composite ${ meta[MContention]($0).map(_.contention).getOrElse(1) }


    /**
     * Controller parent
     * Defines the controller which controls the reset of the given node.
     * Currently defined only for control nodes (readers/writers use writtenIn)
     * User facing: No
     * Set: parentOf(Rep[Any]) = Rep[Any]
     * Get: parentOf(Rep[Any])   // Returns Option[Rep[Any]]. None if undefined.
     * Get: parentOf((Rep[Any], Boolean)) // Returns Option[(Rep[Any], Boolean)]. None if undefined.
     **/
	 	/* TODO: Pipe/Metapipe/Sequential/Parallel: every node (includeing primitive nodes) inside the
		 * controller has the controller as its parent*/ //TODO: is this necessary?
    val MParent = metadata("MParent", "parent" -> MAny)
    val parentOps = metadata("parentOf")
    internal.static (parentOps) ("update", Nil, (MAny, MAny) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MParent($1)) }
    internal.static (parentOps) ("apply", Nil, MAny :: SOption(MAny)) implements
      composite ${ meta[MParent]($0).map(_.parent) }

    // Using verbose form here to avoid weird issue with if-statement in library
    internal.static (parentOps) ("apply", Nil, CTuple2(MAny,SBoolean) :: SOption(CTuple2(MAny,SBoolean))) implements composite ${
      $0._2 match {
        case true => Some(($0._1, false))
        case false => parentOf($0._1) match {
          case Some(p) => Some((p, false))
          case None => None
        }
      }
    }


    /**
     * Controller children
     * A list of control nodes inside given (outer) control node.
     * ASSUMPTION: Children form a linear sequence of stages (not an arbitrary dataflow graph)
     * User facing: No
     * Set: childrenOf(Rep[Any]) = List[Rep[Any]]
     * Get: childrenOf(Rep[Any])   // Returns List[Rep[Any]]. Nil if undefined.
     **/
    val MChildren = metadata("MChildren", "children" -> SList(MAny))
    val childrenOps = metadata("childrenOf")
    internal.static (childrenOps) ("update", Nil, (MAny, SList(MAny)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MChildren($1)) }
    internal.static (childrenOps) ("apply", Nil, (MAny) :: SList(MAny)) implements
      composite ${ meta[MChildren]($0).map(_.children).getOrElse(Nil) }


		/**
     * List of memories written by given controller
     * User facing: No
     * Set: writtenIn(Rep[Any]) = List[Rep[Any]]
     * Get: writtenIn(Rep[Any])   // Returns List[Rep[Any]]. Nil if undefined.
     **/
    val MWritten = metadata("MWritten", "written" -> SList(MAny))
    val writtenOps = metadata("writtenIn")
    internal.static (writtenOps) ("update", Nil, (MAny, SList(MAny)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MWritten($1)) }
    internal.static (writtenOps) ("apply", Nil, (MAny) :: SList(MAny)) implements
      composite ${ meta[MWritten]($0).map(_.written).getOrElse(Nil) }

		/**
     * List of writers for a given memory and owning controller
     * Tuple is (Controller, IsReduction, Writer)
     * IsReduction is only true for nodes within the inner reduction loop of AccumFold
		 * User facing: No
     * Set: writersOf(Rep[Any]) = (Rep[Any], Rep[Any])  // Single writer, isReduction is false
     * Set: writersOf(Rep[Any]) = (Rep[Any], Boolean, Rep[Any])
     * Set: writersOf(Rep[Any]) = List[(Rep[Any], Boolean, Rep[Any])]
     * Get: writersOf(Rep[Any])   // Returns List[(Rep[Any],Boolean,Rep[Any])]. Nil if undefined
     **/
    val MWriter = metadata("MWriters", "writers" -> SList(CTuple3(MAny,SBoolean,MAny)))
    val writersOps = metadata("writersOf")
    internal.static (writersOps) ("update", Nil, (MAny, CTuple2(MAny,MAny)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MWriters( List(($1._1,false,$1._2)) )) }
    internal.static (writersOps) ("update", Nil, (MAny, CTuple3(MAny,SBoolean,MAny)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MWriters(List($1))) }

    internal.static (writersOps) ("update", Nil, (MAny, SList(CTuple3(MAny,SBoolean,MAny))) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MWriters($1)) }

    internal.static (writersOps) ("apply", Nil, (MAny) :: SList(CTuple3(MAny,SBoolean,MAny))) implements
      composite ${ meta[MWriters]($0).map(_.writers).getOrElse(Nil) }

    /**
     * List of OUTER writers for a given memory
     * Same as writersOf, but defined as the controller which determines when a write is complete
     * rather than the controller directly above the write
     * Used for setting control signals for double+ buffers
     * User facing: No
     * Set: topWritersOf(Rep[Any]) = List[(Rep[Any], Boolean, Rep[Any])]
     * Get: topWritersOf(Rep[Any])   // Returns List[(Rep[Any], Boolean, Rep[Any])]. Nil if undefined.
     **/
    val MTopWriters = metadata("MTopWriters", "writers" -> SList(CTuple3(MAny, SBoolean, MAny)))
    val topWriterOps = metadata("topWritersOf")
    internal.static (topWriterOps) ("update", Nil, (MAny, SList(CTuple3(MAny, SBoolean, MAny))) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MTopWriters($1)) }

    internal.static (topWriterOps) ("apply", Nil, (MAny) :: SList(CTuple3(MAny,SBoolean,MAny))) implements
      composite ${ meta[MTopWriters]($0).map(_.writers).getOrElse(Nil) }

		/**
     * List of readers for a given memory
     * Tuple is (Controller, IsReduction, Reader)
     * IsReduction is only true for nodes within the inner reduction loop of AccumFold
     * User facing: No
     * Set: readersOf(Rep[Any]) = List[(Rep[Any], Boolean, Rep[Any])]
     * Get: readersOf(Rep[Any])   // Returns List[(Rep[Any], Boolean, Rep[Any])]. Nil if undefined.
     **/
    val MReaders = metadata("MReaders", "readers" -> SList(CTuple3(MAny,SBoolean,MAny)))
    val readersOps = metadata("readersOf")
    internal.static (readersOps) ("update", Nil, (MAny, SList(CTuple3(MAny,SBoolean,MAny))) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MReaders($1)) }
    internal.static (readersOps) ("apply", Nil, (MAny) :: SList(CTuple3(MAny,SBoolean,MAny))) implements
      composite ${ meta[MReaders]($0).map(_.readers).getOrElse(Nil) }


    /**
     * N-D access indices
     * Used to track the addresses for each dimension independent of address flattening.
     * User facing: No
     * Set: accessIndicesOf(Rep[Any]) = List[Rep[Index]]
     * Get: accessIndicesOf(Rep[Any])   // Returns List[Rep[Index]]. Nil if undefined.
     **/
    val MAccessIndices = metadata("MAccessIndices", "indices" -> SList(Idx))
    val accessOps = metadata("accessIndicesOf")
    internal.static (accessOps) ("update", Nil, (MAny, SList(Idx)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MAccessIndices($1)) }
    internal.static (accessOps) ("apply", Nil, MAny :: SList(Idx)) implements
      composite ${ meta[MAccessIndices]($0).map(_.indices).getOrElse(Nil) }


    /**
     * Parallelized N-D access indices
     **/
    val MParAccessIndices = metadata("MParAccessIndices", "indices" -> SList(SList(Idx)))
    val parAccessOps = metadata("parIndicesOf")
    internal.static (parAccessOps) ("update", Nil, (MAny, SList(SList(Idx))) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MParAccessIndices($1)) }
    internal.static (parAccessOps) ("apply", Nil, (MAny) :: SList(SList(Idx))) implements
      composite ${ meta[MParAccessIndices]($0).map(_.indices).getOrElse(Nil) }

		/* MaxJ Codegen Helper Functions */
    val maxjgrp = grp("maxjGrp")
		/* Not real metadata but need to be globally accessable */
    val maxjmeta = metadata("maxjMeta")
    internal.direct (maxjgrp) ("maxJPreG", Nil, SInt :: SString) implements composite ${
      if ( $0 == 1 ) "DFEVar"
      else "DFEVector<DFEVar>"
    }
    internal.direct (maxjmeta) ("maxJPre", T, T :: SString) implements composite ${
      maxJPreG(parOf( $0 ))
    }
		internal.direct (maxjmeta) ("tpstr", T, SInt :: SString) implements composite ${
			tpstrG[T]( $0 )
		}
		internal.direct (maxjgrp) ("tpstrG", T, SInt :: SString) implements composite ${
			val scalart = if (isFixPtType(manifest[T])) {
				val s = sign(manifest[T].typeArguments(0))
				val d = nbits(manifest[T].typeArguments(1))
				val f = nbits(manifest[T].typeArguments(2))
				if (s) "dfeFixOffset( "+ (d+f) + "," + f + ", SignMode.TWOSCOMPLEMENT)"
				else "dfeFixOffset("+ (d+f) + "," + f + ", SignMode.UNSIGNED)"
			} else if (isFltPtType(manifest[T])) {
				val e = nbits(manifest[T].typeArguments(0))
				val m = nbits(manifest[T].typeArguments(1))
				"dfeFloat(" + e + "," + m + ")"
			} else if (isBitType(manifest[T])) {
			  "dfeFixOffset(1, 0, SignMode.UNSIGNED)"
      } else if (isCounter(manifest[T])) {
        "dfeFixOffset(32,0,SignMode.TWOSCOMPLEMENT)"  // TODO: Is this safe?
			} else {
        // Was commented out before, not sure why
				throw new Exception("Unknown type " + manifest[T])
			}
			if ( $0 > 1) {
				"new DFEVectorType<DFEVar>(" + scalart + "," + $0 + ")"
			} else {
				scalart
			}
		}
	}
}
