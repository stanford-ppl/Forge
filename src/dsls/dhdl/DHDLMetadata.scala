package ppl.dsl.forge
package dsls
package dhdl

trait DHDLMetadata {
  this: DHDLDSL =>

  def importDHDLMetadata () = {
    val T = tpePar("T")

    val RegTpe    = lookupTpe("RegTpe", stage=compile)
    val PipeStyle = lookupTpe("PipeStyle", stage=compile)
    val Reg       = lookupTpe("Reg")
    val Pipeline  = lookupTpe("Pipeline")
    val Idx       = lookupAlias("Index")
    val Tile      = lookupTpe("Tile")
    val Range     = lookupTpe("Range")

    val CTuple3   = lookupTpe("CTuple3")

    /* Static length (for indices and counterchain) */
    val MDims = metadata("MLength", "len" -> SInt)
    val lenOps = metadata("lenOf")
    onMeet(MDims) ${ this }
    internal.static (lenOps) ("update", Nil, (MAny, SInt) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MLength($1)) }
    internal.static (lenOps) ("apply", Nil, MAny :: SInt) implements composite ${ meta[MLength]($0).get.len }


    /* Staged multidimension dimensions */
    val MStagedDims = metadata("MStagedDims", "dims" -> SList(Idx))
    val dimOps = metadata("dimsOf")

    onMeet(MStagedDims) ${ this }
    internal.static (dimOps) ("update", T, (T, SList(Idx)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MStagedDims($1)) }
    internal.static (dimOps) ("apply", T, T :: SList(Idx)) implements composite ${ meta[MStagedDims]($0).get.dims }

    internal (dimOps) ("sizeOf", T, T :: Idx) implements composite ${ productTree(dimsOf($0)) }


    /* Name of a node */
    val MName = metadata("MName", "name" -> SString)
    val nameOps = metadata("nameOf")
    onMeet (MName) ${ this }
    internal.static (nameOps) ("update", T, (T, SString) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MName($1)) }
    internal.static (nameOps) ("apply", T, T :: SString) implements composite ${
      meta[MName]($0) match {
        case Some(n) => n.name
        case None => ""
      }
    }

    /* Is Double Buffer */
    val MDblBuf = metadata("MDblBuf", "isDblBuf" -> SBoolean)
    val dblBufOps = metadata("isDblBuf")
    onMeet (MDblBuf) ${ this }
    internal.static (dblBufOps) ("update", T, (T, SBoolean) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MDblBuf($1)) }
    internal.static (dblBufOps) ("apply", T, T :: SBoolean) implements composite ${ meta[MDblBuf]($0).get.isDblBuf }

    /* Register Type  */
    val MRegTpe = metadata("MRegTpe", "regTpe" -> RegTpe)
    val regTpeOps = metadata("regType")
    onMeet (MRegTpe) ${ this }
    internal.static (regTpeOps) ("update", T, (T, RegTpe) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MRegTpe($1)) }
    internal.static (regTpeOps) ("apply", T, T :: RegTpe) implements composite ${ meta[MRegTpe]($0).map(_.regTpe).getOrElse(Regular) }

    /* Register Initial Value */
    val MRegInit = metadata("MRegInit", "value" -> MAny)
    val regReset = metadata("resetValue")
    onMeet (MRegInit) ${ this }
    internal.static (regReset) ("update", T, (Reg(T), T) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MRegInit($1)) }
    internal.static (regReset) ("apply", T, Reg(T) :: T) implements
      composite ${ meta[MRegInit]($0).get.value.asInstanceOf[Rep[T]] }

    /* Parallelization Factor  */
    val MPar = metadata("MPar", "par" -> SInt)
    val parOps = metadata("par")
    onMeet (MPar) ${ this }
    internal.static (parOps) ("update", T, (T, SInt) :: MUnit, effect = simple) implements
    composite ${ setMetadata($0, MPar($1)) }
    internal.static (parOps) ("apply", T, T :: SInt) implements composite ${
      meta[MPar]($0) match {
        case Some(p) => p.par
        case None => 1
      }
    }

    /* Number of Banks  */
    val MBank = metadata("MBank", "nBanks" -> SInt)
    val bankOps = metadata("banks")
    //TODO:
    onMeet (MBank) ${ this }
    internal.static (bankOps) ("update", T, (T, SInt) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MBank($1)) }
    internal.static (bankOps) ("apply", T, T :: SInt) implements composite ${ meta[MBank]($0).get.nBanks }

    /* Pipeline style */
    val MPipeType = metadata("MPipeType", "tpe" -> PipeStyle)
    val styleOps = metadata("styleOf")
    onMeet (MPipeType) ${ this }
    internal.static (styleOps) ("update", Nil, (MAny, PipeStyle) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MPipeType($1)) }
    internal.static (styleOps) ("apply", Nil, MAny :: PipeStyle) implements composite ${ meta[MPipeType]($0).get.tpe }

    /* Pipeline stages */
    val MNumStages = metadata("MNumStages", "nStages" -> SInt)
    val nstages    = metadata("nStages")
    onMeet (MNumStages) ${ this }
    internal.static (nstages) ("update", Nil, (MAny, SInt) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MNumStages($1)) }
    internal.static (nstages) ("apply", Nil, MAny :: SInt) implements composite ${ meta[MNumStages]($0).get.nStages }

    /* Range is single dimension */
    val MUnitRange = metadata("MUnitRange", "isUnit" -> SBoolean)
    val unitOps = metadata("isUnit")
    onMeet (MUnitRange) ${ this }
    internal.static (unitOps) ("update", Nil, (MAny, SBoolean) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MUnitRange($1)) }
    internal.static (unitOps) ("apply", Nil, MAny :: SBoolean) implements composite ${ meta[MUnitRange]($0).get.isUnit }

    /* Tile Offsets */
    val MTileRanges = metadata("MTileRanges", "ranges" -> SList(Range))
    val rangesOps = metadata("rangesOf")
    onMeet (MTileRanges) ${ this }
    internal.static (rangesOps) ("update", T, (Tile(T), SList(Range)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MTileRanges($1)) }
    internal.static (rangesOps) ("apply", T, Tile(T) :: SList(Range)) implements composite ${ meta[MTileRanges]($0).get.ranges }

    /* Is global value (computed only once at setup) */
    val MGlobal = metadata("MGlobal", "isGlobal" -> SBoolean)
    val globalOps = metadata("isGlobal")
    onMeet (MGlobal) ${ MGlobal(this.isGlobal && that.isGlobal) }
    internal.static (globalOps) ("update", Nil, (MAny, SBoolean) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MGlobal($1)) }
    internal.static (globalOps) ("apply", Nil, MAny :: SBoolean) implements composite ${ meta[MGlobal]($0).map(_.isGlobal).getOrElse(false) }


    // TODO: Should probably change to BigDecimal or something to be accurate
    // NOTE: The user gets to see these! Woah.
    // Meant specifically for range analysis of non-negative size and index calculation

    // Couple of definitions for usage here:
    // - Final = fixed value for all future time (constants or finalized parameters)
    // - Exact = constant value but which may be changed (unfinalized parameters)
    // - Bound = any other upper bound

    val MBound = metadata("MBound", "bound" -> SDouble, "exact" -> SBoolean, "locked" -> SBoolean)
    val boundOps = metadata("bound")
    onMeet(MBound) ${ this }
    static (boundOps) ("update", Nil, (MAny, SDouble) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MBound($1, false, false)) }
    static (boundOps) ("update", Nil, (MAny, MBound) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, $1) }
    static (boundOps) ("update", Nil, (MAny, SOption(MBound)) :: MUnit, effect = simple) implements
      composite ${ $1.foreach{bnd => setMetadata($0, bnd) } }

    static (boundOps) ("apply", Nil, MAny :: SOption(MBound)) implements composite ${ meta[MBound] }

    internal (boundOps) ("boundsOf", Nil, MAny :: SOption(MBound)) implements composite ${ meta[MBound]($0) }

    val boundUnapply = metadata("Bound")
    internal.static (boundUnapply) ("unapply", Nil, MAny :: SOption(SDouble)) implements composite ${
      val bnd = boundsOf($0)
      if (bnd.isDefined) Some(bnd.get.bound) else None
    }

    internal (boundOps) ("exact", Nil, SDouble :: MBound) implements composite ${ MBound($0, true, false) }
    internal (boundOps) ("fixed", Nil, SDouble :: MBound) implements composite ${ MBound($0, true, true) }

    val exactUnapply = metadata("Exact")
    internal.static (boundUnapply) ("unapply", Nil, MAny :: SOption(SDouble)) implements composite ${
      val bnd = boundsOf($0)
      if (bnd.isDefined && bnd.get.exact) Some(bnd.get.bound) else None
    }
    val lockUnapply = metadata("Fixed")
    internal.static (lockUnapply) ("unapply", Nil, MAny :: SOption(SDouble)) implements composite ${
      val bnd = boundsOf($0)
      if (bnd.isDefined && bnd.get.locked) Some(bnd.get.bound) else None
    }


    internal (boundOps) ("extractNumericConstant", T, T :: SOption(SDouble)) implements composite ${
      val mD = manifest[Double]
      val mF = manifest[Float]
      val mI = manifest[Int]
      val mL = manifest[Long]

      manifest[T] match {
        case `mI` => Some($0.asInstanceOf[Int].toDouble)
        case `mL` => Some($0.asInstanceOf[Long].toDouble)
        case `mF` => Some($0.asInstanceOf[Float].toDouble)
        case `mD` => Some($0.asInstanceOf[Double])
        case _ => None
      }
    }
    rewrite (boundOps, "boundsOf") using pattern(${p@Param(x)} -> ${
      val c = extractNumericConstant(x)
      if (p.finalized) fixed(c) else exact(c)
    })
    rewrite (boundOps, "boundsOf") using pattern(${Const(x)} -> ${ fixed(extractNumericConstant(x)) })

    /* Parent of a node */
    val MParent = metadata("MParent", "parent" -> MAny)
    val parentOps = metadata("parentOf")
    onMeet (MParent) ${ this }
    internal.static (parentOps) ("update", T, (T, MAny) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MParent($1)) }
    internal.static (parentOps) ("apply", T, T :: SOption(MAny)) implements composite ${
    	meta[MParent]($0) match {
    	  case Some(p) => Some(p.parent)
    	  case None => None
    	}
		}

		/* MaxJ Codegen Helper Functions */
    val maxjgrp = grp("maxjGrp")
		/* Not real metadata but need to be globally accessable */
    val maxjmeta = metadata("maxjMeta")
    internal.direct (maxjgrp) ("maxJPreG", Nil, SInt :: SString) implements composite ${
      if ( $0 == 1 ) "DFEVar"
      else "DFEVector<DFEVar>"
    }
    internal.direct (maxjmeta) ("maxJPre", T, T :: SString) implements composite ${
      maxJPreG(par( $0 ))
    }
		internal.direct (maxjmeta) ("tpstr", T, SInt :: SString) implements composite 	${
			tpstrG[T]( $0 )
		}
		internal.direct (maxjgrp) ("tpstrG", T, SInt :: SString) implements composite 	${
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
				"TODO"
			} else {
				//throw new Exception("Unknown type " + manifest[T])
				""
			}
			if ( $0 > 1) {
				"new DFEVectorType<DFEVar>(" + scalart + "," + $0
			} else {
				scalart
			}
		}
	}
}
