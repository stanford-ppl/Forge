package ppl.dsl.forge
package dsls
package dhdl

trait DHDLMetadata {
  this: DHDLDSL =>

	def importDHDLMetadata () = {
		val T = tpePar("T")
		val RegTpe    = lookupTpe("RegTpe", stage=compile)
    val PipeStyle = lookupTpe("PipeStyle", stage=compile)
    val Reg = lookupTpe("Reg")
    val Fix = lookupTpe("Fix")
    val Pipeline = lookupTpe("Pipeline")

		/* Static multidimension size */
		val MSize = metadata("MSize", "size" -> SList(SInt))
		val sizeOps = metadata("sizeOf")

		onMeet (MSize) ${ this }
		internal.static (sizeOps) ("update", T, (T, SList(SInt)) :: MUnit, effect = simple) implements
			composite ${ setMetadata($0, MSize($1)) }
		//internal.direct (sizeOps) ("setMSize", T, (T, SInt, SInt) :: MUnit, effect = simple) implements
	  //	composite ${sizeOf($0) = sizeOf($0).updated($1, $2)}
		internal.static (sizeOps) ("apply", T, T :: SList(SInt)) implements composite ${ meta[MSize]($0).get.size }
		internal.direct (sizeOps) ("getDim", T, (T, SInt) :: SInt) implements composite ${ sizeOf($0).apply($1) }

    /* Dynamic multidimension size */
    val MDynamicSize = metadata("MDynamicSize", "size" -> SList(Fix))
    val dynamicSizeOps = metadata("symSizeOf")

    onMeet(MDynamicSize) ${ this }
    internal.static (dynamicSizeOps) ("update", T, (T, SList(Fix)) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MDynamicSize($1)) }
    internal.static (dynamicSizeOps) ("apply", T, T :: SList(Fix)) implements composite ${ meta[MDynamicSize]($0).get.size }

		/* Name of a node */
		val MName = metadata("MName", "name" -> SString)
		val nameOps = metadata("nameOf")
		onMeet (MName) ${ this }
		internal.static (nameOps) ("update", T, (T, SString) :: MUnit, effect = simple) implements
			composite ${ setMetadata($0, MName($1)) }
		internal.static (nameOps) ("apply", T, T :: SString) implements composite ${ meta[MName]($0).get.name }

		/* Is Double Buffer */
		val MDblBuf = metadata("MDblBuf", "isDblBuf" -> SBoolean)
		val dblBufOps = metadata("dblbuf")
		onMeet (MDblBuf) ${ this }
		internal.static (dblBufOps) ("update", T, (T, SBoolean) :: MUnit, effect = simple) implements
			composite ${ setMetadata($0, MDblBuf($1)) }
		internal.static (dblBufOps) ("apply", T, T :: MDblBuf) implements composite ${ meta[MDblBuf]($0).get }
		internal.direct (dblBufOps) ("isDblBuf", T, T :: SBoolean) implements composite ${ dblbuf($0).isDblBuf }

		/* Register Type  */
		val MRegTpe = metadata("MRegTpe", "regTpe" -> RegTpe)
		val regTpeOps = metadata("regtpe")
		onMeet (MRegTpe) ${ this }
		internal.static (regTpeOps) ("update", T, (T, RegTpe) :: MUnit, effect = simple) implements
			composite ${ setMetadata($0, MRegTpe($1)) }
		internal.static (regTpeOps) ("apply", T, T :: RegTpe) implements
      composite ${ meta[MRegTpe]($0).get.regTpe }

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
		//TODO:
		onMeet (MPar) ${ this }
		internal.static (parOps) ("update", T, (T, SInt) :: MUnit, effect = simple) implements
			composite ${ setMetadata($0, MPar($1)) }
		internal.static (parOps) ("apply", T, T :: SInt) implements composite ${
			meta[MPar]($0) match {
				case Some(p) => p.par
				case None => 1
			}
		}
		internal.direct (parOps) ("maxJPre", T, T :: SString) implements composite ${
			maxJPrefix(par( $0 ))
		}

		val normGrp = grp("normGrp")
		internal.direct (normGrp) ("maxJPrefix", Nil, SInt :: SString) implements composite ${
			if ( $0 == 1 ) "DFEVar"
			else "DFEVector<DFEVar>"
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
    internal.static (styleOps) ("update", Nil, (Pipeline, PipeStyle) :: MUnit, effect = simple) implements
      composite ${ setMetadata($0, MPipeType($1)) }
    internal.static (styleOps) ("apply", Nil, Pipeline :: PipeStyle) implements composite ${ meta[MPipeType]($0).get.tpe }
	}
}
