package ppl.dsl.forge
package dsls
package dhdl

trait DHDLMetas {
  this: DHDLDSL =>

	def importDHDLMetadata () = {
		val sizes = grp("size")
		val SList = lookupTpe("scala.List", stage=compile)
		val SString = lookupTpe("java.lang.String", stage=compile)
		val SBoolean = lookupTpe("scala.Boolean", stage=compile)
		val RegTpe = lookupTpe("RegTpe", stage=compile)
		val T = tpePar("T")

		/* Multidimension size */
		val MSize = metadata("MSize", ("size",SList(SInt)))
		val sizeOps = grp("size")
		meet (MSize) ${ this }
		internal.static (sizeOps) ("update", T, (T, SList(SInt)) :: MUnit, effect = simple) implements
			composite ${ setMetadata($0, MSize($1)) }
		internal.direct (sizeOps) ("setMSize", T, (T, SInt, SInt) :: MUnit, effect = simple) implements
			composite ${size($0) = size.apply($0).size.updated($1, $2)}
		internal.static (sizeOps) ("apply", T, T :: MSize) implements composite ${
			meta[MSize]($0).get}
		internal.direct (sizeOps) ("getSize", T, (T, SInt) :: SInt) implements composite ${
			size($0).size($1)}

		/* Name of a node */
		val MName = metadata("MName", ("name",SString))
		val nameOps = grp("name")
		meet (MName) ${ this }
		internal.static (nameOps) ("update", T, (T, SString) :: MUnit, effect = simple) implements
			composite ${ setMetadata($0, MName($1)) }
		internal.static (nameOps) ("apply", T, T :: MName) implements composite ${
			meta[MName]($0).get}
		internal.direct (nameOps) ("getName", T, T :: SString) implements composite ${
			name($0).name}

		/* Is Double Buffer */
		val MDblBuf = metadata("MDblBuf", ("isDblBuf",SBoolean))
		val dblBufOps = grp("dblbuf")
		meet (MDblBuf) ${ this }
		internal.static (dblBufOps) ("update", T, (T, SBoolean) :: MUnit, effect = simple) implements
			composite ${ setMetadata($0, MDblBuf($1)) }
		internal.static (dblBufOps) ("apply", T, T :: MDblBuf) implements composite ${
			meta[MDblBuf]($0).get}
		internal.direct (dblBufOps) ("isDblBuf", T, T :: SBoolean) implements composite ${
			dblbuf($0).isDblBuf}

		/* Register Type  */
		val MRegTpe = metadata("MRegTpe", ("regTpe",RegTpe))
		val regTpeOps = grp("regtpe")
		meet (MRegTpe) ${ this }
		internal.static (regTpeOps) ("update", T, (T, RegTpe) :: MUnit, effect = simple) implements
			composite ${ setMetadata($0, MRegTpe($1)) }
		internal.static (regTpeOps) ("apply", T, T :: MRegTpe) implements composite ${
			meta[MRegTpe]($0).get}
		internal.direct (regTpeOps) ("getRegTpe", T, T :: RegTpe) implements composite ${
			regtpe($0).regTpe}
	}
}
