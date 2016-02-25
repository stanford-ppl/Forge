package ppl.dsl.forge
package dsls
package dhdl

trait DHDLMetas {
  this: DHDLDSL =>

	def importDHDLMetadata () = {
		val sizes = grp("size")
		val T = tpePar("T")
		val SList = tpe("scala.List", T, stage=compile)

		/* Multidimension size */
		val MSize = metadata("MSize", ("size",SList(SInt)))
		val sizeOps = grp("size")
		meet (MSize) ${ this }
		static (sizeOps) ("update", T, (T, SList(SInt)) :: MUnit, effect = simple) implements
		composite ${ setMetadata($0, MSize($1)) }
		direct (sizeOps) ("setMSize", T, (T, SInt, SInt) :: MUnit, effect = simple) implements
		composite ${size($0) = size.apply($0).size.updated($1, $2)}
		static (sizeOps) ("apply", T, T :: MSize) implements composite ${
		meta[MSize]($0).get}
		direct (sizeOps) ("getSize0", T, T :: SInt) implements composite ${
		size($0).size(0)}
	}
}
