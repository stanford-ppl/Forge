package ppl.dsl.forge
package dsls
package dhdl 

trait PrimOps {
  this: DHDLDSL =>

	def importDHDLPrimitives () = {

    val Prim = grp("DHDLPrim")

		val MFixPt = lookupTpe("Int")

		val floatAdd = infix (Prim) ("+", Nil, (MFloat, MFloat) :: MFloat)
		impl (floatAdd) (codegen($cala, ${$0 + $1}))
		val fixPtAdd = infix (Prim) ("+", Nil, (MFixPt, MFixPt) :: MFixPt)
		impl (fixPtAdd) (codegen($cala, ${$0 + $1}))
	}
}

