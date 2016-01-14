package ppl.dsl.forge
package dsls
package dhdl 

trait PrimOps {
  this: DHDLDSL =>

	def importDHDLPrimitives () = {

    val Prim = grp("DHDLPrim")

		val FixPt = lookupTpe("Long")

		//val floatAdd = infix (Prim) ("+", Nil, (MFloat, MFloat) :: MFloat)
		//impl (floatAdd) (codegen($cala, ${$0 + $1}))
		//TODO: Assume precision align for now
		val fxpAdd = infix (Prim) ("+", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpAdd) (codegen($cala, ${$0 + $1}))

		val fxpSub = infix (Prim) ("-", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpSub) (codegen($cala, ${$0 - $1}))

		val fxpAnd = infix (Prim) ("&", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpAnd) (codegen($cala, ${$0 & $1}))

		val fxpOr = infix (Prim) ("|", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpOr) (codegen($cala, ${$0 | $1}))

		val fxpLls = infix (Prim) ("<<", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpLls) (codegen($cala, ${$0 << $1}))

		val fxpLrs = infix (Prim) (">>", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpLrs) (codegen($cala, ${$0 >> $1}))

		val fxpAbs = direct (Prim) ("abs", Nil, FixPt :: FixPt)
		impl (fxpAbs) (codegen($cala, ${scala.math.abs($0)}))

	}
}

