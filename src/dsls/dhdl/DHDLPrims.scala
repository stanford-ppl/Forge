package ppl.dsl.forge
package dsls
package dhdl 

trait PrimOps {
  this: DHDLDSL =>

	def importDHDLPrimitives () = {

    val Prim = grp("DHDLPrim")

		val FixPt = lookupTpe("Long")

		//TODO: Assume precision align for now
		val fxpAdd = infix (Prim) ("+", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpAdd) (codegen($cala, ${$0 + $1}))
		val fltAdd = infix (Prim) ("+", Nil, (MFloat, MFloat) :: MFloat)
		impl (fltAdd) (codegen($cala, ${$0 + $1}))

		val fxpSub = infix (Prim) ("-", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpSub) (codegen($cala, ${$0 - $1}))
		val fltSub = infix (Prim) ("-", Nil, (MFloat, MFloat) :: MFloat)
		impl (fltSub) (codegen($cala, ${$0 - $1}))

		val fxpMul = infix (Prim) ("*", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpMul) (codegen($cala, ${$0 * $1}))
		val fltMul = infix (Prim) ("*", Nil, (MFloat, MFloat) :: MFloat)
		impl (fltMul) (codegen($cala, ${$0 * $1}))

		val fxpDiv = infix (Prim) ("/", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpDiv) (codegen($cala, ${$0 / $1}))
		val fltDiv = infix (Prim) ("/", Nil, (MFloat, MFloat) :: MFloat)
		impl (fltDiv) (codegen($cala, ${$0 / $1}))

		val fixEq = infix (Prim) ("==", Nil, (FixPt, FixPt):: MBoolean)
		impl (fixEq) (codegen ($cala, ${$0 == $1}))
		val fltEq = infix (Prim) ("==", Nil, (MFloat, MFloat):: MBoolean)
		impl (fltEq) (codegen ($cala, ${$0 == $1}))
		val boolEq = infix (Prim) ("==", Nil, (MBoolean, MBoolean):: MBoolean)
		impl (boolEq) (codegen ($cala, ${$0 == $1}))

		val fixGt = infix (Prim) ("<", Nil, (FixPt, FixPt):: MBoolean)
		impl (fixGt) (codegen ($cala, ${$0 < $1}))
		val fltGt = infix (Prim) ("<", Nil, (MFloat, MFloat):: MBoolean)
		impl (fltGt) (codegen ($cala, ${$0 < $1}))

		val fixLt = infix (Prim) (">", Nil, (FixPt, FixPt):: MBoolean)
		impl (fixLt) (codegen ($cala, ${$0 > $1}))
		val fltLt = infix (Prim) (">", Nil, (MFloat, MFloat):: MBoolean)
		impl (fltLt) (codegen ($cala, ${$0 > $1}))

		val fxpAnd = infix (Prim) ("&", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpAnd) (codegen($cala, ${$0 & $1}))
		val boolAnd = infix (Prim) ("&&", Nil, (MBoolean, MBoolean):: MBoolean)
		impl (boolAnd) (codegen ($cala, ${$0 && $1}))

		val fxpOr = infix (Prim) ("|", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpOr) (codegen($cala, ${$0 | $1}))
		val boolOr = infix (Prim) ("||", Nil, (MBoolean, MBoolean):: MBoolean)
		impl (boolOr) (codegen ($cala, ${$0 || $1}))

		val fxpLls = infix (Prim) ("<<", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpLls) (codegen($cala, ${$0 << $1}))

		val fxpLrs = infix (Prim) (">>", Nil, (FixPt, FixPt) :: FixPt)
		impl (fxpLrs) (codegen($cala, ${$0 >> $1}))

		val fxpAbs = direct (Prim) ("abs", Nil, FixPt :: FixPt)
		impl (fxpAbs) (codegen($cala, ${scala.math.abs($0)}))
		val fltAbs = direct (Prim) ("abs", Nil, MFloat :: MFloat)
		impl (fltAbs) (codegen($cala, ${scala.math.abs($0)}))

	}
}

