package ppl.dsl.forge
package dsls
package dhdl 

import core.{ForgeApplication,ForgeApplicationRunner}

object DHDLDSLRunner extends ForgeApplicationRunner with DHDLDSL 

trait DHDLDSL extends ForgeApplication 
	with PrimOps with MiscOps{
	
  def dslName = "DHDL"
	
  override def addREPLOverride = false 

  def specification() = {

		val MFixPt = tpe("Int", stage=future)

		importDHDLPrimitives()
		importMiscs()

		val Prim = lookupGrp("DHDLPrim")
		lift(Prim) (MFloat)
		lift(Prim) (MFixPt)
		lift(Prim) (MBoolean)

		val TConv = grp("TypeConv") 
		val float2FixPt = infix (TConv) ("toFixPt", Nil, MFloat::MFixPt)	
		impl (float2FixPt) (codegen($cala, ${$0.toInt})) 
		val fixPt2Float = infix (TConv) ("toFloat", Nil, MFixPt::MFloat)	
		impl (fixPt2Float) (codegen($cala, ${$0.toFloat})) 
		val fixPt2Bool = infix (TConv) ("toBoolean", Nil, MFixPt::MBoolean)
		impl (fixPt2Bool) (codegen($cala, ${$0 != 0})) 
		val bool2FixPt = infix (TConv) ("toFixPt", Nil, MBoolean::MFixPt)
		impl (bool2FixPt) (codegen($cala, ${if ($0) 1 else 0})) 

		fimplicit (TConv) ("fixpt_to_float", Nil, MFixPt::MFloat) implements composite ${$0.toFloat}
		fimplicit (TConv) ("float_to_fixpt", Nil, MFloat::MFixPt) implements composite ${$0.toFixPt}

		()
	}
}
