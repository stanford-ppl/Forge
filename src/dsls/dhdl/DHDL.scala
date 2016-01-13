package ppl.dsl.forge
package dsls
package dhdl 

import core.{ForgeApplication,ForgeApplicationRunner}

object DHDLDSLRunner extends ForgeApplicationRunner with DHDLDSL 

trait DHDLDSL extends ForgeApplication 
	with PrimOps with MiscOps{

	val MAX_FIXPT_PRECISION = 64
	val SIGN_MASK:Long = 1L << (MAX_FIXPT_PRECISION-1)
	
  def dslName = "DHDL"
	
  override def addREPLOverride = false 

  def specification() = {

		val FixPt = tpe("Long", stage=future)

		val TpeOps = grp("TpeOps")
		lift(TpeOps) (MInt)
		lift(TpeOps) (MFloat)
		lift(TpeOps) (MBoolean)

		def checkFixPtPrec (intPrec:Int, fracPrec:Int) = {
			if (intPrec>=MAX_FIXPT_PRECISION)
				throw new Exception("Integer precision cannot exceed " + (MAX_FIXPT_PRECISION-1) + ": " +
					intPrec)
			if (fracPrec>=MAX_FIXPT_PRECISION)
				throw new Exception("Fraction precision cannot exceed " + (MAX_FIXPT_PRECISION-1) + ": " + fracPrec)
			if ((intPrec + fracPrec)>=MAX_FIXPT_PRECISION) {
				throw new Exception("Sum of integer and fraction precisions cannot exceed "
					(MAX_FIXPT_PRECISION-1) + ": " + (intPrec + fracPrec))
			}
		}

		infix (TpeOps) ("getSign", Nil, FixPt :: MInt) implements signle ${
			(SIGN_MASK & $0) >> (MAX_FIXPT_PRECISION-1)
		}
		/* getInt(intPrec, fracPrec): returns integer part of the fix point number */
		infix (TpeOps) ("getInt", Nil, (FixPt, MInt, MInt) :: MInt) implements single ${
			val intPrec = $1
			val fracPrec = $2
			val intMask = (1L << intPrec + fracPrec) - 1 
			(scala.math.abs($0) & intMask) >> fracPrec
		}
		/* getFrac(fracPrec): returns fraction part of the fix point number */
		infix (TpeOps) ("getFrac", Nil, (FixPt, MInt) :: MInt) implements single ${
			val fracPrec = $1
			val fracMask = (1L << fracPrec) - 1
			(scala.math.abs($0) & fracMask)
		}
		direct (TpeOps) ("FixPt", Nil, (FixPt,MInt,MInt,MInt,MInt,MInt) :: MInt) implements codegen ${
			val intPrec = $4
			val fracPrec = $5
			val sign = $1
			val int = $2
			val frac = $3
			val posval = (int << fracPrec.toLong) | frac.toLong 
			if (sign==0) posval else -posval
		}
		infix (TpeOps) ("toString", Nil, (MInt :: MInt) :: MString) implements single ${
			if ($0.getSign==0) "" else "-" 
			+ $0.getFrac($1, $2) + "." + frac
		}
		val fix2Float = infix (TpeOps) ("toFloat", Nil, FixPt:: MFloat)
		impl (fix2Float) (codegen($cala, ${ $0.toString.toFloat }))

		//val FloatOps = withTpe (MFloat)
		//FloatOps {
		//	val float2fix = infix ("toFixPt") (Nil::FixPt)
		//	//TODO: Don't know how to convert float to fixpt without knowing precision in FixPt
		//	impl (float2fix) (codegen($cala, ${ FixPt($0.toInt, 0)} ))
		//}

		importDHDLPrimitives()
		importMiscs()

		//val Prim = lookupGrp("DHDLPrim")


		//val TConv = grp("TypeConv") 
		//val float2FixPt = infix (TConv) ("toFixPt", Nil, MFloat::MFixPt)	
		//impl (float2FixPt) (codegen($cala, ${$0.toInt})) 
		//val fixPt2Float = infix (TConv) ("toFloat", Nil, MFixPt::MFloat)	
		//impl (fixPt2Float) (codegen($cala, ${$0.toFloat})) 
		//val fixPt2Bool = infix (TConv) ("toBoolean", Nil, MFixPt::MBoolean)
		//impl (fixPt2Bool) (codegen($cala, ${$0 != 0})) 
		//val bool2FixPt = infix (TConv) ("toFixPt", Nil, MBoolean::MFixPt)
		//impl (bool2FixPt) (codegen($cala, ${if ($0) 1 else 0})) 

		//fimplicit (TConv) ("fixpt_to_float", Nil, MFixPt::MFloat) implements composite ${$0.toFloat}
		//fimplicit (TConv) ("float_to_fixpt", Nil, MFloat::MFixPt) implements composite ${$0.toFixPt}

		()
	}
}
