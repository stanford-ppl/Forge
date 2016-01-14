package ppl.dsl.forge
package dsls
package dhdl 

import core.{ForgeApplication,ForgeApplicationRunner}

object DHDLDSLRunner extends ForgeApplicationRunner with DHDLDSL 

trait DHDLDSL extends ForgeApplication 
	with PrimOps with MiscOps{

	val MAX_FIXPT_PRECISION = 64
  def dslName = "DHDL"
	
  override def addREPLOverride = false 

  def specification() = {

		val FixPt = tpe("Long", stage=future)

		val TpeOps = grp("TpeOps")

		lift(TpeOps) (FixPt)
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

		val FixPtOps = withTpe (FixPt)
		FixPtOps {
			infix ("getSign") (Nil :: MInt) implements single ${
				val MAX_FIXPT_PRECISION = 64
				val SIGN_MASK:Long = 1L << (MAX_FIXPT_PRECISION-1)
				(SIGN_MASK & $0) >> (MAX_FIXPT_PRECISION-1)
			}
			/* getInt(intPrec, fracPrec): returns integer part of the fix point number */
			infix ("getInt") ((MInt, MInt) :: MInt) implements composite ${
				val intPrec = $1
				val fracPrec = $2
				val intMask = 1L << (intPrec + fracPrec) - 1L 
				(abs($self) & intMask) >> fracPrec
			}
			/* getFrac(fracPrec): returns fraction part of the fix point number */
			infix ("getFrac") (MInt :: MInt) implements composite ${
				val fracPrec = $1
				val fracMask = (1L << fracPrec.toFixPt) - 1L
				(abs($self) & fracMask)
			}
			infix ("toString") ((MInt, MInt) :: MString) implements composite ${
				if ($self.getSign==0) "" else "-" 
				+ $self.getFrac($1, $2) + "." + frac
			}
			direct ("FixPt") (((MInt,MInt,MInt,MInt,MInt) :: MInt)) implements codegen ($cala, ${
				val intPrec = $4
				val fracPrec = $5
				val sign = $1
				val int = $2
				val frac = $3
				val posval = (int << fracPrec.toLong) | frac.toLong 
				if (sign==0) posval else -posval
			})
			val fix2Float = infix ("toFloat") (Nil :: MFloat)
			impl (fix2Float) (codegen($cala, ${
				val intprec = 31
				val fracprec = 0
				//TODO
				$0.toFloat
			}))
			val fix2Int = infix ("toInt") (Nil :: MInt)
			impl (fix2Int) (composite ${
				val intprec = 31
				val fracprec = 0
				$0 >> fracprec.toLong
			})
		
			fimplicit ("fixpt_to_float") (Nil::MFloat) implements composite ${ $self.toFloat }
			fimplicit ("fixpt_to_int") (Nil::MInt) implements composite ${ $self.toInt }
		}

		val IntOps = withTpe (MInt)
		IntOps {
			val int2fxp = infix ("toFixPt") (Nil::FixPt)
			impl (int2fxp) (codegen($cala, ${ $self.toLong } ))
			fimplicit ("int_to_fixpt") (Nil::FixPt) implements composite ${ $self.toFixPt }

			infix ("+") (MInt :: MInt) implements single ${$self + $1}
		}

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
