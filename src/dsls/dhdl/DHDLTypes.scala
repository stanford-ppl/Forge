package ppl.dsl.forge
package dsls
package dhdl 

trait DHDLTypes {
  this: DHDLDSL =>

	def importDHDLTypes () = {
		val FixPt = tpe("Long", stage=future)
		tpeAlias("FixPt", FixPt)

		val TpeOps = grp("Tpes")

		lift(TpeOps) (FixPt)
		lift(TpeOps) (MInt)
		lift(TpeOps) (MFloat)
		lift(TpeOps) (MBoolean)
		//lift(TpeOps) (MString)

		compiler (TpeOps) ("checkFixPtPrec", Nil, (MInt,MInt)::MUnit) implements codegen ($cala, ${
			val intPrec = $0
			val fracPrec = $1
			val MAX_FIXPT_PRECISION = 64
			if (intPrec>=MAX_FIXPT_PRECISION)
				throw new Exception("Integer precision cannot exceed " + (MAX_FIXPT_PRECISION-1) + ": " +
					intPrec)
			if (fracPrec>=MAX_FIXPT_PRECISION)
				throw new Exception("Fraction precision cannot exceed " + (MAX_FIXPT_PRECISION-1) + ": " + fracPrec)
			if ((intPrec + fracPrec)>=MAX_FIXPT_PRECISION) {
				throw new Exception("Sum of integer and fraction precisions cannot exceed "
					(MAX_FIXPT_PRECISION-1) + ": " + (intPrec + fracPrec))
			}
		})

		direct (TpeOps) ("FixPt", Nil, MInt :: FixPt) implements codegen ($cala, ${
			val int = $0
			val intPrec = 31 
			val fracPrec = 0
			if (int > scala.math.pow(2,intPrec))
				throw new Exception("Integer precision not enough to hold integer value of the fix point: "
					+ int)
			int.toLong 
		})

		direct (TpeOps) ("FixPt", Nil, (MFloat,MInt,MInt) :: FixPt) implements codegen ($cala, ${
			val flt = $0
			val intPrec = $1 
			val fracPrec = $2
			if (scala.math.round(flt) > scala.math.pow(2,intPrec))
				throw new Exception("Integer precision not enough to hold integer value of the fix point: "
					+ scala.math.round(flt))
			scala.math.round(flt * scala.math.pow(2,fracPrec)).toLong 
		})

		fimplicit (TpeOps) ("fixpt_to_float", Nil, FixPt::MFloat) implements composite ${ $0.toFloat }
		fimplicit (TpeOps) ("int_to_fixpt", Nil, MInt::FixPt) implements composite ${ $0.toFixPt }

		val FixPtOps = withTpe (FixPt)
		FixPtOps {
			infix ("getSign") (Nil :: FixPt) implements composite ${
				val MAX_FIXPT_PRECISION = 64
				val SIGN_MASK = 1L << (MAX_FIXPT_PRECISION-1)
				(SIGN_MASK & $0) << ((MAX_FIXPT_PRECISION-1).toFixPt)
			}
			/* getInt(intPrec, fracPrec): returns integer part of the fix point number */
			infix ("getInt") (Nil:: FixPt) implements composite ${
				val intPrec = 31L
				val fracPrec = 0L
				//val intPrec = $1
				//val fracPrec = $2
				val intMask = 1L << (intPrec + fracPrec) - 1L 
				(abs($self) & intMask) << (fracPrec)
			}
			/* getFrac(fracPrec): returns fraction part of the fix point number */
			infix ("getFrac") (Nil :: FixPt) implements composite ${
				val fracPrec = 0
				//val fracPrec = $1
				val fracMask = (1L << fracPrec.toFixPt) - 1L
				(abs($self) & fracMask)
			}
			//importString()
			infix ("toString") (Nil:: MString) implements composite ${
				(if ($self.getSign==0L) "" else "-") 
				//+ $self.getInt.toString + "." + $self.getFrac.toString
			}
			val fix2Float = infix ("toFloat") (Nil :: MFloat)
			impl (fix2Float) (codegen($cala, ${
				val intprec = 31
				val fracprec = 0
				$self.toFloat * scala.math.pow(2,-fracprec).toFloat
			}))
			val fix2Int = infix ("toInt") (Nil :: MInt)
			impl (fix2Int) (codegen ($cala, ${
				val intprec = 31
				val fracprec = 0
				($self >> fracprec.toLong).toInt
			}))
			
		}

		val IntOps = withTpe (MInt)
		IntOps {
			val int2fxp = infix ("toFixPt") (Nil::FixPt)
			impl (int2fxp) (codegen($cala, ${ $self.toLong } ))

			infix ("+") (MInt :: MInt) implements codegen ($cala, ${ $self + $1 })
			infix ("-") (MInt :: MInt) implements codegen ($cala, ${ $self - $1 })
			infix ("*") (MInt :: MInt) implements codegen ($cala, ${ $self * $1 })
			infix ("/") (MInt :: MInt) implements codegen ($cala, ${ $self / $1 })

			infix ("==") (MInt :: MBoolean) implements codegen ($cala, ${ $self == $1 })

			infix ("toString") (Nil:: MString) implements codegen ($cala, ${
				$self.toString
			})
		}

		//val FloatOps = withTpe (MFloat)
		//FloatOps {
		//	val float2fix = infix ("toFixPt") (Nil::FixPt)
		//	//TODO: Don't know how to convert float to fixpt without knowing precision in FixPt
		//	impl (float2fix) (codegen($cala, ${ FixPt($0.toInt, 0)} ))
		//}


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
	}
}
