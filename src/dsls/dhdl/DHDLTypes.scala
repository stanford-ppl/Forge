package ppl.dsl.forge
package dsls
package dhdl

trait DHDLTypes {
  this: DHDLDSL =>

	def importDHDLTypes () = {
		val T = tpePar("T", stage = compile)
    val Bit = lookupTpe("Bit")
		val Fix = lookupTpe("Fix")
    val Flt = lookupTpe("Flt")

		val Tpes = grp("Tpes")

    // TODO
		/*internal (TpeOps) ("checkFixPtPrec", Nil, (MInt,MInt)::MUnit, effect = simple) implements codegen ($cala, ${
			val intPrec = $0
			val fracPrec = $1
			val MAX_FIXPT_PRECISION = 64
			if (intPrec>=MAX_FIXPT_PRECISION)
				throw new Exception("Integer precision cannot exceed " + (MAX_FIXPT_PRECISION-1) + ": " + intPrec)
			if (fracPrec>=MAX_FIXPT_PRECISION)
				throw new Exception("Fraction precision cannot exceed " + (MAX_FIXPT_PRECISION-1) + ": " + fracPrec)
			if ((intPrec + fracPrec)>=MAX_FIXPT_PRECISION) {
				throw new Exception("Sum of integer and fraction precisions cannot exceed "
					(MAX_FIXPT_PRECISION-1) + ": " + (intPrec + fracPrec))
			}
		})*/

    val boolean_to_bit = direct (Tpes) ("cbit", Nil, CBoolean :: Bit)
    val bit_to_string = direct (Tpes) ("bit_to_string", Nil, Bit :: MString)


    val numeric_to_fixpt = direct (Fix) ("fixPt", T, T :: Fix, TNumeric(T))
    val fixpt_to_string = direct (Fix) ("fixpt_to_string", Nil, Fix :: MString)
    val fixpt_to_fltpt = direct (Fix) ("fixpt_to_fltpt", Nil, Fix :: Flt)

    val numeric_to_fltpt = direct (Flt) ("fltPt", T, T :: Flt, TNumeric(T))
    val fltpt_to_string = direct (Flt) ("fltpt_to_string", Nil, Flt :: MString)
    val fltpt_to_fixpt = direct (Flt) ("fltpt_to_fixpt", Nil, Flt :: Fix)

    infix (Tpes) ("toBit", Nil, SBoolean :: Bit) implements redirect ${ cbit($0) }
    infix (Bit) ("toString", Nil, Bit :: MString) implements redirect ${ bit_to_string($0) }
    infix (Bit) ("mkString", Nil, Bit :: MString) implements redirect ${ bit_to_string($0) }

    infix (Tpes) ("toFixPt", T, T :: Fix, TNumeric(T)) implements redirect ${ fixPt($0) }
    /*infix (Tpes) ("toFixPt", Nil, SInt :: Fix) implements redirect ${ long_to_fixpt($0.toLong) }
    infix (Tpes) ("toFixPt", Nil, SLong :: Fix) implements redirect ${ long_to_fixpt($0) }
    infix (Tpes) ("toFixPt", Nil, SFloat :: Fix) implements redirect ${ double_to_fixpt($0.toDouble) }
    infix (Tpes) ("toFixPt", Nil, SDouble :: Fix) implements redirect ${ double_to_fixpt($0) }*/
    infix (Fix) ("toString", Nil, Fix :: MString) implements redirect ${ fixpt_to_string($0) }
    infix (Fix) ("mkString", Nil, Fix :: MString) implements redirect ${ fixpt_to_string($0) }
    infix (Fix) ("toFltPt", Nil, Fix :: Flt) implements redirect ${ fixpt_to_fltpt($0) }


    infix (Tpes) ("toFltPt", T, T :: Flt, TNumeric(T)) implements redirect ${ fltPt($0) }
    /*infix (Tpes) ("toFltPt", Nil, SInt :: Flt) implements redirect ${ long_to_fltpt($0.toLong) }
    infix (Tpes) ("toFltPt", Nil, SLong :: Flt) implements redirect ${ long_to_fltpt($0) }
    infix (Tpes) ("toFltPt", Nil, SFloat :: Flt) implements redirect ${ double_to_fltpt($0.toDouble) }
    infix (Tpes) ("toFltPt", Nil, SDouble :: Flt) implements redirect ${ double_to_fltpt($0) }*/
    infix (Flt) ("toString", Nil, Flt :: MString) implements redirect ${ fltpt_to_string($0) }
    infix (Flt) ("mkString", Nil, Flt :: MString) implements redirect ${ fltpt_to_string($0) }
    infix (Flt) ("toFixPt", Nil, Flt :: Fix) implements redirect ${ fltpt_to_fixpt($0) }

    fimplicit (Tpes) ("sboolean_to_bit", Nil, SBoolean :: Bit) implements redirect ${ cbit($0) }
    fimplicit (Tpes) ("sint_to_fixpt", Nil, SInt :: Fix) implements redirect ${ fixPt($0) }
    fimplicit (Tpes) ("sfloat_to_fltpt", Nil, SFloat :: Flt) implements redirect ${ fltPt($0) }


    // --- Scala Backend
    impl (boolean_to_bit) (codegen($cala, ${ $0 }))
    impl (bit_to_string) (codegen($cala, ${ $0.toString }))

    impl (numeric_to_fixpt) (codegen($cala, ${ $0.toLong }))
    impl (fixpt_to_string) (codegen($cala, ${ $0.toString }))
    impl (fixpt_to_fltpt) (codegen($cala, ${ $0.toDouble }))

    impl (numeric_to_fltpt) (codegen($cala, ${ $0.toDouble }))
    impl (fltpt_to_string) (codegen($cala, ${ $0.toString }))
    impl (fltpt_to_fixpt) (codegen($cala, ${ $0.toLong }))

    // --- Dot Backend
    impl (fixpt_to_fltpt) (codegen(dot, ${
			$sym [ label="fix2flt" ]
			$0 -> $sym
			$0 [style="invisible" height=0 size=0 margin=0 label=""]
		}))
    impl (fltpt_to_fixpt) (codegen(dot, ${
			$sym [ label="flt2fix" ]
			$0 -> $sym
			$0 [style="invisible" height=0 size=0 margin=0 label=""]
		}))

    /*val fix_to_string = internal (FixPt) ("fix_to_string", Nil, (("sign", FixPt), ("int", FixPt), ("frac", FixPt)) :: MString)
    impl (fix_to_string) (codegen ($cala, ${
      (if ($sign == 0L) "" else "-") + $int + "." + $frac
    }))*/

    /*	val int = $0
			val intPrec = 31
			val fracPrec = 0
			if (int > scala.math.pow(2,intPrec))
				throw new Exception("Integer precision not enough to hold integer value " + int)
			int.toLong
		}))*/

		/*direct (TpeOps) ("FixPt", Nil, (MFloat,MInt,MInt) :: FixPt) implements codegen ($cala, ${
			val flt = $0
			val intPrec = $1
			val fracPrec = $2
			if (scala.math.round(flt) > scala.math.pow(2,intPrec))
				throw new Exception("Integer precision not enough to hold integer value " + scala.math.round(flt))
			scala.math.round(flt * scala.math.pow(2,fracPrec)).toLong
		})*/

		/*val FixPtOps = withTpe(FixPt)
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
			//TODO: Fix this. right now always print 0.0
			infix ("mkString") (Nil:: MString) implements composite ${
				fix_to_string($self.getSign, $self.getInt, $self.getFrac)
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
		}*/



		/*TODO: these should be for internal use only. No codegen rules */
		/*val IntOps = withTpe (MInt)
		IntOps {
			val int2fxp = infix ("toFixPt") (Nil::FixPt)
			impl (int2fxp) (codegen($cala, ${ $self.toLong } ))

			infix ("+") (MInt :: MInt) implements codegen ($cala, ${ $self + $1 })
			infix ("-") (MInt :: MInt) implements codegen ($cala, ${ $self - $1 })
			infix ("*") (MInt :: MInt) implements codegen ($cala, ${ $self * $1 })
			infix ("/") (MInt :: MInt) implements codegen ($cala, ${ $self / $1 })

			infix ("==") (MInt :: MBoolean) implements codegen ($cala, ${ $self == $1 })
			infix ("<") (MInt :: MBoolean) implements codegen ($cala, ${ $self < $1 })
		}*/

		//fimplicit (TpeOps) ("fixpt_to_float", Nil, FixPt::MFloat) implements composite ${ $0.toFloat }
		//fimplicit (TpeOps) ("float_to_fixpt", Nil, MFloat::FixPt) implements composite ${ $0.toFixPt }
		//fimplicit (TpeOps) ("int_to_fixpt", Nil, MInt::FixPt) implements composite ${ $0.toFixPt }
		//fimplicit (TpeOps) ("sint_to_fixpt", Nil, SInt::FixPt) implements composite ${ unit($0).toFixPt }
	}
}
