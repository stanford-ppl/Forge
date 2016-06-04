package ppl.dsl.forge
package dsls
package dhdl
import scala.reflect.SourceContext

@dsl
trait DHDLTypes {
  this: DHDLDSL =>

	def importDHDLTypes() {
    val S2 = tpePar("S2")
    val I2 = tpePar("I2")
    val F2 = tpePar("F2")
    val G2 = tpePar("G2")
    val E2 = tpePar("E2")

    val R = tpePar("R")
		val T = tpePar("T", stage = compile)

    val Bit   = lookupTpe("Bit")
		val FixPt = lookupTpe("FixPt")
    val FltPt = lookupTpe("FltPt")

    val Z = lookupTpe("B0", compile)

    val Tpes = grp("Tpes")
    val Lifts = grp("ConstLifts")

    // Want Manifest[S], not Manifest[Rep[S]]
    val SS = tpePar("S",stage=compile)
    val II = tpePar("I",stage=compile)
    val FF = tpePar("F",stage=compile)
    val GG = tpePar("G",stage=compile)
    val EE = tpePar("E",stage=compile)



    // --- Nodes
    // Bordering on a ridiculous numbers of type parameters here
    // Somewhat concerning since each type parameter means an implicit parameter, but maybe it's ok

    val boolean_to_bit = internal (Lifts) ("constBit", Nil, SBoolean :: Bit)
    val const_to_fixpt = internal (Lifts) ("constFixPt", (T,S,I,F), (T, SManifest(SS), SManifest(II), SManifest(FF)) :: FixPt(S,I,F), TNumeric(T))
    val const_to_fltpt = internal (Lifts) ("constFltPt", (T,G,E), (T, SManifest(GG), SManifest(EE)) :: FltPt(G,E), TNumeric(T))

    impl (const_to_fixpt) (codegen(cpp, ${
        $0
    }))


    val bit_to_string = direct (Tpes) ("bit_to_string", Nil, Bit :: MString)

    // Include Manifests to avoid CSE issues
    val string_to_fixpt = direct (Tpes) ("string_to_fixpt", (S,I,F), MString :: FixPt(S,I,F))
    val fixpt_to_string = direct (Tpes) ("fixpt_to_string", (S,I,F), FixPt(S,I,F) :: MString)
    val fixpt_to_fltpt = internal (Tpes) ("fixpt_to_fltpt", (S,I,F,G,E), FixPt(S,I,F) :: FltPt(G,E))
    val convert_fixpt = internal (Tpes) ("convert_fixpt", (S,I,F,S2,I2,F2), FixPt(S,I,F) :: FixPt(S2,I2,F2))

    // Include Manifests to avoid CSE issues
    val string_to_fltpt = direct (Tpes) ("string_to_fltpt", (G,E), MString :: FltPt(G,E))
    val fltpt_to_string = direct (Tpes) ("fltpt_to_string", (G,E), FltPt(G,E) :: MString)
    val fltpt_to_fixpt = internal (Tpes) ("fltpt_to_fixpt", (G,E,S,I,F), FltPt(G,E) :: FixPt(S,I,F))
    val convert_fltpt = internal (Tpes) ("convert_fltpt", (G,E,G2,E2), FltPt(G,E) :: FltPt(G2,E2))

    // For testing / compatibility with rest of Delite and LMS
    val fix_to_rep_int = direct (Tpes) ("fix_to_int", (S,I), FixPt(S,I,Z) :: MInt)
    val rep_int_to_fix = direct (Tpes) ("int_to_fix", (S,I), MInt :: FixPt(S,I,Z))
    val bit_to_bool    = direct (Tpes) ("bit_to_bool", Nil, Bit :: MBoolean)

    // --- Internals
    /**@nodoc **/
    direct (Tpes) ("lift_to", (T,R), T :: R, TNumeric(T)) implements composite ${
      manifest[R] match {
        case mR if isFixPtType(mR) =>
          fixPt($0)(manifest[T], mR.typeArguments(0),mR.typeArguments(1),mR.typeArguments(2),implicitly[SourceContext],implicitly[Numeric[T]]).asInstanceOf[Rep[R]]

        case mR if isFltPtType(mR) =>
          fltPt($0)(manifest[T], mR.typeArguments(0),mR.typeArguments(1),implicitly[SourceContext],implicitly[Numeric[T]]).asInstanceOf[Rep[R]]

        case mR =>
          stageError("Don't know how to cast type " + manifest[T].runtimeClass.getSimpleName + " to " + mR.runtimeClass.getSimpleName)
      }
    }

    /** @nodoc **/
    direct (Tpes) ("cast_fixpt_to", (S,I,F,R), FixPt(S,I,F) :: R) implements composite ${
      manifest[R] match {
        case mR if isFixPtType(mR) =>
          convert_fixpt($0)(manifest[S],manifest[I],manifest[F],mR.typeArguments(0),mR.typeArguments(1),mR.typeArguments(2), implicitly[SourceContext]).asInstanceOf[Rep[R]]

        case mR if isFltPtType(mR) =>
          fixpt_to_fltpt($0)(manifest[S],manifest[I],manifest[F],mR.typeArguments(0),mR.typeArguments(1), implicitly[SourceContext]).asInstanceOf[Rep[R]]

        case mR =>
          stageError("Don't know how to cast fixed point value to " + mR.runtimeClass.getSimpleName + ".")
      }
    }

    /** @nodoc **/
    direct (Tpes) ("cast_fltpt_to", (G,E,R), FltPt(G,E) :: R) implements composite ${
      manifest[R] match {
        case mR if isFltPtType(mR) =>
          convert_fltpt($0)(manifest[G],manifest[E],mR.typeArguments(0),mR.typeArguments(1), implicitly[SourceContext]).asInstanceOf[Rep[R]]

        case mR if isFixPtType(mR) =>
          fltpt_to_fixpt($0)(manifest[G],manifest[E],mR.typeArguments(0),mR.typeArguments(1),mR.typeArguments(2), implicitly[SourceContext]).asInstanceOf[Rep[R]]

        case mR =>
          stageError("Don't know how to cast floating point value to " + mR.runtimeClass.getSimpleName + ".")
      }
    }

    /** @nodoc **/
    direct (Tpes) ("cast_string_to", R, MString :: R) implements composite ${
      manifest[R] match {
        case mR if isFltPtType(mR) =>
          string_to_fltpt($0)(mR.typeArguments(0),mR.typeArguments(1),implicitly[SourceContext]).asInstanceOf[Rep[R]]
        case mR if isFixPtType(mR) =>
          string_to_fixpt($0)(mR.typeArguments(0),mR.typeArguments(1),mR.typeArguments(2),implicitly[SourceContext]).asInstanceOf[Rep[R]]
        case mR =>
          stageError("Don't know how to cast string to " + mR.runtimeClass.getSimpleName + ".")
      }
    }

    // --- API
    // TODO: Add precision checking (can this number fit in that representation?)
    /** @nodoc **/
    direct (Tpes) ("bit", Nil, SBoolean :: Bit) implements composite ${ constBit($0) }

    // Primarily for internal use, but users can use this as well (but is less friendly syntax, e.g. fixPt[Int,...](value) )
    /** @nodoc **/
    direct (Tpes) ("fixPt", (T,S,I,F), T :: FixPt(S,I,F), TNumeric(T)) implements composite ${
      constFixPt[T,S,I,F]($0, manifest[S], manifest[I], manifest[F])
    }
    /** @nodoc **/
    direct (Tpes) ("fltPt", (T,G,E), T :: FltPt(G,E), TNumeric(T)) implements composite ${
      constFltPt[T,G,E]($0, manifest[G], manifest[E])
    }

    // TODO: Can probably change this to be an infix defined for all T:Numeric
    // Using "as" rather than "to" since "to" is already defined for int
    /** Stages this constant boolean as a Bit **/
    infix (Tpes) ("asBit", Nil, SBoolean :: Bit) implements composite ${ constBit($0) }
    /** Stages this constant Int as the specified type **/
    infix (Tpes) ("as", R, SInt :: R) implements redirect ${ lift_to[Int,R]($0) }
    /** Stages this constant Long as the specified type **/
    infix (Tpes) ("as", R, SLong :: R) implements redirect ${ lift_to[Long,R]($0) }
    /** Stages this constant Float as the specified type **/
    infix (Tpes) ("as", R, SFloat :: R) implements redirect ${ lift_to[Float,R]($0) }
    /** Stages this constant Double as the specified type **/
    infix (Tpes) ("as", R, SDouble :: R) implements redirect ${ lift_to[Double,R]($0) }
    /** Converts this String to the specified type **/
    infix (Tpes) ("to", R, MString :: R) implements redirect ${ cast_string_to[R]($0) }

    infix (Tpes) ("toString", Nil, Bit :: MString) implements redirect ${ bit_to_string($0) }
    infix (Tpes) ("mkString", Nil, Bit :: MString) implements redirect ${ bit_to_string($0) }

    infix (Tpes) ("toString", (S,I,F), FixPt(S,I,F) :: MString) implements redirect ${ fixpt_to_string($0) }
    infix (Tpes) ("mkString", (S,I,F), FixPt(S,I,F) :: MString) implements redirect ${ fixpt_to_string($0) }
    /** Creates a conversion of this value to the given type**/
    infix (Tpes) ("to", (S,I,F,R),  FixPt(S,I,F) :: R) implements redirect ${ cast_fixpt_to[S,I,F,R]($0) }

    infix (Tpes) ("toString", (G,E), FltPt(G,E) :: MString) implements redirect ${ fltpt_to_string($0) }
    infix (Tpes) ("mkString", (G,E), FltPt(G,E) :: MString) implements redirect ${ fltpt_to_string($0) }
    /** Creates a conversion of this value to the given type **/
    infix (Tpes) ("to", (G,E,R), FltPt(G,E) :: R) implements redirect ${ cast_fltpt_to[G,E,R]($0) }


    val SInt32 = lookupAlias("SInt")
    val Flt    = lookupAlias("Flt")

    // Needed for if-then-else and while (default requires Rep[Boolean] and overloading is tough in these cases)
    /** @nodoc **/
    fimplicit (Tpes) ("bit_to_boolean", Nil, Bit :: MBoolean) implements composite ${ bit_to_bool($0) }

    /** @nodoc **/
    fimplicit (Tpes) ("scala_boolean_to_bit", Nil, SBoolean :: Bit) implements redirect ${ bit($0) }
    /** @nodoc **/
    fimplicit (Tpes) ("scala_int_to_fixpt", Nil, SInt :: SInt32) implements redirect ${ fixPt[Int,Signed,B32,B0]($0) }
    /** @nodoc **/
    fimplicit (Tpes) ("stage_int_to_fixpt", Nil, MInt :: SInt32) implements redirect ${ int_to_fix[Signed,B32]($0) }  // Needed for params
    /** @nodoc **/
    fimplicit (Tpes) ("scala_float_to_fltpt", Nil, SFloat :: Flt) implements redirect ${ fltPt[Float,B24,B8]($0) }


    // --- Scala Backend
    impl (boolean_to_bit) (codegen($cala, ${ $0 }))
    impl (bit_to_string)  (codegen($cala, ${ $0.toString }))
    impl (bit_to_bool)    (codegen($cala, ${ $0 }))

    impl (string_to_fixpt) (codegen($cala, ${ FixedPoint[$t[S],$t[I],$t[F]]($0) }))
    impl (const_to_fixpt) (codegen($cala, ${ FixedPoint[$t[S],$t[I],$t[F]]($0.toString) }))
    impl (fixpt_to_string) (codegen($cala, ${ $0.toString }))
    impl (fixpt_to_fltpt) (codegen($cala, ${ $0.toFloatPoint[$t[G],$t[E]] }))
    impl (convert_fixpt) (codegen($cala, ${ $0.changeFormat[$t[S2],$t[I2],$t[F2]] }))
    impl (fix_to_rep_int) (codegen($cala, ${ $0.toInt }))
    impl (rep_int_to_fix) (codegen($cala, ${ FixedPoint[$t[S],$t[I],B0]($0) }))

    impl (string_to_fltpt) (codegen($cala, ${ FloatPoint[$t[G],$t[E]]($0) }))
    impl (const_to_fltpt) (codegen($cala, ${ FloatPoint[$t[G],$t[E]]($0.toString) }))
    impl (fltpt_to_string) (codegen($cala, ${ $0.toString }))
    impl (fltpt_to_fixpt) (codegen($cala, ${ $0.toFixedPoint[$t[S],$t[I],$t[F]] }))
    impl (convert_fltpt) (codegen($cala, ${ $0.changeFormat[$t[G2],$t[E2]] }))

    // --- C++ Backend
    impl (boolean_to_bit) (codegen(cpp, ${ $0 }))
    impl (bit_to_string)  (codegen(cpp, ${ $0.toString }))
    impl (bit_to_bool)    (codegen(cpp, ${ $0 }))

    impl (const_to_fixpt) (codegen(cpp, ${ FixedPoint[$t[S],$t[I],$t[F]]($0.toString) }))
    impl (fixpt_to_string) (codegen(cpp, ${ $0.toString }))
    impl (fixpt_to_fltpt) (codegen(cpp, ${ $0.toFloatPoint[$t[G],$t[E]] }))
    impl (convert_fixpt) (codegen(cpp, ${ $0.changeFormat[$t[S2],$t[I2],$t[F2]] }))
    impl (fix_to_rep_int) (codegen(cpp, ${
      (int32_t) $0
    }))
    impl (rep_int_to_fix) (codegen(cpp, ${
      @ val fixPtType = remap($0.tp)
      ($fixPtType) ($0)
    }))

    impl (const_to_fltpt) (codegen(cpp, ${ FloatPoint[$t[G],$t[E]]($0.toString) }))
    impl (fltpt_to_string) (codegen(cpp, ${ $0.toString }))
    impl (fltpt_to_fixpt) (codegen(cpp, ${ $0.toFixedPoint[$t[S],$t[I],$t[F]] }))
    impl (convert_fltpt) (codegen(cpp, ${ $0.changeFormat[$t[G2],$t[E2]] }))

    // --- MaxJ Backend
    impl (boolean_to_bit) (codegen(maxj, ${
      @ alwaysGen {
    		DFEVar $sym = constant.var( $0 );
      @ }
		}))
    impl (const_to_fixpt) (codegen(maxj, ${
      @ alwaysGen {
				@ val ts = tpstr(parOf(sym)) (sym.tp, implicitly[SourceContext])
				DFEVar $sym = constant.var( $ts, $0 );
      @ }
		}))
    impl (const_to_fltpt) (codegen(maxj, ${
      @ alwaysGen {
				@ val ts = tpstr(parOf(sym)) (sym.tp, implicitly[SourceContext])
      	DFEVar $sym = constant.var( $ts, $0 );
      @ }
		}))

    impl (fixpt_to_fltpt) (codegen(maxj, ${
			@ val ts = tpstr(parOf(sym)) (sym.tp, implicitly[SourceContext])
      DFEVar $sym = $0.cast( $ts );
		}))
    impl (convert_fixpt)  (codegen(maxj, ${
			//TODO: right way to do this?
			@ val ts = tpstr(parOf(sym)) (sym.tp, implicitly[SourceContext])
      DFEVar $sym = $0.cast( $ts );
		}))
    impl (rep_int_to_fix) (codegen(maxj, ${
			@ alwaysGen {
				@ val ts = tpstr(parOf(sym)) (sym.tp, implicitly[SourceContext])
				DFEVar $sym = constant.var( $ts, $0 );
			@ }
    }))

    impl (fltpt_to_fixpt) (codegen(maxj, ${
			@ val ts = tpstr(parOf(sym)) (sym.tp, implicitly[SourceContext])
      DFEVar $sym = $0.cast( $ts );
		}))
    impl (convert_fltpt)  (codegen(maxj, ${
			//TODO: right way to do this?
			@ val ts = tpstr(parOf(sym)) (sym.tp, implicitly[SourceContext])
      DFEVar $sym = $0.cast( $ts );
		}))

	}
}
