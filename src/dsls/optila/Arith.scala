package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Defines the Arith type class for scalars, vectors, and matrices
 */
trait ArithOps {
  this: OptiLADSL =>

  object TArith extends TypeClassSignature {
    def name = "Arith"
    def prefix = "_a"
    def wrapper = Some("atype")
  }

  def importArithOps() {
    val T = tpePar("T")
    val A = tpePar("A")
    val B = tpePar("B")

    val Arith = tpeClass("Arith", TArith, T)

    // Arith type class interface
    infix (Arith) ("zero", T, T :: T)
    infix (Arith) ("empty", T, Nil :: T)
    infix (Arith) ("+", T, (T,T) :: T)
    infix (Arith) ("-", T, (T,T) :: T)
    infix (Arith) ("*", T, (T,T) :: T)
    infix (Arith) ("/", T, (T,T) :: T)
    infix (Arith) ("abs", T, T :: T)
    infix (Arith) ("exp", T, T :: T)
    infix (Arith) ("log", T, T :: T)

    // primitive implementations
    val DoubleArith = tpeClassInst("ArithDouble", Nil, Arith(MDouble))
    infix (DoubleArith) ("zero", Nil, MDouble :: MDouble) implements composite ${ unit(0.0) }
    infix (DoubleArith) ("empty", Nil, Nil :: MDouble) implements composite ${ unit(0.0) }
    infix (DoubleArith) ("+", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_plus($0,$1) }
    infix (DoubleArith) ("-", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_minus($0,$1) }
    infix (DoubleArith) ("*", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_times($0,$1) }
    infix (DoubleArith) ("/", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_divide($0,$1) }
    infix (DoubleArith) ("abs", Nil, MDouble :: MDouble) implements composite ${ math_object_abs($0) }
    infix (DoubleArith) ("exp", Nil, MDouble :: MDouble) implements composite ${ math_object_exp($0) }
    infix (DoubleArith) ("log", Nil, MDouble :: MDouble) implements composite ${ math_object_log($0) }

    val FloatArith = tpeClassInst("ArithFloat", Nil, Arith(MFloat))
    infix (FloatArith) ("zero", Nil, MFloat :: MFloat) implements composite ${ unit(0f) }
    infix (FloatArith) ("empty", Nil, Nil :: MFloat) implements composite ${ unit(0f) }
    infix (FloatArith) ("+", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_plus($0,$1) }
    infix (FloatArith) ("-", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_minus($0,$1) }
    infix (FloatArith) ("*", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_times($0,$1) }
    infix (FloatArith) ("/", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_divide($0,$1) }
    infix (FloatArith) ("abs", Nil, MFloat :: MFloat) implements composite ${ math_object_abs($0).toFloat }
    infix (FloatArith) ("exp", Nil, MFloat :: MFloat) implements composite ${ math_object_exp($0).toFloat }
    infix (FloatArith) ("log", Nil, MFloat :: MFloat) implements composite ${ math_object_log($0).toFloat }

    val IntArith = tpeClassInst("ArithInt", Nil, Arith(MInt))
    infix (IntArith) ("zero", Nil, MInt :: MInt) implements composite ${ unit(0) }
    infix (IntArith) ("empty", Nil, Nil :: MInt) implements composite ${ unit(0) }
    infix (IntArith) ("+", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_plus($0,$1) }
    infix (IntArith) ("-", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_minus($0,$1) }
    infix (IntArith) ("*", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_times($0,$1) }
    infix (IntArith) ("/", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_divide($0,$1) }
    infix (IntArith) ("abs", Nil, MInt :: MInt) implements composite ${ math_object_abs($0).toInt }
    infix (IntArith) ("exp", Nil, MInt :: MInt) implements composite ${ math_object_exp($0).toInt }
    infix (IntArith) ("log", Nil, MInt :: MInt) implements composite ${ math_object_log($0).toInt }
  }
}
