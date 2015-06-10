package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

/* Defines the Arith type class for scalars, vectors, and matrices */
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
    infix (Arith) ("one", T, T :: T)
    infix (Arith) ("zero", T, T :: T)
    infix (Arith) ("empty", T, Nil :: T)
    infix (Arith) ("+", T, (T,T) :: T)
    infix (Arith) ("-", T, (T,T) :: T)
    infix (Arith) ("*", T, (T,T) :: T)
    infix (Arith) ("/", T, (T,T) :: T)
    infix (Arith) ("negate", T, T :: T)
    infix (Arith) ("square", T, T :: T)
    infix (Arith) ("abs", T, T :: T)
    infix (Arith) ("ceil", T, T :: T)
    infix (Arith) ("floor", T, T :: T)
    // These really make no sense here, but keeping them in since the old version had them
    infix (Arith) ("exp", T, T :: T)
    infix (Arith) ("log", T, T :: T)

    // Basic functions (moved from BasicMath)
    val BasicArith = grp("BasicArith")
    direct (BasicArith) ("square", T, T :: T, TArith(T)) implements composite ${ $0.square }
    direct (BasicArith) ("abs", T, T :: T, TArith(T)) implements composite ${ $0.abs }
    direct (BasicArith) ("ceil", T, T :: T, TArith(T)) implements composite ${ $0.ceil }
    direct (BasicArith) ("floor", T, T :: T, TArith(T)) implements composite ${ $0.floor }


    // primitive implementations
    val DoubleArith = tpeClassInst("ArithDouble", Nil, Arith(MDouble))
    infix (DoubleArith) ("one", Nil, MDouble :: MDouble) implements composite ${ unit(1.0) }
    infix (DoubleArith) ("zero", Nil, MDouble :: MDouble) implements composite ${ unit(0.0) }
    infix (DoubleArith) ("empty", Nil, Nil :: MDouble) implements composite ${ unit(0.0) } 
    infix (DoubleArith) ("+", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_plus($0,$1) } 
    infix (DoubleArith) ("-", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_minus($0,$1) }
    infix (DoubleArith) ("*", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_times($0,$1) }
    infix (DoubleArith) ("/", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_divide($0,$1) }
    infix (DoubleArith) ("negate", Nil, MDouble :: MDouble) implements composite ${ -1.0 * $0 } 
    infix (DoubleArith) ("square", Nil, MDouble :: MDouble) implements composite ${ $0 * $0 }
    infix (DoubleArith) ("abs", Nil, MDouble :: MDouble) implements composite ${ math_object_abs($0) }
    infix (DoubleArith) ("ceil", Nil, MDouble :: MDouble) implements composite ${ math_object_ceil($0) }
    infix (DoubleArith) ("floor", Nil, MDouble :: MDouble) implements composite ${ math_object_floor($0) }
    infix (DoubleArith) ("exp", Nil, MDouble :: MDouble) implements composite ${ math_object_exp($0) }
    infix (DoubleArith) ("log", Nil, MDouble :: MDouble) implements composite ${ math_object_log($0) }

    val FloatArith = tpeClassInst("ArithFloat", Nil, Arith(MFloat))
    infix (FloatArith) ("one", Nil, MFloat :: MFloat) implements composite ${ unit(1f) } 
    infix (FloatArith) ("zero", Nil, MFloat :: MFloat) implements composite ${ unit(0f) } 
    infix (FloatArith) ("empty", Nil, Nil :: MFloat) implements composite ${ unit(0f) }
    infix (FloatArith) ("+", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_plus($0,$1) }
    infix (FloatArith) ("-", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_minus($0,$1) }
    infix (FloatArith) ("*", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_times($0,$1) }
    infix (FloatArith) ("/", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_divide($0,$1) }
    infix (FloatArith) ("negate", Nil, MFloat :: MFloat) implements composite ${ -1.0f * $0 } 
    infix (FloatArith) ("square", Nil, MFloat :: MFloat) implements composite ${ $0 * $0 }
    infix (FloatArith) ("abs", Nil, MFloat :: MFloat) implements composite ${ math_object_abs($0) }
    infix (FloatArith) ("ceil", Nil, MFloat :: MFloat) implements composite ${ math_object_ceil($0.toDouble).toFloat }
    infix (FloatArith) ("floor", Nil, MFloat :: MFloat) implements composite ${ math_object_floor($0.toDouble).toFloat }
    infix (FloatArith) ("exp", Nil, MFloat :: MFloat) implements composite ${ math_object_exp($0).toFloat }
    infix (FloatArith) ("log", Nil, MFloat :: MFloat) implements composite ${ math_object_log($0).toFloat }


    val IntArith = tpeClassInst("ArithInt", Nil, Arith(MInt))
    infix (IntArith) ("one", Nil, MInt :: MInt) implements composite ${ unit(1) }
    infix (IntArith) ("zero", Nil, MInt :: MInt) implements composite ${ unit(0) }
    infix (IntArith) ("empty", Nil, Nil :: MInt) implements composite ${ unit(0) }
    infix (IntArith) ("+", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_plus($0,$1) }
    infix (IntArith) ("-", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_minus($0,$1) }
    infix (IntArith) ("*", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_times($0,$1) }
    infix (IntArith) ("/", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_divide($0,$1) }
    infix (IntArith) ("negate", Nil, MInt :: MInt) implements composite ${ -1 * $0 } 
    infix (IntArith) ("square", Nil, MInt :: MInt) implements composite ${ $0 * $0 }
    infix (IntArith) ("abs", Nil, MInt :: MInt) implements composite ${ math_object_abs($0) }
    infix (IntArith) ("ceil", Nil, MInt :: MInt) implements composite ${ $0 }
    infix (IntArith) ("floor", Nil, MInt :: MInt) implements composite ${ $0 }
    infix (IntArith) ("exp", Nil, MInt :: MInt) implements composite ${ math_object_exp($0).toInt }
    infix (IntArith) ("log", Nil, MInt :: MInt) implements composite ${ math_object_log($0).toInt }


    val LongArith = tpeClassInst("ArithLong", Nil, Arith(MLong))
    infix (LongArith) ("one", Nil, MLong :: MLong) implements composite ${ unit(1L) }
    infix (LongArith) ("zero", Nil, MLong :: MLong) implements composite ${ unit(0L) }
    infix (LongArith) ("empty", Nil, Nil :: MLong) implements composite ${ unit(0L) }
    infix (LongArith) ("+", Nil, (MLong,MLong) :: MLong) implements composite ${ forge_long_plus($0,$1) }
    infix (LongArith) ("-", Nil, (MLong,MLong) :: MLong) implements composite ${ forge_long_minus($0,$1) }
    infix (LongArith) ("*", Nil, (MLong,MLong) :: MLong) implements composite ${ forge_long_times($0,$1) }
    infix (LongArith) ("/", Nil, (MLong,MLong) :: MLong) implements composite ${ forge_long_divide($0,$1) }
    infix (LongArith) ("negate", Nil, MLong :: MLong) implements composite ${ unit(-1L) * $0 } 
    infix (LongArith) ("square", Nil, MLong :: MLong) implements composite ${ $0 * $0 }
    infix (LongArith) ("abs", Nil, MLong :: MLong) implements composite ${ math_object_abs($0) }
    infix (LongArith) ("ceil", Nil, MLong :: MLong) implements composite ${ $0 }
    infix (LongArith) ("floor", Nil, MLong :: MLong) implements composite ${ $0 }
    infix (LongArith) ("exp", Nil, MLong :: MLong) implements composite ${ math_object_exp($0).toLong }
    infix (LongArith) ("log", Nil, MLong :: MLong) implements composite ${ math_object_log($0).toLong }


    // tuples of ariths
    for (arity <- 2 until maxTuples) {
      val Tup = lookupTpe("Tup"+arity)
      val pars = (0 until arity).map(i => tpePar(('A'.toInt+i).toChar.toString) withBound TArith).toList
      val TupArith = tpeClassInst("ArithTup"+arity, pars, Arith(Tup))

      def tupArithStr(op: String) = "pack((" + pars.map(p => "implicitly[Arith["+p.name+"]]."+op).mkString(",") + "))"
      def tupArithFromSrcStr(op: String) = "pack((" + (1 to arity).map(i => "t._"+i+"."+op).mkString(",") + "))"
      def tupArithBinStr(op: String) = "pack((" + (1 to arity).map(i => "t1._"+i+op+"t2._"+i).mkString(",") + "))"

      infix (TupArith) ("one", pars, ("t", Tup) :: Tup) implements composite { tupArithFromSrcStr("one") }
      infix (TupArith) ("zero", pars, ("t", Tup) :: Tup) implements composite { tupArithFromSrcStr("zero") }
      infix (TupArith) ("empty", pars, Nil :: Tup) implements composite { tupArithStr("empty") }
      infix (TupArith) ("+", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupArithBinStr("+") }
      infix (TupArith) ("-", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupArithBinStr("-") }
      infix (TupArith) ("*", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupArithBinStr("*") }
      infix (TupArith) ("/", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupArithBinStr("/") }
      infix (TupArith) ("negate", pars, ("t",Tup) :: Tup) implements composite { tupArithFromSrcStr("negate") }
      infix (TupArith) ("square", pars, ("t",Tup) :: Tup) implements composite { tupArithFromSrcStr("square") } 
      infix (TupArith) ("abs", pars, ("t",Tup) :: Tup) implements composite { tupArithFromSrcStr("abs") }
      infix (TupArith) ("ceil", pars, ("t",Tup) :: Tup) implements composite { tupArithFromSrcStr("ceil") }
      infix (TupArith) ("floor", pars, ("t",Tup) :: Tup) implements composite { tupArithFromSrcStr("floor") }
    	infix (TupArith) ("exp", pars, ("t",Tup) :: Tup) implements composite { tupArithFromSrcStr("exp") }
    	infix (TupArith) ("log", pars, ("t",Tup) :: Tup) implements composite { tupArithFromSrcStr("log") }
    }

  }
}