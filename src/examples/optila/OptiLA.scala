package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiLADSLRunner extends ForgeApplicationRunner with OptiLADSL

trait OptiLADSL extends ForgeApplication
  with BasicMathOps with RandomOps with IOOps with ArithOps with StringableOps
  with VectorOps with DenseVectorOps with IndexVectorOps with DenseVectorViewOps
  with DenseMatrixOps {

  def dslName = "OptiLA"

  def specification() = {
    // our selection of Scala ops
    // we don't use Numeric or Fractional, since they are replaced by Arith
    importPrimitives()
    importMisc()
    importCasts()
    importOrdering()
    importStrings()
    importMath()
    importTuples()

    // OptiLA types
    // declare all tpes first, so that they are available to all ops (similar to Delite)
    val DenseVector = tpe("DenseVector", tpePar("T"))
    val DenseVectorView = tpe("DenseVectorView", tpePar("T"))
    val DenseMatrix = tpe("DenseMatrix", tpePar("T"))
    val IndexVector = tpe("IndexVector")

    // OptiLA ops
    // note that the order matters with respect to 'lookup' calls

    // sneak in a compiler-only range method
    val Range = tpe("Range")
    data(Range, ("start", MInt), ("end", MInt))
    compiler (Range) ("range_start", Nil, Range :: MInt) implements getter(0, "start")
    compiler (Range) ("range_end", Nil, Range :: MInt) implements getter(0, "end")

    noInfixList :::= List("infix_foreach")
    compiler (Range) ("infix_until", Nil, (MInt,MInt) :: Range) implements allocates(Range, quotedArg(0), quotedArg(1))

    // this is a little convoluted unfortunately (because of the restriction on passing structs to codegen nodes)
    compiler (Range) ("infix_foreach", Nil, (Range, MInt ==> MUnit) :: MUnit) implements composite ${ range_foreach(range_start($0), range_end($0), $1) }
    compiler (Range) ("range_foreach", Nil, (("start",MInt),("end",MInt),("func",MInt ==> MUnit)) :: MUnit) implements codegen($cala, ${
      var i = $start
      while (i < $end) {
        $b[func](i)
        i += 1
      }
    })

    importBasicMathOps()
    importRandomOps()
    importArithOps()
    importStringableOps()

    // override default string formatting
    // numericPrecision is a global defined in extern
    val strConcatWithNumerics = {
      val a = quotedArg(0)
      val b = quotedArg(1)
      val f1 = "(\"%.\"+Global.numericPrecision+\"f\").format("+a+")" // can't escape quotes inside string interpolation scope
      val f2 = "(\"%.\"+Global.numericPrecision+\"f\").format("+b+")"
s"""
val a1 = if ($a.isInstanceOf[Double] || $a.isInstanceOf[Float]) $f1 else $a.toString
val b1 = if ($b.isInstanceOf[Double] || $b.isInstanceOf[Float]) $f2 else $b.toString
a1+b1
"""
    }
    // the ones that matter are the first that resolve to a unique tpe combination
    impl (lookupOverloaded("FString","+",0)) (codegen($cala, strConcatWithNumerics))
    impl (lookupOverloaded("FString","+",6)) (codegen($cala, strConcatWithNumerics))
    impl (lookupOverloaded("FString","+",11)) (codegen($cala, strConcatWithNumerics))

    importIndexVectorOps()
    importDenseVectorViewOps()
    importDenseVectorOps()
    importDenseMatrixOps()
    importIOOps()

    // native libs
    extern(grp("BLAS"))
  }
}
