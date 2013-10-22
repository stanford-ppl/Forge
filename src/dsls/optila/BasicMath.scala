package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait BasicMathOps {
  this: OptiLADSL =>

  def importBasicMathOps() {
    val Math = grp("BasicMath")
	  val T = tpePar("T")

    // -- aliases for Math._
    direct (Math) ("abs", Nil, MDouble :: MDouble) implements redirect ${ Math.abs($0) }
    direct (Math) ("exp", Nil, MDouble :: MDouble) implements redirect ${ Math.exp($0) }
    direct (Math) ("log", Nil, MDouble :: MDouble) implements redirect ${ Math.log($0) }
    direct (Math) ("log10", Nil, MDouble :: MDouble) implements redirect ${ Math.log10($0) }
    direct (Math) ("sqrt", Nil, MDouble :: MDouble) implements redirect ${ Math.sqrt($0) }
    direct (Math) ("ceil", Nil, MDouble :: MInt) implements redirect ${ Math.ceil($0).toInt }
    direct (Math) ("floor", Nil, MDouble :: MInt) implements redirect ${ Math.floor($0).toInt }
    direct (Math) ("sin", Nil, MDouble :: MDouble) implements redirect ${ Math.sin($0) }
    direct (Math) ("sinh", Nil, MDouble :: MDouble) implements redirect ${ Math.sinh($0) }
    direct (Math) ("asin", Nil, MDouble :: MDouble) implements redirect ${ Math.asin($0) }
    direct (Math) ("cos", Nil, MDouble :: MDouble) implements redirect ${ Math.cos($0) }
    direct (Math) ("cosh", Nil, MDouble :: MDouble) implements redirect ${ Math.cosh($0) }
    direct (Math) ("acos", Nil, MDouble :: MDouble) implements redirect ${ Math.acos($0) }
    direct (Math) ("tan", Nil, MDouble :: MDouble) implements redirect ${ Math.tan($0) }
    direct (Math) ("tanh", Nil, MDouble :: MDouble) implements redirect ${ Math.tanh($0) }
    direct (Math) ("atan", Nil, MDouble :: MDouble) implements redirect ${ Math.atan($0) }
    direct (Math) ("atan2", Nil, (MDouble,MDouble) :: MDouble) implements redirect ${ Math.atan2($0,$1) }
    direct (Math) ("pow", Nil, (MDouble,MDouble) :: MDouble) implements redirect ${ Math.pow($0,$1) }
    val max = direct (Math) ("max", T withBound TNumeric, (T,T) :: T) implements codegen($cala, ${ implicitly[Numeric[$t[T]]].max($0,$1) })
    val min = direct (Math) ("min", T withBound TNumeric, (T,T) :: T) implements codegen($cala, ${ implicitly[Numeric[$t[T]]].min($0,$1) })
    for (g <- List(cuda, cpp)) {
    	impl (max) (codegen(g, "(" + quotedArg(0) + ">" + quotedArg(1) + ")?" + quotedArg(0) + ":" + quotedArg(1)))
    	impl (min) (codegen(g, "(" + quotedArg(0) + "<" + quotedArg(1) + ")?" + quotedArg(0) + ":" + quotedArg(1)))
  	}

    // -- other math ops
    direct (Math) ("sigmoid", Nil, MDouble :: MDouble) implements composite ${ 1.0 / (1.0 + exp(-$0)) }

    // -- aliases for instance math ops
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")

    for (V <- List(DenseVector(T),DenseVectorView(T),IndexVector,DenseMatrix(T))) {
      val A = if (V != IndexVector) List(TArith(T)) else Nil
      val O = if (V != IndexVector) List(TOrdering(T)) else Nil
      val C = if (V != IndexVector) List(arg("conv",T ==> MDouble)) else Nil
      val P = if (V != IndexVector) List(T) else Nil
      val R1 = if (V == DenseMatrix(T)) DenseMatrix(T) else if (V == IndexVector) DenseVector(MInt) else DenseVector(T)
      val R2 = if (V != IndexVector) T else MInt

      direct (Math) ("abs", P, V :: R1, A) implements redirect ${ $0.abs }
      direct (Math) ("exp", P, V :: R1, A) implements redirect ${ $0.exp }
      direct (Math) ("log", P, V :: R1, A) implements redirect ${ $0.log }
      direct (Math) ("square", P, V :: R1, A) implements composite ${ $0.map(e => e*e) }
      direct (Math) ("sum", P, V :: R2, A) implements redirect ${ $0.sum }
      direct (Math) ("mean", P, V :: MDouble, C) implements redirect ${ $0.mean }
      direct (Math) ("min", P, V :: R2, O) implements redirect ${ $0.min }
      direct (Math) ("max", P, V :: R2, O) implements redirect ${ $0.max }

      // only DenseVector has sort, and therefore median, defined right now
      if (V == DenseVector(T)) {
     	  direct (Math) ("median", T, V :: T, (TNumeric(T),TOrdering(T))) implements redirect ${ $0.median }
      }
    }

    // -- math on sequences
    // can't have a sequence-based sum, because it makes sum(0,n) { i => ... } ambiguous for int arguments
    // direct (Math) ("sum", T, varArgs(T) :: T, TArith(T)) implements composite ${ densevector_fromarray(array_fromseq($0),true).sum }
    direct (Math) ("mean", T, varArgs(T) :: MDouble, ("conv",T ==> MDouble)) implements composite ${ densevector_fromarray(array_fromseq($0),true).mean }
    direct (Math) ("min", T, varArgs(T) :: T, TOrdering(T)) implements composite ${ densevector_fromarray(array_fromseq($0),true).min }
    direct (Math) ("max", T, varArgs(T) :: T, TOrdering(T)) implements composite ${ densevector_fromarray(array_fromseq($0),true).max }
    direct (Math) ("median", T, varArgs(T) :: T, (TNumeric(T),TOrdering(T))) implements composite ${ densevector_fromarray(array_fromseq($0),true).median }
  }
}
