package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait BasicMathOps {
  this: OptiLADSL =>

  def importBasicMathOps() {
    val Math = grp("BasicMath")
    val Prim = lookupGrp("Primitive")

	  val T = tpePar("T")

    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseMatrixView = lookupTpe("DenseMatrixView")
    val SparseVector = lookupTpe("SparseVector")
    val SparseMatrix = lookupTpe("SparseMatrix")

    // -- aliases for Math._
    direct (Math) ("abs", Nil, MDouble :: MDouble) implements redirect ${ Math.abs($0) }
    direct (Math) ("exp", Nil, MDouble :: MDouble) implements redirect ${ Math.exp($0) }
    direct (Math) ("log", Nil, MDouble :: MDouble) implements redirect ${ Math.log($0) }
    direct (Math) ("log10", Nil, MDouble :: MDouble) implements redirect ${ Math.log10($0) }
    direct (Math) ("square", Nil, MDouble :: MDouble) implements redirect ${ $0*$0 }
    direct (Math) ("sqrt", Nil, MDouble :: MDouble) implements redirect ${ Math.sqrt($0) }
    direct (Math) ("ceil", Nil, MDouble :: MInt) implements redirect ${ Math.ceil($0).toInt }
    direct (Math) ("floor", Nil, MDouble :: MInt) implements redirect ${ Math.floor($0).toInt }
    direct (Math) ("round", Nil, MDouble :: MInt) implements redirect ${ Math.round($0).toInt }
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
    infix (Math)  ("~^", Nil, (MDouble,MInt) :: MDouble) implements redirect ${ Math.pow($0,$1) } // wrong precedence without ~

    val max = direct (Math) ("max", T withBound TNumeric, (T,T) :: T) implements codegen($cala, ${ implicitly[Numeric[$t[T]]].max($0,$1) })
    val min = direct (Math) ("min", T withBound TNumeric, (T,T) :: T) implements codegen($cala, ${ implicitly[Numeric[$t[T]]].min($0,$1) })
    for (g <- List(cuda, cpp)) {
    	impl (max) (codegen(g, "(" + quotedArg(0) + ">" + quotedArg(1) + ")?" + quotedArg(0) + ":" + quotedArg(1)))
    	impl (min) (codegen(g, "(" + quotedArg(0) + "<" + quotedArg(1) + ")?" + quotedArg(0) + ":" + quotedArg(1)))
  	}


    // -- distance

    // don't kick in when polymorphic, for unknown reasons
    // fimplicit (DenseVector) ("dist", T, (DenseVector(T),DenseVector(T)) :: MDouble, ("conv", T ==> MDouble)) implements composite ${ sum(abs($0.toDouble - $1.toDouble)) }
    // fimplicit (DenseMatrix) ("dist", T, (DenseMatrix(T),DenseMatrix(T)) :: MDouble, ("conv", T ==> MDouble)) implements composite ${ sum(abs($0.toDouble - $1.toDouble)) }
    fimplicit (Prim) ("dist", Nil, (MInt,MInt) :: MDouble) implements composite ${ abs($0-$1) }
    fimplicit (Prim) ("dist", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ abs($0-$1) }

    // metrics: default is ABS
    val DMetric = tpe("DistanceMetric", stage = compile)
    identifier (DMetric) ("ABS")
    identifier (DMetric) ("SQUARE")
    identifier (DMetric) ("EUC")

    for (TP <- List(DenseVector,DenseVectorView,DenseMatrix,SparseVector)) {
      fimplicit (TP) ("dist", Nil, (TP(MDouble),TP(MDouble)) :: MDouble) implements redirect ${ dist($0,$1,ABS) }

      direct (TP) ("dist", Nil, (TP(MDouble),TP(MDouble),DMetric) :: MDouble) implements composite ${
        $2 match {
          case ABS => sum(abs($0 - $1))
          case SQUARE => sum(square($0 - $1))
          case EUC => sqrt(sum(square($0 - $1)))
        }
      }
    }


    // -- norms

    // norm ids: default is L2
    val NormId = tpe("NormId", stage = compile)
    identifier (NormId) ("L1")
    identifier (NormId) ("L2")
    identifier (NormId) ("FRO")

    for (TP <- List(DenseVector,DenseVectorView)) {
      direct (TP) ("norm", Nil, TP(MDouble) :: MDouble) implements redirect ${ norm($0,L2) }

      direct (TP) ("norm", Nil, (TP(MDouble),NormId) :: MDouble) implements composite ${
        $1 match {
          case L1 => sum(abs($0))
          case L2 => sqrt(sum(square($0)))
          case FRO => norm($0, L2)
        }
      }
    }

    for (TP <- List(DenseMatrix,DenseMatrixView)) {
      direct (TP) ("norm", Nil, TP(MDouble) :: MDouble) implements redirect ${ norm($0,L2) }

      direct (TP) ("norm", Nil, (TP(MDouble),NormId) :: MDouble) implements composite ${
        $1 match {
          case L1 => max($0.mapColsToVector(c => norm(c, L1)))
          case L2 => fatal("not implemented")
          case FRO => sqrt(sum($0.mapColsToVector(c => sum(square(c)))))
        }
      }
    }


    // -- other math ops
    direct (Math) ("sigmoid", Nil, MDouble :: MDouble) implements composite ${ 1.0 / (1.0 + exp(-$0)) }


    // -- pdfs
    direct (Math) ("normpdf", Nil, (("x",MDouble),("mu",MDouble),("sigma",MDouble)) :: MDouble) implements composite ${
      (1.0 / (sigma * sqrt(2.0*Pi))) * exp(-((x-mu)*(x-mu)) / (2.0*sigma*sigma))
    }

    direct (Math) ("normpdf", Nil, (("x",DenseVector(MDouble)),("mu",DenseVector(MDouble)),("sigma",DenseVector(MDouble))) :: DenseVector(MDouble)) implements composite ${
      (0::x.length) { i => normpdf(x(i), mu(i), sigma(i)) }
    }

    direct (Math) ("normpdf", Nil, (("x",DenseMatrix(MDouble)),("mu",DenseMatrix(MDouble)),("sigma",DenseMatrix(MDouble))) :: DenseMatrix(MDouble)) implements composite ${
      (0::x.numRows, 0::x.numCols) { (i,j) => normpdf(x(i,j), mu(i,j), sigma(i,j)) }
    }

    for (Col <- List(DenseVector(MDouble),DenseMatrix(MDouble))) {
      direct (Math) ("normpdf", Nil, (("x",Col),("mu",MDouble),("sigma",Col)) :: Col) implements redirect ${
        x.zip(sigma) { (a,b) => normpdf(a, mu, b) }
      }
      direct (Math) ("normpdf", Nil, (("x",Col),("mu",Col),("sigma",MDouble)) :: Col) implements redirect ${
        x.zip(mu) { (a,b) => normpdf(a, b, sigma) }
      }
      direct (Math) ("normpdf", Nil, (("x",Col),("mu",MDouble),("sigma",MDouble)) :: Col) implements redirect ${
        x.map { e => normpdf(e, mu, sigma) }
      }
    }


    // -- aliases for instance math ops

    for (V <- List(DenseVector(T),DenseVectorView(T),IndexVector,DenseMatrix(T))) {
      val A = if (V != IndexVector) List(TArith(T)) else Nil
      val O = if (V != IndexVector) List(TOrdering(T)) else Nil
      val C = if (V != IndexVector) List(arg("conv",T ==> MDouble)) else Nil
      val P = if (V != IndexVector) List(T) else Nil
      val R1 = if (V == DenseMatrix(T)) DenseMatrix(T) else if (V == IndexVector) DenseVector(MInt) else DenseVector(T)
      val R2 = if (V != IndexVector) T else MInt
      val size = if (V == DenseMatrix(T)) "size" else "length"

      direct (Math) ("abs", P, V :: R1, A) implements redirect ${ $0.abs }
      direct (Math) ("exp", P, V :: R1, A) implements redirect ${ $0.exp }
      direct (Math) ("log", P, V :: R1, A) implements redirect ${ $0.log }
      direct (Math) ("square", P, V :: R1, A) implements composite ${ $0.map(e => e*e) }
      direct (Math) ("sum", P, V :: R2, A) implements redirect ${ $0.sum }
      direct (Math) ("prod", P, V :: R2, A) implements redirect ${ $0.prod }
      direct (Math) ("mean", P, V :: MDouble, C) implements redirect ${ $0.mean }
      direct (Math) ("variance", P, V :: MDouble, C) implements composite ${
        val dbls = $0.toDouble
        val avg = mean(dbls)
        val diffs = dbls map { e => square(e-avg) }
        sum(diffs) / (diffs.\$size-1.0)
        // mean(diffs) // 2nd moment around mean
      }
      direct (Math) ("stddev", P, V :: MDouble, C) implements composite ${ sqrt(variance($0)) }
      direct (Math) ("min", P, V :: R2, O ::: A) implements redirect ${ $0.min }
      direct (Math) ("max", P, V :: R2, O ::: A) implements redirect ${ $0.max }

      // only DenseVector has sort, and therefore median, defined right now
      if (V == DenseVector(T)) {
     	  direct (Math) ("median", T, V :: T, (TNumeric(T),TOrdering(T))) implements redirect ${ $0.median }
      }
    }

    for (V <- List(SparseVector(T), SparseMatrix(T))) {
      val R = if (V == SparseMatrix(T)) SparseMatrix(T) else SparseVector(T)
      direct (Math) ("abs", T, V :: R, TArith(T)) implements redirect ${ $0.abs }
      direct (Math) ("square", T, V :: R, TArith(T)) implements composite ${ $0.mapnz(e => e*e) }
      direct (Math) ("sum", T, V :: T, TArith(T)) implements redirect ${ $0.sum }
      direct (Math) ("mean", T, V :: MDouble, ("conv",T ==> MDouble)) implements redirect ${ $0.mean }
      direct (Math) ("min", T, V :: T, (TOrdering(T), TArith(T))) implements redirect ${ $0.min }
      direct (Math) ("max", T, V :: T, (TOrdering(T), TArith(T))) implements redirect ${ $0.max }
    }


    // -- math on sequences
    // can't have a sequence-based sum, because it makes sum(0,n) { i => ... } ambiguous for int arguments
    // direct (Math) ("sum", T, varArgs(T) :: T, TArith(T)) implements composite ${ densevector_fromarray(array_fromseq($0),true).sum }
    direct (Math) ("mean", T, varArgs(T) :: MDouble, ("conv",T ==> MDouble)) implements composite ${ densevector_fromarray(array_fromseq($0),true).mean }
    direct (Math) ("min", T, varArgs(T) :: T, (TOrdering(T), TArith(T))) implements composite ${ densevector_fromarray(array_fromseq($0),true).min }
    direct (Math) ("max", T, varArgs(T) :: T, (TOrdering(T), TArith(T))) implements composite ${ densevector_fromarray(array_fromseq($0),true).max }
    direct (Math) ("median", T, varArgs(T) :: T, (TNumeric(T),TOrdering(T))) implements composite ${ densevector_fromarray(array_fromseq($0),true).median }
  }
}
