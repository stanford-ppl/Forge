package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiLADSLRunner extends ForgeApplicationRunner with OptiLADSL

trait OptiLADSL extends ForgeApplication
  with ArithOps with StringableOps
  with BasicMathOps with RandomOps with IOOps
  with VectorOps with DenseVectorOps with IndexVectorOps with DenseVectorViewOps
  with DenseMatrixOps
  with LinAlgOps {

  def dslName = "OptiLA"

  override def addREPLOverride = true

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
    val IndexWildcard = tpe("IndexWildcard", stage = compile)
    identifier (IndexWildcard) ("*")

    // OptiLA ops
    // note that the order matters with respect to 'lookup' calls

    // sneak in a compiler-only range method
    val Range = tpe("Range")
    data(Range, ("start", MInt), ("end", MInt))
    compiler (Range) ("range_start", Nil, Range :: MInt) implements getter(0, "start")
    compiler (Range) ("range_end", Nil, Range :: MInt) implements getter(0, "end")

    noInfixList :::= List("infix_foreach")
    compiler (Range) ("infix_until", Nil, (MInt,MInt) :: Range) implements allocates(Range, quotedArg(0), quotedArg(1))

    // infix_foreach must be compiler only both so that it is not used improperly and to not interfere with other codegen nodes in the library
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

    compiler (lookupGrp("FString")) ("optila_padspace", Nil, MString :: MString) implements composite ${
      if ($0.startsWith("-")) "  " + $0 else "   " + $0
    }

    importIndexVectorOps()
    importDenseVectorViewOps()
    importDenseVectorOps()
    importDenseMatrixOps()
    importVecMatConstructor()
    importIOOps()
    importLinAlgOps()

    // native libs
    extern(grp("BLAS"))
    extern(grp("LAPACK"))
  }

  def importVecMatConstructor() {
    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val T = tpePar("T")

    // vector constructor (0 :: end) { ... }
    noSourceContextList ::= "::" // surpress SourceContext implicit because it interferes with the 'apply' method being immediately callable
    infix (IndexVector) ("::", Nil, ((("end", MInt), ("start", MInt)) :: IndexVector)) implements composite ${ IndexVector($start, $end) }

    // should add apply directly to IndexVector, otherwise we have issues with ambiguous implicit conversions
    infix (IndexVector) ("apply", T, (IndexVector, MInt ==> T)  :: DenseVector(T)) implements composite ${ $0.map($1) }

    // matrix constructor (0::numRows,0::numCols) { ... }
    infix (IndexVector) ("apply", T, (CTuple2(IndexVector,IndexVector), (MInt,MInt) ==> T) :: DenseMatrix(T)) implements composite ${
      val (rowIndices,colIndices) = $0

      // can fuse with flat matrix loops
      val v = (0::(rowIndices.length*colIndices.length)).toDense
      val indices = densematrix_fromarray(densevector_raw_data(v),rowIndices.length,colIndices.length)
      indices map { i =>
        val rowIndex = rowIndices(i / colIndices.length)
        val colIndex = colIndices(i % colIndices.length)
        $1(rowIndex,colIndex)
      }

      // could fuse with nested matrix loops (loops over rowIndices), but not with loops directly over individual matrix elements -- like map!
      // it seems best for us to be consistent: matrix loops should either all be flat or all be nested. which one? should we use lowerings?
      // however, mutable version also supresses fusion due to unsafeImmutable...

      // val out = DenseMatrix[T](rowIndices.length,colIndices.length)
      // rowIndices foreach { i =>
      //   colIndices foreach { j =>
      //     out(i,j) = $1(i,j)
      //   }
      // }
      // out.unsafeImmutable
    }

    val IndexWildcard = lookupTpe("IndexWildcard", stage = compile)

    // specifying both DenseVector and DenseVectorViews as rhs arguments seems to make this ambiguous;
    // one alternative is to use an IsVector type class for the function return type, instead of overloading.
    // currently, we just let DenseVectorViews implicitly convert to DenseVector, which will cause overhead
    // in the lib implementation, but should fuse away in the Delite implementation.
    for (rhs <- List(DenseVector(T)/*, DenseVectorView(T))*/)) {
      infix (IndexVector) ("apply", T, (CTuple2(IndexVector,IndexWildcard), MInt ==> rhs) :: DenseMatrix(T)) implements composite ${
        val rowIndices = $0._1
        val first = $1(rowIndices(0)) // better be pure, because we ignore it to maintain normal loop size below
        val out = DenseMatrix[T](rowIndices.length,first.length)
        (0::rowIndices.length) foreach { i =>
          out(i) = $1(rowIndices(i))
        }
        out.unsafeImmutable
      }

      infix (IndexVector) ("apply", T, (CTuple2(IndexWildcard,IndexVector), MInt ==> rhs) :: DenseMatrix(T)) implements composite ${
        val colIndices = $0._2
        val first = $1(colIndices(0)) // better be pure, because we ignore it to maintain normal loop size below
        val out = DenseMatrix[T](first.length, colIndices.length)
        (0::colIndices.length) foreach { j =>
          out.updateCol(j, $1(colIndices(j)))
        }
        out.unsafeImmutable
      }
    }
  }
}
