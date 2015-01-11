package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiLADSLRunner extends ForgeApplicationRunner with OptiLADSL

trait OptiLADSL extends ForgeApplication
  with ArithOps with StringableOps with ShapeOps
  with BasicMathOps with RandomOps with IOOps
  with VectorOps with DenseVectorOps with IndexVectorOps with DenseVectorViewOps with SparseVectorOps with SparseVectorViewOps
  with MatrixOps with DenseMatrixOps with DenseMatrixViewOps with SparseMatrixOps
  with ComplexOps with LinAlgOps {

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
    importHashMap()
    importConcurrentHashMap()

    // OptiLA types
    // declare all tpes first, so that they are available to all ops (similar to Delite)
    val T = tpePar("T")
    val DenseVector = tpe("DenseVector", T)
    val DenseVectorView = tpe("DenseVectorView", T)
    val DenseMatrix = tpe("DenseMatrix", T)
    val DenseMatrixView = tpe("DenseMatrixView", T)
    val IndexVector = tpe("IndexVector")
    val IndexWildcard = tpe("IndexWildcard", stage = compile)
    identifier (IndexWildcard) ("*")
    val SparseVector = tpe("SparseVector", T)
    val SparseVectorView = tpe("SparseVectorView", T)
    val SparseMatrix = tpe("SparseMatrix", T)
    val SparseMatrixBuildable = tpe("SparseMatrixBuildable", T)

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
    val range_foreach = compiler (Range) ("range_foreach", Nil, (("start",MInt),("end",MInt),("func",MInt ==> MUnit)) :: MUnit)
    impl (range_foreach) (codegen($cala, ${
      var i = $start
      while (i < $end) {
        $b[func](i)
        i += 1
      }
    }))

    impl (range_foreach) (codegen(cpp, ${
      for(int i=$start ; i<$end ; i++) {
        $b[func](i)
      }
    }))

    importBasicMathOps()
    importRandomOps()
    importArithOps()
    importStringableOps()
    importComplexOps()

    // override default string formatting (numericPrecision is a global defined in extern)
    // we use "" + $a instead of $a.toString to avoid an NPE when explicitly calling toString inside the REPL
    val formatStr = {
      val a = quotedArg(0)
      val f = "(\"% .\"+Global.numericPrecision+\"g\")" // can't escape quotes inside string interpolation scope

s"""
def numericStr[A](x: A) = {
  val s = $f.format(x)
  val padPrefix = (Global.numericPrecision+6) - s.length
  if (padPrefix > 0) " "*padPrefix + s else s
}
if ($a.isInstanceOf[Double] || $a.isInstanceOf[Float]) numericStr($a) else ("" + $a)
"""
    }

    val fmt_str = direct (lookupGrp("FString")) ("optila_fmt_str", T, T :: MString)
    impl (fmt_str) (codegen($cala, formatStr))
    impl (fmt_str) (codegen(cpp, "convert_to_string<" +  unquotes("remapWithRef("+opArgPrefix+"0.tp)") + " >(" + quotedArg(0) + ")"))

    compiler (lookupGrp("FString")) ("optila_padspace", Nil, MString :: MString) implements composite ${
      "  " + $0
      // if ($0.startsWith("-")) "  " + $0 else "   " + $0
    }

    importIndexVectorOps()
    importDenseVectorViewOps()
    importDenseVectorOps()
    importDenseMatrixOps()
    importDenseMatrixViewOps()
    importSparseVectorOps()
    importSparseVectorViewOps()
    importSparseMatrixOps()
    importVecMatConstructor()
    importIOOps()
    importLinAlgOps()
    importShapeOps()

    // native libs
    extern(grp("BLAS"))
    extern(grp("LAPACK"))

    // rewrites
    extern(grp("Rewrite"), targets = Nil)
    extern(grp("Distributed"), targets = List($cala))
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
      val (rowIndices, colIndices) = $0

      // can fuse with flat matrix loops
      val size = rowIndices.length*colIndices.length
      val outData = array_fromfunction(size, i => {
        val (rowIndex, colIndex) = unpack(matrix_shapeindex(i, colIndices.length))
        $1(rowIndices(rowIndex),colIndices(colIndex))
      })
      densematrix_fromarray(outData,rowIndices.length,colIndices.length)

      // could fuse with nested matrix loops (loops over rowIndices), but not with loops directly over individual matrix elements -- like map!
      // it seems best for us to be consistent: matrix loops should either all be flat or all be nested. which one? should we use lowerings?
      // however, mutable version also supresses fusion due to unsafeImmutable and code motion due to effects...

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
        // Prefer immutable version and materialize vectors incrementally to enable garbage collection.
        val rowIndices = $0._1
        // val arrayIndices = array_buffer_fromfunction(rowIndices.length, i => rowIndices(i))
        // val arrayIndices = array_buffer_new_imm(array_fromfunction(rowIndices.length, i => rowIndices(i)), rowIndices.length)
        // val outData = array_buffer_flatmap[Int,T](arrayIndices, i => {
        //   val v = $1(i)
        //   array_buffer_new_imm(densevector_raw_data(v), v.length)
        // })

        val arrayIndices = array_fromfunction(rowIndices.length, i => rowIndices(i))
        val outData = array_flatmap[Int,T](arrayIndices, i => {
          val v = $1(i)
          array_fromfunction(v.length, i => v(i))
        })

        // val numCols = $1(rowIndices(0)).length
        val numCols =
          if (rowIndices.length == 0) 0
          else {
            var z = rowIndices // manual guard against code motion
            $1(z(0)).length // better be pure
          }

        // densematrix_fromarray(array_buffer_result(outData),rowIndices.length,numCols)
        densematrix_fromarray(outData,rowIndices.length,numCols)

        // val rowVectors = rowIndices.map(i => $1(i))
        // // We don't put a check with manual guard here, because it interferes with fusing rowVectors and the subsequent
        // // constructor. Instead, callers are responsible for ensuring the IndexVector is not empty.
        // fassert(rowIndices.length > 0, "error: matrix constructor with empty indices")
        // val numCols = rowVectors(0).length
        // (0::rowVectors.length, 0::numCols) { (i,j) => rowVectors(i).apply(j) }

        // effectful version is more efficient, but prevents optimizations and distribution
        //
        // val first = $1(rowIndices(0)) // better be pure, because we ignore it to maintain normal loop size below
        // val out = DenseMatrix[T](rowIndices.length,first.length)
        // (0::rowIndices.length) foreach { i =>
        //   out(i) = $1(rowIndices(i))
        // }
        // out.unsafeImmutable
      }

      infix (IndexVector) ("apply", T, (CTuple2(IndexWildcard,IndexVector), MInt ==> rhs) :: DenseMatrix(T)) implements composite ${
        val colIndices = $0._2
        // val arrayIndices = array_buffer_fromfunction(colIndices.length, i => colIndices(i))
        // val arrayIndices = array_buffer_new_imm(array_fromfunction(colIndices.length, i => colIndices(i)), colIndices.length)
        // val outData = array_buffer_flatmap[Int,T](arrayIndices, i => {
        //   val v = $1(i)
        //   array_buffer_new_imm(densevector_raw_data(v), v.length)
        // })

        val arrayIndices = array_fromfunction(colIndices.length, i => colIndices(i))
        val outData = array_flatmap[Int,T](arrayIndices, i => {
          val v = $1(i)
          array_fromfunction(v.length, i => v(i))
        })

        // val numRows = $1(colIndices(0)).length
        val numRows =
          if (colIndices.length == 0) 0
          else {
            var z = colIndices // manual guard against code motion
            $1(z(0)).length // better be pure
          }

        // We have to transpose here since we filled the output array in the (incorrect) column-major order.
        // (densematrix_fromarray(array_buffer_result(outData),colIndices.length,numRows)).t
        (densematrix_fromarray(outData,colIndices.length,numRows)).t

        // fassert(colIndices.length > 0, "error: matrix constructor with empty indices")
        // val colVectors = colIndices.map(i => $1(i))
        // val numRows = colVectors(0).length
        // (0::numRows, 0::colVectors.length) { (i,j) => colVectors(j).apply(i) }

        // val first = $1(colIndices(0)) // better be pure, because we ignore it to maintain normal loop size below
        // val out = DenseMatrix[T](first.length, colIndices.length)
        // (0::colIndices.length) foreach { j =>
        //   out.updateCol(j, $1(colIndices(j)))
        // }
        // out.unsafeImmutable
      }
    }
  }
}
