package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait CSROps {
  this: OptiLADSL =>

  def importCSROps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")

    val COO = lookupTpe("COO")
    val CSR = lookupTpe("CSR")
    val SparseRowView = lookupTpe("SparseRowView")
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseVector = lookupTpe("SparseVector")
    val SparseVectorView = lookupTpe("SparseVectorView")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val SparseMatrixNoTranspose = lookupTpe("SparseMatrixNoTranspose")
    val SparseDirectedGraph = lookupTpe("SparseDirectedGraph")
    val SparseUndirectedGraph = lookupTpe("SparseUndirectedGraph")

    // data fields
    data(CSR, ("_numRows", MInt), ("_numCols", MInt), ("_data", MArray(T)), ("_colIndices",MArray(MInt)), ("_rowPtr",MArray(MInt)), ("_nnz",MInt))

    // static methods
    static (CSR) ("apply", T, MethodSignature(List(MInt, MInt, MArray(T), MArray(MInt), MArray(MInt), MInt), CSR(T))) implements composite ${ csr_from_raw($0,$1,$2,$3,$4,$5) }
    // helper
    compiler (CSR) ("csr_alloc_raw", T, MethodSignature(List(MInt, MInt, MArray(T), MArray(MInt), MArray(MInt), MInt), CSR(T))) implements allocates(CSR, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5})
    direct (CSR) ("csr_from_raw", T, MethodSignature(List(MInt, MInt, MArray(T), MArray(MInt), MArray(MInt), MInt), CSR(T))) implements allocates(CSR, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5})

    val CSROps = withTpe (CSR)
    CSROps {
      /**
       * Accessors
       */
      infix ("numRows") (Nil :: MInt) implements getter(0, "_numRows")
      infix ("numCols") (Nil :: MInt) implements getter(0, "_numCols")
      infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }
      infix ("nnz") (Nil :: MInt) implements getter(0, "_nnz")
      infix ("nz") (Nil :: DenseVector(T)) implements composite ${ densevector_fromarray(csr_get_data($self), unit(true)) }
      infix ("apply") ((MInt,MInt) :: T) implements composite ${
        $self.getRow($1).apply($2)
      }

      infix ("toDense") (Nil :: DenseMatrix(T)) implements composite ${
        val out = DenseMatrix[T]($self.numRows, $self.numCols)
        val rowPtr = csr_get_rowptr($self)
        val colIndices = csr_get_colindices($self)
        val data = csr_get_data($self)

        for (i <- 0 :: $self.numRows) {
          for (j <- rowPtr(i) :: rowPtr(i+1)) {
            out(i,colIndices(j)) = data(j)
          }
        }
        out.unsafeImmutable
      }

      // $self.toString doesn't work in Delite, since there is no 'self' instance
      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ println($self.makeStr + "\\n") }

      infix ("makeString") (Nil :: MString, TStringable(T)) implements single ${
        val rowPtr = csr_get_rowptr($self)
        val colIndices = csr_get_colindices($self)
        val data = csr_get_data($self)
        var s = ""

        if ($self == null) {
          s = "null"
        }
        else if ($self.nnz < 1) {
          s = "[ ]"
        }
        else {
          for (i <- 0 until $self.numRows) {
            val nnz = rowPtr(i+1) - rowPtr(i)
            if (nnz > 0) {
              s = s + "(" + i + "): "
              for (j <- rowPtr(i) until rowPtr(i+1)-1) {
                s = s + "(" + colIndices(j) + ", " + data(j).makeStr + "), "
              }
              val lineEnd = if (i == $self.numRows-1) "" else "\\n"
              s = s + "(" + colIndices(rowPtr(i+1)-1) + ", " + data(rowPtr(i+1)-1).makeStr + ")" + lineEnd
            }
          }
          s
        }
        s
      }

      /**
       * Data operations
       */
      compiler ("csr_get_data"      ) (Nil          :: MArray(T)   ) implements getter(0,       "_data")
      compiler ("csr_get_rowptr"    ) (Nil          :: MArray(MInt)) implements getter(0,     "_rowPtr")
      compiler ("csr_get_colindices") (Nil          :: MArray(MInt)) implements getter(0, "_colIndices")
      compiler ("csr_set_numrows"   ) (MInt         :: MUnit, effect = write(0)) implements setter(0,    "_numRows", ${$1})
      compiler ("csr_set_numcols"   ) (MInt         :: MUnit, effect = write(0)) implements setter(0,    "_numCols", ${$1})
      compiler ("csr_set_data"      ) (MArray(T)    :: MUnit, effect = write(0)) implements setter(0,       "_data", ${$1})
      compiler ("csr_set_rowptr"    ) (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0,     "_rowPtr", ${$1})
      compiler ("csr_set_colindices") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_colIndices", ${$1})
      compiler ("csr_set_nnz"       ) (MInt         :: MUnit, effect = write(0)) implements setter(0,        "_nnz", ${$1})
      
      infix ("to_coo") (Nil :: COO(T)) implements composite ${
        val out        = COO[T]($self.numCols, $self.numRows)
        val rowPtr     = csr_get_rowptr($self)
        val colIndices = csr_get_colindices($self)
        val data       = csr_get_data($self)
        val rowIndices = DenseVector[Int](array_length(colIndices), false)
        for (i <- 0 until $self.numRows) {
          for (j <- rowPtr(i) until rowPtr(i+1)-1) {
            rowIndices(j) = j - rowPtr(i)
          }
        }
        coo_set_rowindices(out, densevector_raw_data(rowIndices))
        coo_set_colindices(out, colIndices)
        coo_set_data(out, data)
        coo_set_nnz(out, $self.nnz)
        out
      }

      // Note the following 3 should only be called if we loaded CSR from file.
      // Note we don't check if M and M^T are the same
      infix ("to_sparsematrix") (CSR(T) :: SparseMatrix(T), TArith(T)) implements composite ${
        sparse_matrix_alloc_raw[T]($self.unsafeImmutable, $1.unsafeImmutable)
      }
      infix ("to_sparsematrixnotranspose") (Nil :: SparseMatrixNoTranspose(T), TArith(T)) implements composite ${
        sparse_matrix_no_transpose_alloc_raw[T]($self.unsafeImmutable)
      }
      infix ("to_sparsedirectedgraph") ((("csrt", CSR(T)), ("nodeProp", DenseVector(T))) :: SparseDirectedGraph(T)) implements composite ${ 
        sparse_directed_graph_alloc_raw[T]($self.unsafeImmutable, csrt.unsafeImmutable, nodeProp)
      }
      // $1 is nodeProp
      infix ("to_sparseundirectedgraph") (("nodeProp", DenseVector(T)) :: SparseUndirectedGraph(T)) implements composite ${ 
        sparse_undirected_graph_alloc_raw[T]($self.unsafeImmutable, nodeProp)
      }

      /**
       * Math
       */

       // Sparse +/* Sparse needs to be improved
      compiler ("zipSparseUnion") ((CSR(B), (T,B) ==> R) :: CSR(R), addTpePars = (B,R)) implements single ${
        val aData         = csr_get_data($self)
        val aColIndices   = csr_get_colindices($self)
        val aRowPtr       = csr_get_rowptr($self)
        val bData         = csr_get_data($1)
        val bColIndices   = csr_get_colindices($1)
        val bRowPtr       = csr_get_rowptr($1)
        val outRowPtr     = array_empty[Int]($self.numRows+1)
        val outColIndices = array_empty[Int]($self.nnz + $1.nnz) // upper bound
        val outData       = array_empty[R]($self.nnz + $1.nnz)

        var aOldRow = aRowPtr(0)
        var bOldRow = bRowPtr(0)
        var nnz     = 0
        var i       = 0

        while (i < $self.numRows+1) {
          // union of colIndicesA and colIndicesB at row i
          if (aRowPtr(i) != aOldRow || bRowPtr(i) != bOldRow) {
            nnz = zipUnion(nnz, aOldRow, aRowPtr(i), aColIndices, aData, bOldRow, bRowPtr(i), bColIndices, bData, outColIndices, outData, $2)
          }
          outRowPtr(i) = nnz
          aOldRow = aRowPtr(i)
          bOldRow = bRowPtr(i)
          i += 1
        }

        csr_alloc_raw[R]($self.numRows, $self.numCols, outData.unsafeImmutable, outColIndices.unsafeImmutable, outRowPtr.unsafeImmutable, nnz)
      }
      compiler ("zipSparseIntersect") ((CSR(B), (T,B) ==> R) :: CSR(R), addTpePars = (B,R)) implements single ${
        val aData         = csr_get_data($self)
        val aColIndices   = csr_get_colindices($self)
        val aRowPtr       = csr_get_rowptr($self)
        val bData         = csr_get_data($1)
        val bColIndices   = csr_get_colindices($1)
        val bRowPtr       = csr_get_rowptr($1)
        val outRowPtr     = array_empty[Int]($self.numRows+1)
        val outColIndices = array_empty[Int]($self.nnz + $1.nnz) // upper bound
        val outData       = array_empty[R]($self.nnz + $1.nnz)

        var aOldRow = aRowPtr(0)
        var bOldRow = bRowPtr(0)
        var nnz     = 0
        var i       = 0

        while (i < $self.numRows+1) {
          // union of colIndicesA and colIndicesB at row i
          if (aRowPtr(i) != aOldRow || bRowPtr(i) != bOldRow) {
            nnz = zipIntersect(nnz, aOldRow, aRowPtr(i), aColIndices, aData, bOldRow, bRowPtr(i), bColIndices, bData, outColIndices, outData, $2)
          }
          outRowPtr(i) = nnz
          aOldRow = aRowPtr(i)
          bOldRow = bRowPtr(i)
          i += 1
        }

        csr_alloc_raw[R]($self.numRows, $self.numCols, outData.unsafeImmutable, outColIndices.unsafeImmutable, outRowPtr.unsafeImmutable, nnz)
      }
      infix ("mul_vector_for") ( DenseVector(T) :: DenseVector(T), TArith(T) ) implements composite ${
        fassert($self.numCols == $1.length && !$1.isRow, "dimension mismatch: matrix * vector")
        val out        = DenseVector[T]($self.numRows, false)
        val data       = csr_get_data($self)
        val rowPtr     = csr_get_rowptr($self)
        val colIndices = csr_get_colindices($self)
        val vec        = densevector_raw_data($1)
        for (i <- 0 :: $self.numRows) {
          for (j <- rowPtr(i) :: rowPtr(i+1)) {
            out(i) = out(i) + data(j) * vec(colIndices(j))
          }
        }
        out
      }

      // Math
      infix ("+")   (CSR(T)      ::      CSR(T), TArith(T)) implements composite ${     zipSparseUnion[T,T,T]($self, $1, (a,b) => a+b) }
      infix ("-")   (CSR(T)      ::      CSR(T), TArith(T)) implements composite ${     zipSparseUnion[T,T,T]($self, $1, (a,b) => a-b) }
      infix ("*:*") (CSR(T)      ::      CSR(T), TArith(T)) implements composite ${ zipSparseIntersect[T,T,T]($self, $1, (a,b) => a*b) }

      infix ("+")   (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense + $1 }
      infix ("-")   (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense - $1 }
      infix ("*:*") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense * $1 }

      infix ("*")   (DenseVector(T) :: DenseVector(T), TArith(T)) implements composite ${$self.mul_vector_for($1)}

      infix ("+")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense + $1 }
      infix ("-")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense - $1 }
      infix ("*")   (T :: CSR(T), TArith(T)) implements composite ${
        val out_data = $self.nz.map(e => e*$1)
        csr_alloc_raw($self.numRows, $self.numCols, densevector_raw_data(out_data), csr_get_colindices($self), csr_get_rowptr($self), $self.nnz)
      }
      infix ("/")   (T :: CSR(T), TArith(T)) implements composite ${
        val out_data = $self.nz.map(e => e/$1)
        csr_alloc_raw($self.numRows, $self.numCols, densevector_raw_data(out_data), csr_get_colindices($self), csr_get_rowptr($self), $self.nnz)
      }
      infix ("sum") (Nil :: T, TArith(T)) implements composite ${ $self.nz.sum }
      infix ("abs") (Nil :: CSR(T), TArith(T)) implements composite ${
        val out_data = $self.nz.map(e => e.abs)
        csr_alloc_raw($self.numRows, $self.numCols, densevector_raw_data(out_data), csr_get_colindices($self), csr_get_rowptr($self), $self.nnz)
      }
      infix ("min") (Nil :: T, (TOrdering(T), THasMinMax(T))) implements composite ${ $self.nz.min }
      infix ("max") (Nil :: T, (TOrdering(T), THasMinMax(T))) implements composite ${ $self.nz.max }

      // TODO: compare different implementations of getRow
      infix ("getRow") ( MInt :: SparseRowView(T) ) implements composite ${
        val data       = csr_get_data($self)
        val rowPtr     = csr_get_rowptr($self)
        val colIndices = csr_get_colindices($self)
        SparseRowView[T](data, colIndices(rowPtr($1)), colIndices(rowPtr($1+1))-colIndices(rowPtr($1)))
      }
      infix ("getRowIndices") ( MInt :: SparseRowView(MInt) ) implements composite ${
        val rowPtr     = csr_get_rowptr($self)
        val colIndices = csr_get_colindices($self)
        SparseRowView[Int](colIndices, colIndices(rowPtr($1)), colIndices(rowPtr($1+1))-colIndices(rowPtr($1)))
      }
      // Warning: should use colIndices on the transpose whenever possible
      infix ("rowIndices") (Nil :: IndexVector) implements single ${
        val rowPtr = csr_get_rowptr($self)
        val nnz = rowPtr(array_length(rowPtr)-1)
        val rows = array_empty[Int](nnz)
        var i = 0
        var oldRow = rowPtr(0)
        while (i < array_length(rowPtr)) {
          val nextRow = rowPtr(i)
          if (nextRow != oldRow) {
            for (j <- oldRow until nextRow) {
              rows(j) = i-1
            }
          }
          oldRow = nextRow
          i += 1
        }
        indexvector_fromarray(rows.unsafeImmutable, false)
      }
      infix ("colIndices") (Nil :: IndexVector) implements composite ${
        IndexVector(indexvector_fromarray(csr_get_colindices($self), true))
      }
      infix ("nzRows") (Nil :: IndexVector) implements redirect ${ IndexVector($self.rowIndices.distinct) }
      infix ("nzCols") (Nil :: IndexVector) implements redirect ${ IndexVector($self.colIndices.distinct) }
      direct ("__equal") (CSR(T) :: MBoolean) implements composite ${
        if ($self.numRows != $1.numRows || $self.numCols != $1.numCols) false
        else {
          val dataEqual = densevector_alloc_raw($self.nnz, true, csr_get_data($self)) == densevector_alloc_raw($1.nnz, true, csr_get_data($1))
          val colIndexEqual = densevector_alloc_raw($self.nnz, true, csr_get_colindices($self)) == densevector_alloc_raw($1.nnz, true, csr_get_colindices($1))
          val rowPtrEqual = densevector_alloc_raw($self.numRows+1, true, csr_get_rowptr($self)) == densevector_alloc_raw($1.numRows+1, true, csr_get_rowptr($1))
          dataEqual && colIndexEqual && rowPtrEqual
        }
      }
      direct ("__equal") (DenseMatrix(T) :: MBoolean) implements composite ${ $self.toDense == $1 }

      // Bulk
      infix ("mapnz") ((T ==> R) :: CSR(R), addTpePars = R) implements composite ${
        val out = $self.nz.map($1)
        csr_alloc_raw($self.numRows, $self.numCols, densevector_raw_data(out), csr_get_colindices($self), csr_get_rowptr($self), $self.nnz)
      }
      infix ("foreachnz") ((T ==> MUnit) :: MUnit) implements composite ${ $self.nz.foreach($1) }
      infix ("countnz") ((T ==> MBoolean) :: MInt) implements composite ${ $self.nz.count($1) }
      // Do we need the output as SparseVector?
      // infix ("mapRowsToVector") ((SparseVectorView(T) ==> R) :: SparseVector(R), addTpePars = R) implements composite ${}
      infix ("mapRowsToDenseVector") ((SparseRowView(T) ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
         IndexVector(0, $self.numRows, false).map(i => $1($self.getRow(i)))
      }
      // TODO: decide how we should suffer for methods that's only fast on one of csr or csc
      //       either we suffer losing our transpose after getting the rows/cols
      //       or we call suffer the slowness on one part
      //       The following is a list of problem methods
      // infix ("getRows") (IndexVector :: SparseMatrix(T)) implements composite
      // infix ("getCols") (IndexVector :: SparseMatrix(T)) implements composite
      // infix ("slice") ((("startRow",MInt),("endRow",MInt),("startCol",MInt),("endCol",MInt)) :: SparseMatrix(T))
      // infix ("sliceRows") ((("start",MInt),("end",MInt)) :: SparseMatrix(T))
      // infix ("sliceCols") ((("start",MInt),("end",MInt)) :: SparseMatrix(T))
      // infix ("filterRows") ((SparseVectorView(T) ==> MBoolean) :: SparseMatrix(T))
      // infix ("filterCols") ((SparseVectorView(T) ==> MBoolean) :: SparseMatrix(T))
      // infix ("foreachRow") ((SparseVectorView(T) ==> MUnit) :: MUnit, effect = simple) implements composite ${
      //   $self.nzRows foreach { i => $1($self(i)) }
      // }
      // infix ("foreachCol") ((SparseVectorView(T) ==> MUnit) :: MUnit, effect = simple) implements composite ${
      //   $self.nzCols foreach { i => $1($self.getCol(i)) }
      // }
    }
  }
}
