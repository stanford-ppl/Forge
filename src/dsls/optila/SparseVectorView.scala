package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait SparseVectorViewOps {
  this: OptiLADSL =>

  def importSparseVectorViewOps() {
    val T = tpePar("T")
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val IndexVector = lookupTpe("IndexVector")
    val SparseVector = lookupTpe("SparseVector")
    val SparseVectorView = lookupTpe("SparseVectorView")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val Tuple2 = lookupTpe("Tup2")
    val Tuple6 = lookupTpe("Tup6")

    // data fields
    data(SparseVectorView, ("_source", SparseMatrix(T)), ("_start", MInt), ("_stride", MInt), ("_length", MInt), ("_isRow", MBoolean))

    // static methods
    static (SparseVectorView) ("apply", T, ((SparseMatrix(T), MInt, MInt, MInt, MBoolean) :: SparseVectorView)) implements allocates(SparseVectorView, ${$0}, ${$1}, ${$2}, ${$3}, ${$4})

    val SparseVectorViewOps = withTpe(SparseVectorView)
    SparseVectorViewOps {
      compiler ("sparsevectorview_source") (Nil :: SparseMatrix(T)) implements getter(0, "_source")
      compiler ("sparsevectorview_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("sparsevectorview_stride") (Nil :: MInt) implements getter(0, "_stride")

      compiler ("sparsevectorview_calc_offsets_all") (Nil :: Tuple6(MInt,MInt,MInt,MInt,MInt,MInt)) implements composite ${
        val numCols = sparsevectorview_source($self).numCols
        val (startRow,startCol) = unpack(matrix_shapeindex(sparsevectorview_start($self), numCols))
        val (endRow,endCol) = unpack(matrix_shapeindex(sparsevectorview_start($self)+sparsevectorview_stride($self)*$self.length, numCols))
        val rowPtr = sparsematrix_csr_rowptr(sparsevectorview_source($self))
        pack(startRow,endRow,startCol,endCol,rowPtr(startRow),rowPtr(endRow))
      }

      compiler ("sparsevectorview_calc_offsets") (Nil :: Tuple2(MInt,MInt)) implements composite ${
        val (startRow,endRow,startCol,endCol,startOffset,endOffset) = unpack(sparsevectorview_calc_offsets_all($self))
        pack(startOffset,endOffset)
      }

      // since we don't pass in the logical row, this only checks if the column index matches the view's stride
      compiler ("sparsevectorview_includeoffset") (MInt :: MBoolean) implements composite ${
        val srcIndices = sparsematrix_csr_colindices(sparsevectorview_source($self))
        (sparsevectorview_stride($self) == 1) || ((srcIndices($1) % sparsevectorview_stride($self)) == sparsevectorview_start($self))
      }

      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("apply") (MInt :: T) implements composite ${
        val flatIdx = sparsevectorview_start($self) + sparsevectorview_stride($self)*$1
        val src = sparsevectorview_source($self)
        val (rowIdx, colIdx) = unpack(matrix_shapeindex(flatIdx, src.numCols))
        src(rowIdx, colIdx)
      }

      infix ("nnz") (Nil :: MInt) implements composite ${
        val (startOffset, endOffset) = unpack(sparsevectorview_calc_offsets($self))
        val src = sparsevectorview_source($self)
        val srcIndices = sparsematrix_csr_colindices(src)
        var nnz = 0
        for (i <- startOffset until endOffset) {
          if (sparsevectorview_includeoffset($self,i)) {
            nnz += 1
          }
        }
        nnz
      }

      // currently most operations on SparseVectorViews will copy the relevant nz entries in the array into a new array,
      // create a SparseVector wrapper over the new array, and then perform the operation. in order to avoid this copy,
      // we will need a variable-strided view. (or to replicate the interface and repeat the variable stride on each op)

      infix ("nz") (Nil :: DenseVectorView(T), aliasHint = contains(0)) implements single ${
        if (sparsevectorview_stride($self) == 1) {
          val src = sparsevectorview_source($self)
          val (startOffset, endOffset) = unpack(sparsevectorview_calc_offsets($self))
          DenseVectorView[T](sparsematrix_csr_data(src), startOffset, 1, endOffset - startOffset, $self.isRow)
        }
        else {
          // if the stride is uneven in the underlying array, we cannot currently represent this is as a direct view
          $self.toSparse.nz
        }
      }

      infix ("indices") (Nil :: IndexVector) implements single ${
        if (sparsevectorview_stride($self) == 1) {
          // could use a view, except we need to return an IndexVector
          val (startOffset, endOffset) = unpack(sparsevectorview_calc_offsets($self))
          val nnz = endOffset - startOffset
          val srcIndices = sparsematrix_csr_colindices(sparsevectorview_source($self))
          val outIndices = array_empty[Int](nnz)
          for (i <- 0 until nnz) {
            outIndices(i) = srcIndices(i + startOffset)
          }
          indexvector_fromarray(outIndices, $self.isRow)
        }
        else {
          $self.toSparse.indices
        }
      }

      // clones return a SparseVector, so that we do not retain the same underlying pointer
      infix ("Clone") (Nil :: SparseVector(T)) implements redirect ${ $self.toSparse }

      infix ("toDense") (Nil :: DenseVector(T)) implements redirect ${ $self.toSparse.toDense }

      infix ("toSparse") (Nil :: SparseVector(T)) implements single ${
        val (startRow, endRow, startCol, endCol, startOffset, endOffset) = unpack(sparsevectorview_calc_offsets_all($self))
        val nnz = endOffset - startOffset // this is a max, due to stride
        val src = sparsevectorview_source($self)
        val srcIndices = sparsematrix_csr_colindices(src)
        val srcData = sparsematrix_csr_data(src)
        val rowPtr = sparsematrix_csr_rowptr(src)
        var rowIndex = startRow
        val outIndices = array_empty[Int](nnz)
        val outData = array_empty[T](nnz)
        var outNnz = 0

        for (i <- startOffset until endOffset) {
          while (i == rowPtr(rowIndex+1) - rowPtr(startRow)) {
            rowIndex += 1
          }
          if (sparsevectorview_includeoffset($self,i)) {
            outData(outNnz) = srcData(i)
            val idx = if ($self.isRow) srcIndices(i) else rowIndex
            outIndices(outNnz) = idx
            outNnz += 1
          }
        }

        sparsevector_alloc_raw($self.length, $self.isRow, outData.unsafeImmutable, outIndices.unsafeImmutable, outNnz)
      }

      direct ("__equal") (SparseVectorView(T) :: MBoolean) implements composite ${
        $self.length == $1.length &&
        $self.isRow == $1.isRow &&
        sparsevectorview_start($self) == sparsevectorview_start($1) &&
        sparsevectorview_stride($self) == sparsevectorview_stride($1) &&
        sparsevectorview_source($self) == sparsevectorview_source($1)
      }

      direct ("__equal") (SparseVector(T) :: MBoolean) implements single ${
        if ($self.length != $1.length || $self.nnz != $1.nnz || $self.isRow != $1.isRow) false
        else {
          val (startOffset, endOffset) = unpack(sparsevectorview_calc_offsets($self))
          val nnz = endOffset - startOffset
          val src = sparsevectorview_source($self)
          val matIndices = sparsematrix_csr_colindices(src)
          val matData = sparsematrix_csr_data(src)
          val vecIndices = sparsevector_raw_indices($1)
          val vecData = sparsevector_raw_data($1)
          var matIdx = 0
          var vecIdx = 0
          var equal = true

          while (equal && matIdx < nnz) {
            if (sparsevectorview_includeoffset($self,matIndices(matIdx))) {
              if (matIndices(startOffset+matIdx) != vecIndices(vecIdx) || matData(startOffset+matIdx) != vecData(vecIdx)) {
                equal = false
              }
              vecIdx += 1
            }
            matIdx += 1
          }
          equal
        }
      }

      infix ("toString") (Nil :: MString) implements single ${ $self.toSparse.toString }

      fimplicit ("viewToSparse") (Nil :: SparseVector(T)) implements composite ${
        if (Settings.verbose > 0) println("(performance warning): automatic conversion from SparseVectorView to SparseVector")
        // Console.println("  at " + quotePos(fresh[Nothing].withPos(List(implicitly[SourceContext]))))
        $self.toSparse
      }
      val grpName = if (Config.fastCompile) "$Flat" else "SparseVector"
      fimplicit ("chainViewToSparseOps") (Nil :: ephemeralTpe(grpName+"SparseVectorOpsCls[T]", stage = now)) implements composite ${
        repTo\${grpName}SparseVectorOpsCls(viewToSparse($self))
      }
    }
  }
}
