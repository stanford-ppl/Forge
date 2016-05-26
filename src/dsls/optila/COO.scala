package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait COOOps {
  this: OptiLADSL =>

  def importCOOOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")
    val Tuple3 = lookupTpe("Tup3")

    val COO = lookupTpe("COO")
    val CSR = lookupTpe("CSR")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val SparseMatrixNoTranspose = lookupTpe("SparseMatrixNoTranspose")
    val SparseBlockMatrix = lookupTpe("SparseBlockMatrix")
    val SparseDirectedGraph = lookupTpe("SparseDirectedGraph")
    val SparseUndirectedGraph = lookupTpe("SparseUndirectedGraph")

    // COO format
    data(COO, ("_numRows", MInt),
              ("_numCols", MInt),
              ("_data", MArray(T)),
              ("_colIndices", MArray(MInt)),
              ("_rowIndices", MArray(MInt)),
              ("_nnz", MInt),
              ("_nodeProp", MArray(T)))

    // static methods
    static (COO) ("apply", T, (MInt, MInt) :: COO(T), effect = mutable) implements allocates(COO, ${$0}, ${$1}, ${array_empty[T](unit(32))}, ${array_empty[Int](unit(32))}, ${array_empty[Int](unit(32))}, ${unit(0)}, ${array_empty[T](unit(32))})

    compiler (COO) ("coo_alloc_raw", T, MethodSignature(List(
      ("numRows", MInt),
      ("numCols", MInt),
      ("nzElements", MArray(T)),
      ("rowIndices", MArray(MInt)),
      ("colIndices", MArray(MInt)),
      ("nnz", MInt), ("nodeProp", MArray(T))), COO(T))) implements allocates(COO, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5}, ${$6})

    val COOOps = withTpe(COO)
    COOOps {
      /**
       * Accessors
       */
      infix ("numRows") (Nil :: MInt) implements getter(0, "_numRows")
      infix ("numCols") (Nil :: MInt) implements getter(0, "_numCols")
      infix ("nnz"    ) (Nil :: MInt) implements getter(0,     "_nnz")
      infix ("size"   ) (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }

      /**
       * Data ops
       */
      compiler ("coo_get_data"      ) (Nil          :: MArray(T)   ) implements getter(0,       "_data")
      compiler ("coo_get_rowindices") (Nil          :: MArray(MInt)) implements getter(0, "_rowIndices")
      compiler ("coo_get_colindices") (Nil          :: MArray(MInt)) implements getter(0, "_colIndices")
      compiler ("coo_get_nodeprop"  ) (Nil          :: MArray(T)   ) implements getter(0,   "_nodeProp")
      compiler ("coo_set_numrows"   ) (MInt         :: MUnit, effect = write(0)) implements setter(0,    "_numRows", ${$1})
      compiler ("coo_set_numcols"   ) (MInt         :: MUnit, effect = write(0)) implements setter(0,    "_numCols", ${$1})
      compiler ("coo_set_data"      ) (MArray(T)    :: MUnit, effect = write(0)) implements setter(0,       "_data", ${$1})
      compiler ("coo_set_nodeprop"  ) (MArray(T)    :: MUnit, effect = write(0)) implements setter(0,   "_nodeProp", ${$1})
      compiler ("coo_set_rowindices") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_rowIndices", ${$1})
      compiler ("coo_set_colindices") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_colIndices", ${$1})
      compiler ("coo_set_nnz"       ) (MInt         :: MUnit, effect = write(0)) implements setter(0,        "_nnz", ${$1})
      compiler ("coo_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
        if (array_length(coo_get_data($self)) - $self.nnz < extra) {
          coo_realloc($self, $self.nnz + extra)
        }
      }
      compiler ("coo_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = coo_get_data($self)
        val rowIndices = coo_get_rowindices($self)
        val colIndices = coo_get_colindices($self)
        var n = max(4, array_length(data) * 2)
        while (n < minLen) n = n*2
        val outData = array_empty[T](n)
        val outRowIndices = array_empty[Int](n)
        val outColIndices = array_empty[Int](n)
        array_copy(data, 0, outData, 0, $self.nnz)
        array_copy(rowIndices, 0, outRowIndices, 0, $self.nnz)
        array_copy(colIndices, 0, outColIndices, 0, $self.nnz)
        coo_set_data($self, outData.unsafeImmutable)
        coo_set_rowindices($self, outRowIndices.unsafeImmutable)
        coo_set_colindices($self, outColIndices.unsafeImmutable)
      }

      infix ("update") ((MInt,MInt,T) :: MUnit, effect = write(0)) implements composite ${
        // duplicates are allowed, so don't bother checking if the value already exists
        // the last value in the array (furthest to the right is the true value)
        // Note: If we do have duplicate values, we get wrong nnz
        coo_ensureextra($self, 1)
        array_update(coo_get_data($self), $self.nnz, $3)
        array_update(coo_get_rowindices($self), $self.nnz, $1)
        array_update(coo_get_colindices($self), $self.nnz, $2)
        coo_set_nnz($self, $self.nnz+1)
      }

      /**
       * Miscellaneous
       */
      // $self.toString doesn't work in Delite, since there is no 'self' instance
      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ println($self.makeStr + "\\n") }

      infix ("makeString") (Nil :: MString, TStringable(T)) implements single ${
        val rowIndices = coo_get_rowindices($self)
        val colIndices = coo_get_colindices($self)
        val data       = coo_get_data($self)
        var s          = ""

        if ($self == null) {
          s = "null"
        }
        else if ($self.nnz == 0) {
          s = "[ ]"
        }
        else {
          // COO is not stored in order, so just output coordinates as a list
          for (i <- 0 until $self.nnz-1) {
            if (rowIndices(i) > -1)
              s = s + "((" + rowIndices(i) + ", " + colIndices(i) + "), " + data(i).makeStr + ")\\n"
          }
          s = s + "((" + rowIndices($self.nnz-1) + ", " + colIndices($self.nnz-1) + "), " + data($self.nnz-1).makeStr + ")\\n"
        }
        s
      }

      /**
       * Conversion
       * can be CSR, CSC, or both
       */
      infix ("to_sparsematrix") (Nil :: SparseMatrix(T), TArith(T)) implements composite ${ 
        val data = coo_get_data($self)
        val rowIndices = coo_get_rowindices($self)
        val colIndices = coo_get_colindices($self)
        val nodeProp = coo_get_nodeprop($self)
        val csr = coo_to_csr($self)
        val csc = coo_to_csr(coo_alloc_raw[T]($self.numCols,$self.numRows,data.unsafeImmutable,rowIndices.unsafeImmutable,colIndices.unsafeImmutable,$self.nnz,nodeProp.unsafeImmutable))
        sparse_matrix_alloc_raw[T](csr.unsafeImmutable, csc.unsafeImmutable)
      }
      infix ("to_sparsematrixnotranspose") (Nil :: SparseMatrixNoTranspose(T), TArith(T)) implements composite ${ 
        val data = coo_get_data($self)
        val rowIndices = coo_get_rowindices($self)
        val colIndices = coo_get_colindices($self)
        val csr = coo_to_csr($self)
        sparse_matrix_no_transpose_alloc_raw[T](csr.unsafeImmutable)
      }
      // sizes in KB, Row_density = number of columns / number of nz per row
      infix ("to_sparseblockmat") ((("L1_size", MInt), ("L2_size", MInt), ("L3_size", MInt), ("Row_density", MInt)) :: SparseBlockMatrix(T), TArith(T)) implements composite ${ 
        val numCols = $self.numCols
        // assuming T >= 4 bytes
        val l1_s = L1_size * 1024 / 4
        val l2_s = L2_size * 1024 / 4
        val l3_s = L3_size * 1024 / 4
        val l1_density = l1_s / Row_density
        val l2_density = l2_s / Row_density
        val l3_density = l3_s / Row_density
        val density_threshold = 100
        var block_size = numCols
        // Idea: in most cases we want to block for L3, and only upgrade to L2 or L1 when possible
        if (l3_density > density_threshold)
          block_size = L3_size
        if (l2_density > density_threshold)
          block_size = L2_size
        if (l1_density > density_threshold)
          block_size = L1_size
        var numBlocks = numCols / block_size
        if (numCols % block_size > 0)
          numBlocks = numBlocks + 1
        val data = coo_get_data($self)
        val rowIndices = coo_get_rowindices($self)
        val colIndices = coo_get_colindices($self)

        // sort out COO so it's ready to be blocked
        val elems = SHashMap[Long,T]()
        // remove duplicates by preferring elements further to the right in the array
        var i = 0
        while (i < $self.nnz) {
          if (rowIndices(i) >= 0) {  // removed elements are represented by a negative index
            val key = (rowIndices(i).toLong << 32) + colIndices(i).toLong
            elems(key) = data(i)
          }
          i += 1
        }
        val indices       = array_sort(elems.keys)
        val outData       = array_empty[T](array_length(indices))
        val outColIndices = array_empty[Int](array_length(indices))
        val outRowIndices = array_empty[Int](array_length(indices))
        // write to output in sorted order without duplicates
        // left-to-right, top-to-bottom
        i = 0
        while (i < array_length(indices)) {
          val colIdx = (indices(i) & unit(0x00000000ffffffffL)).toInt
          array_update(outColIndices, i, colIdx)
          array_update(outRowIndices, i, (indices(i) >>> 32).toInt)
          array_update(outData, i, elems(indices(i)))
          i += 1
        }

        var csr_blocks = DenseVector[SparseMatrix[T]](numBlocks, unit(true))
        i = 0
        var j = 0
        var k = 0
        while (i < numBlocks) {
          val out_data_block = array_empty[T](block_size)
          val out_colIndices_block = array_empty[Int](block_size)
          val out_rowIndices_block = array_empty[Int](block_size)
          while (j < block_size && k < array_length(indices)) {
            array_update(out_data_block, j, outData(i))
            array_update(out_colIndices_block, j, outColIndices(i))
            array_update(out_rowIndices_block, j, outRowIndices(i))
            j += 1
            k += 1
          }
          val coo = coo_alloc_raw[T](block_size,
                                            $self.numRows,
                                            out_data_block.unsafeImmutable,
                                            out_rowIndices_block.unsafeImmutable,
                                            out_colIndices_block.unsafeImmutable,
                                            out_data_block.length,
                                            array_empty[T](unit(32)))
          val coo_t = coo_alloc_raw[T](block_size,
                                            $self.numRows,
                                            out_data_block.unsafeImmutable,
                                            out_colIndices_block.unsafeImmutable,
                                            out_rowIndices_block.unsafeImmutable,
                                            out_data_block.length,
                                            array_empty[T](unit(32)))
          val csr = coo_to_csr(coo)
          val csc = coo_to_csr(coo_t)
          csr_blocks(i) = sparse_matrix_alloc_raw[T](csr.unsafeImmutable, csc.unsafeImmutable)
          j = 0
          i += 1
        }
        sparse_block_matrix_alloc_raw[T](csr_blocks.unsafeImmutable)
      }
      infix ("to_sparsedirectedgraph") (Nil :: SparseDirectedGraph(T)) implements composite ${ 
        val data = coo_get_data($self)
        val rowIndices = coo_get_rowindices($self)
        val colIndices = coo_get_colindices($self)
        val nodeProp = coo_get_nodeprop($self)
        val csr = coo_to_csr($self)
        val csc = coo_to_csr(coo_alloc_raw[T]($self.numCols,$self.numRows,data.unsafeImmutable,rowIndices.unsafeImmutable,colIndices.unsafeImmutable,$self.nnz,nodeProp))
        sparse_directed_graph_alloc_raw[T](csr.unsafeImmutable, csc.unsafeImmutable, densevector_fromarray(nodeProp, true))
      }
      infix ("to_sparseundirectedgraph") (Nil :: SparseUndirectedGraph(T)) implements composite ${ 
        val data = coo_get_data($self)
        val rowIndices = coo_get_rowindices($self)
        val colIndices = coo_get_colindices($self)
        val nodeProp = coo_get_nodeprop($self)
        val csr = coo_to_csr($self)
        sparse_undirected_graph_alloc_raw[T](csr.unsafeImmutable, densevector_fromarray(nodeProp, true))
      }

      compiler ("coo_to_csr") (Nil :: CSR(T)) implements single ${
        if (coo_ordered($self, $self.nnz, coo_get_rowindices($self),coo_get_colindices($self)))
          coo_to_csr_ordered($self)
        else
          coo_to_csr_unordered($self)
      }

      compiler ("coo_ordered") ((("nnz",MInt),("rowIndices",MArray(MInt)),("colIndices",MArray(MInt))) :: MBoolean) implements single ${
        var i          = 0
        var lastRow    = 0
        var lastCol    = 0
        var outOfOrder = false
        while (i < nnz && !outOfOrder) {
          if (rowIndices(i) < lastRow)
            outOfOrder = true
          if (rowIndices(i) == lastRow && colIndices(i) < lastCol)
            outOfOrder = true
          lastRow = rowIndices(i)
          lastCol = colIndices(i)
          i += 1
        }
        !outOfOrder
      }

      compiler ("coo_to_csr_ordered") (Nil :: CSR(T)) implements single ${
        val data          = coo_get_data($self)
        val rowIndices    = coo_get_rowindices($self)
        val colIndices    = coo_get_colindices($self)

        val outData       = array_empty[T]($self.nnz)
        val outColIndices = array_empty[Int]($self.nnz)
        val outRowPtr     = array_empty[Int]($self.numRows+1)

        var i = 0
        while (i < $self.nnz) {
          array_update(outColIndices, i, colIndices(i))
          array_update(outData, i, data(i))
          array_update(outRowPtr, rowIndices(i)+1, outRowPtr(rowIndices(i)+1)+1)
          i += 1
        }

        coo_to_csr_finalize($self,outData,outColIndices,outRowPtr)
      }

      compiler ("coo_to_csr_unordered") (Nil :: CSR(T)) implements single ${
        val data          = coo_get_data($self)
        val rowIndices    = coo_get_rowindices($self)
        val colIndices    = coo_get_colindices($self)

        // build a hashmap containing the elements of the COO matrix,
        // with tuples mapped to longs and rowIndices in the high bits so we can sort by them.
        // TODO: switch to using a specialized HashMap impl to avoid boxing
        val elems = SHashMap[Long,T]()

        // remove duplicates by preferring elements further to the right in the array
        var i = 0
        while (i < $self.nnz) {
          if (rowIndices(i) >= 0) {  // removed elements are represented by a negative index
            val key = (rowIndices(i).toLong << 32) + colIndices(i).toLong
            elems(key) = data(i)
          }
          i += 1
        }
        val indices       = array_sort(elems.keys)
        val outData       = array_empty[T](array_length(indices))
        val outColIndices = array_empty[Int](array_length(indices))
        val outRowPtr     = array_empty[Int]($self.numRows+1)

        // write to output in sorted order without duplicates
        // left-to-right, top-to-bottom
        i = 0
        while (i < array_length(indices)) {
          val colIdx = (indices(i) & unit(0x00000000ffffffffL)).toInt
          array_update(outColIndices, i, colIdx)
          array_update(outData, i, elems(indices(i)))

          val rowIdx = (indices(i) >>> 32).toInt
          array_update(outRowPtr, rowIdx+1, outRowPtr(rowIdx+1)+1)
          i += 1
        }

        coo_to_csr_finalize($self,outData,outColIndices,outRowPtr)
      }

      compiler ("coo_to_csr_finalize") ((("outData",MArray(T)),("outColIndices",MArray(MInt)),("outRowPtr",MArray(MInt))) :: CSR(T)) implements single ${
        // finalize rowPtr
        var i = 0
        var acc = 0
        while (i < $self.numRows) {
          acc += outRowPtr(i)
          array_update(outRowPtr, i, acc)
          i += 1
        }
        array_update(outRowPtr, $self.numRows, array_length(outData))
        csr_alloc_raw[T]($self.numRows,$self.numCols,outData.unsafeImmutable,outColIndices.unsafeImmutable,outRowPtr.unsafeImmutable,array_length(outData))
      }
    }
  }
}
