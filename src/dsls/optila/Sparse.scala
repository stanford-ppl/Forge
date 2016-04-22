/*//////////////////////////////////////////////////////////////
Author: Alex G B Jin

Description: Combination of graph and sparsematrix traits from
optigraph and optila. This implementation supports CSR and CSR+
CSC format.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait SparseOps {
  this: OptiLADSL =>

  def importSparseOps() {
    importSparseCOOOps() // EdgeList
    importSparseMatrixOps() // Rename to SparseMatrixrix
    //importSparseBlockMatrixOps()
    importSparseDirectedGraphOps()
    importSparseUndirectedGraphOps()
    importSparseCSROps()
  }

  def importSparseMatrixOps() {
    val T = tpePar("T")
    val Sparse = lookupTpe("SparseCSR")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    data(SparseMatrix, ("_csr", SparseCSR(T)))
    // helper
    compiler (SparseMatrix) ("sparse_matrix_alloc_raw", T, MethodSignature(SparseCSR(T), SparseMatrix(T))) implements allocates(SparseMatrix, ${$0})

    val SparseMatrixOps = withTpe (SparseMatrix)
    SparseMatrixOps {
      compiler ("get_csr" ) (Nil :: SparseCSR(T)) implements getter(0, "_csr")
      compiler ("set_csr" ) (SparseCSR(T) :: MUnit, effect = write(0)) implements setter(0,  "_csr", ${$1})
      infix ("numRows") (Nil :: MInt) implements composite ${ get_csr($self).numRows }
      infix ("numCols") (Nil :: MInt) implements composite ${ get_csr($self).numCols }
      infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }
      infix ("nnz") (Nil :: MInt) implements composite ${ get_csr($self).nnz }
      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ get_csr($self).pprint }
      infix ("+")   (SparseMatrix(T) :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) + get_csr($1)) }
      infix ("-")   (SparseMatrix(T) :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) - get_csr($1)) }
      infix ("*:*") (SparseMatrix(T) :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) *:* get_csr($1)) }
      infix ("+")   (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self) + $1 }
      infix ("-")   (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self) - $1 }
      infix ("*:*") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self).toDense * $1 }
      infix ("*")   (DenseVector(T) :: DenseVector(T), TArith(T)) implements composite ${ get_csr($self)*$1 }
      infix ("+")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self).toDense + $1 }
      infix ("-")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self).toDense - $1 }
      infix ("*")   (T :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self)*$1) }
      infix ("/")   (T :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self)/$1) }
      infix ("sum") (Nil :: T, TArith(T)) implements composite ${ get_csr($self).sum }
      infix ("abs") (Nil :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self).abs) }
    }
  }

  // TODO: fix this
  // def importSparseBlockMatrixOps() {
  //   val T = tpePar("T")
  //   val Sparse = lookupTpe("SparseCSR")
  //   val SparseMatrix = lookupTpe("SparseMatrix")
  //   val SparseBlockMatrix = lookupTpe("SparseBlockMatrix")
  //   val DenseVector = lookupTpe("DenseVector")
  //   val DenseMatrix = lookupTpe("DenseMatrix")
  //   data(SparseBlockMatrix, ("_csr_blocks", DenseVector(SparseMatrix(T))))
  //   // helper
  //   compiler (SparseBlockMatrix) ("sparse_block_mat_alloc_raw", T, MethodSignature(DenseVector(SparseMatrix(T)), SparseBlockMatrix(T))) implements allocates(SparseBlockMatrix, ${$0})

  //   val SparseBlockMatrixOps = withTpe (SparseBlockMatrix)
  //   SparseBlockMatrixOps {
  //     /**
  //      * Data operations, Accessors and Mutators
  //      */
  //     compiler ("get_csr_blocks" ) (Nil :: DenseVector(SparseMatrix(T))) implements getter(0, "_csr_blocks")
  //     infix ("numBlocks") (Nil :: MInt) implements composite ${ get_csr_blocks($self).length }
  //     infix ("numRows") (Nil :: MInt) implements composite ${
  //       val csr_blocks = get_csr_blocks($self)
  //       (0::$self.numBlocks).map({e => csr_blocks(e).numRows}).max
  //     }
  //     infix ("numCols") (Nil :: MInt) implements composite ${
  //       val csr_blocks = get_csr_blocks($self)
  //       (0::$self.numBlocks).map({e => csr_blocks(e).numCols}).sum
  //     }
  //     infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }
  //     infix ("nnz") (Nil :: MInt) implements composite ${
  //       val csr_blocks = get_csr_blocks($self)
  //       (0::$self.numBlocks).map({e => csr_blocks(e).nnz}).sum
  //     }
  //     infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${
  //       val csr_blocks = get_csr_blocks($self)
  //       var i = 0
  //       while (i < $self.numBlocks) {
  //         csr_blocks(i).pprint
  //       }
  //     }
  //     // infix ("+")   (SparseMatrix(T)   ::   SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) + get_csr($1)) }
  //     // infix ("-")   (SparseMatrix(T)   ::   SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) - get_csr($1)) }
  //     // infix ("*:*") (SparseMatrix(T)   ::   SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) *:* get_csr($1)) }
  //     // infix ("+")   (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self) + $1 }
  //     // infix ("-")   (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self) - $1 }
  //     // infix ("*:*") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self).toDense * $1 }
  //     infix ("*")   (DenseVector(T) :: DenseVector(T), TArith(T)) implements composite ${
  //       val csr_blocks = get_csr_blocks($self)
  //       val block_size = csr_blocks(0).numCols
  //       val vector_blocks = (0::$self.numBlocks).map({e =>
  //           $1.slice(e*block_size, (e+1)*block_size)
  //         })
  //       //val out_blocks = (0::$self.numBlocks).map({e => csr_blocks(e) * vector_blocks(e)})
  //       val out_block = csr_blocks(0).asInstanceOf[Rep[SparseMatrix[T]]] * vector_blocks(0).asInstanceOf[Rep[DenseVector[T]]]
  //       var i = 1
  //       while (i < $self.numBlocks) {
  //         out_block += csr_blocks(i).asInstanceOf[Rep[SparseMatrix[T]]] * vector_blocks(i).asInstanceOf[Rep[DenseVector[T]]]
  //         i += 1
  //       }
  //       out_block
  //     }
  //     // infix ("+")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self).toDense + $1 }
  //     // infix ("-")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self).toDense - $1 }
  //     // infix ("*")   (T :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self)*$1) }
  //     // infix ("/")   (T :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self)/$1) }
  //     infix ("sum") (Nil :: T, TArith(T)) implements composite ${
  //       val csr_blocks = get_csr_blocks($self)
  //       (0::$self.numBlocks).map({e => csr_blocks(e).sum}).sum
  //     }
  //     // infix ("abs") (Nil :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self).abs) }
  //   }
  // }

  def importSparseDirectedGraphOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")
    val Sparse = lookupTpe("SparseCSR")
    val SparseRowView = lookupTpe("SparseRowView")
    val SparseDirectedGraph = lookupTpe("SparseDirectedGraph")
    val DenseVector = lookupTpe("DenseVector")
    data(SparseDirectedGraph, ("_outNeighbors", SparseCSR(T)), ("_inNeighbors", SparseCSR(T)), ("_nodeprop", DenseVector(T)))
    // helper
    compiler (SparseDirectedGraph) ("sparse_directed_graph_alloc_raw", T, MethodSignature(List(SparseCSR(T), SparseCSR(T), DenseVector(T)), SparseDirectedGraph(T))) implements allocates(SparseDirectedGraph, ${$0}, ${$1}, ${$2})

    val SparseDirectedGraphOps = withTpe (SparseDirectedGraph)
    SparseDirectedGraphOps {
      /**
       * Data operations, Accessors and Mutators
       */
      compiler ("get_outNeighbors" ) (Nil :: SparseCSR(T)) implements getter(0, "_outNeighbors")
      compiler ("get_inNeighbors") (Nil :: SparseCSR(T)) implements getter(0, "_inNeighbors")
      compiler ("get_nodeprop_directed" ) (Nil :: DenseVector(T)) implements getter(0, "_nodeprop")
      compiler ("set_outNeighbors" ) (SparseCSR(T) :: MUnit, effect = write(0)) implements setter(0,  "_outNeighbors", ${$1})
      compiler ("set_inNeighbors") (SparseCSR(T) :: MUnit, effect = write(0)) implements setter(0, "_inNeighbors", ${$1})
      compiler ("set_nodeprop_directed") (DenseVector(T) :: MUnit, effect = write(0)) implements setter(0, "_nodeprop", ${$1})
      infix ("numRows") (Nil :: MInt) implements composite ${ get_outNeighbors($self).numRows }
      infix ("numCols") (Nil :: MInt) implements composite ${ get_outNeighbors($self).numCols }
      infix ("numNodes") (Nil :: MInt) implements composite ${ get_outNeighbors($self).numRows }
      infix ("numEdges") (Nil :: MInt) implements composite ${ get_outNeighbors($self).nnz }
      infix ("isDirected") (Nil :: MBoolean) implements composite ${true}
      infix ("outNeighbors") (MInt :: SparseRowView(MInt)) implements composite ${ get_outNeighbors($self).getRowIndices($1) } 
      infix ("inNeighbors") (MInt :: SparseRowView(MInt)) implements composite ${ get_inNeighbors($self).getRowIndices($1) }
      infix ("nodeProp") (Nil :: DenseVector(T)) implements composite ${ get_nodeprop_directed($self) }
      infix ("outDegree") (MInt :: MInt) implements composite ${ $self.outNeighbors($1).length() }
      infix ("inDegree") (MInt :: MInt) implements composite ${ $self.inNeighbors($1).length() }
      infix ("sumOverInNeighbors") ((MInt, MInt ==> R) :: R, TArith(R), addTpePars = R) implements composite ${
        val in_neighbors = $self.inNeighbors($1)
        in_neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => true})
      }
      // (Node, Neighbor==>value, filter) :: sum
      infix ("sumOverInNeighborsCond") ((MInt, MInt ==> R, MInt ==> MBoolean) :: R, TArith(R), addTpePars = R) implements composite ${
        val in_neighbors = $self.inNeighbors($1)
        in_neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => $3(n)})
      }
      // (Node, Neighbor==>value) :: sum
      infix ("sumOverOutNeighbors") ((MInt, MInt ==> R) :: R, TArith(R), addTpePars = R) implements composite ${
        val out_neighbors = $self.outNeighbors($1)
        out_neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => true})
      }
      infix ("sumOverOutNeighborsCond") ((MInt, MInt ==> R, MInt ==> MBoolean) :: R, TArith(R), addTpePars = R) implements composite ${
        val out_neighbors = $self.outNeighbors($1)
        out_neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => $3(n)})
      }
      // (Nodes Array, (Nodeprop, Node)==>value) :: sum
      infix ("sumOverNodes") ((MArray(MInt), (MInt, T) ==> R) :: R, TArith(R), addTpePars = R) implements composite ${
        val nodes = densevector_fromarray($1, unit(true))
        val nodeprop = $self.nodeProp
        nodes.map[R]({n => $2(n, nodeprop(n))}).reduce({(a,b) => a+b})
      }
    }
  }

  def importSparseUndirectedGraphOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")
    val Sparse = lookupTpe("SparseCSR")
    val SparseRowView = lookupTpe("SparseRowView")
    val SparseUndirectedGraph = lookupTpe("SparseUndirectedGraph")
    val DenseVector = lookupTpe("DenseVector")
    data(SparseUndirectedGraph, ("_neighbors", SparseCSR(T)), ("_nodeprop", DenseVector(T)))
    // helper
    compiler (SparseUndirectedGraph) ("sparse_undirected_graph_alloc_raw", T, MethodSignature(List(SparseCSR(T), DenseVector(T)), SparseUndirectedGraph(T))) implements allocates(SparseUndirectedGraph, ${$0}, ${$1})

    val SparseUndirectedGraphOps = withTpe (SparseUndirectedGraph)
    SparseUndirectedGraphOps {
      /**
       * Data operations, Accessors and Mutators
       */
      compiler ("get_neighbors" ) (Nil :: SparseCSR(T)) implements getter(0, "_neighbors")
      compiler ("get_nodeprop_undirected" ) (Nil :: DenseVector(T)) implements getter(0, "_nodeprop")
      compiler ("set_neighbors" ) (SparseCSR(T) :: MUnit, effect = write(0)) implements setter(0,  "_neighbors", ${$1})
      compiler ("set_nodeprop_undirected") (DenseVector(T) :: MUnit, effect = write(0)) implements setter(0, "_nodeprop", ${$1})
      infix ("numRows") (Nil :: MInt) implements composite ${ get_neighbors($self).numRows }
      infix ("numCols") (Nil :: MInt) implements composite ${ get_neighbors($self).numCols }
      infix ("numNodes") (Nil :: MInt) implements composite ${ get_neighbors($self).numRows }
      infix ("numEdges") (Nil :: MInt) implements composite ${ get_neighbors($self).nnz }
      infix ("isDirected") (Nil :: MBoolean) implements composite ${false}
      infix ("neighbors") (MInt :: SparseRowView(MInt)) implements composite ${ get_neighbors($self).getRowIndices($1) } 
      infix ("nodeProp") (Nil :: DenseVector(T)) implements composite ${ get_nodeprop_undirected($self) }
      infix ("degree") (MInt :: MInt) implements composite ${ $self.neighbors($1).length() }
      infix ("sumOverNeighbors") ((MInt, MInt ==> R) :: R, TArith(R), addTpePars = R) implements composite ${
        val neighbors = $self.neighbors($1)
        neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => true})
      }
      infix ("sumOverNeighborsCond") ((MInt, MInt ==> R, MInt ==> MBoolean) :: R, TArith(R), addTpePars = R) implements composite ${
        val neighbors = $self.neighbors($1)
        neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => $3(n)})
      }
      // (Nodes Array, (Nodeprop, Node)==>value) :: sum
      infix ("sumOverNodes") ((MArray(MInt), (MInt, T) ==> R) :: R, TArith(R), addTpePars = R) implements composite ${
        val nodes = densevector_fromarray($1, unit(true))
        val nodeprop = $self.nodeProp
        nodes.map[R]({n => $2(n, nodeprop(n))}).reduce({(a,b) => a+b})
      }
    }
  }

  def importSparseCOOOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")

    val Sparse    = lookupTpe("SparseCSR")
    val SparseCOO = lookupTpe("SparseCOO")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val SparseBlockMatrix = lookupTpe("SparseBlockMatrix")
    val SparseDirectedGraph = lookupTpe("SparseDirectedGraph")
    val SparseUndirectedGraph = lookupTpe("SparseUndirectedGraph")
    /* for testing, remove*/
    val SparseMatrixrixBuildable = lookupTpe("SparseMatrixrixBuildable")
    /* for testing, remove*/

    // COO format
    data(SparseCOO, ("_numRows", MInt),
                    ("_numCols", MInt),
                    ("_data", MArray(T)),
                    ("_colIndices", MArray(MInt)),
                    ("_rowIndices", MArray(MInt)),
                    ("_nnz", MInt),
                    ("_nodeProp", MArray(T)))

    // static methods
    static (Sparse) ("apply", T, (MInt, MInt) :: SparseCOO(T), effect = mutable) implements allocates(SparseCOO, ${$0}, ${$1}, ${array_empty[T](unit(32))}, ${array_empty[Int](unit(32))}, ${array_empty[Int](unit(32))}, ${unit(0)}, ${array_empty[T](unit(32))})

    compiler (Sparse) ("sparse_coo_alloc_raw", T, MethodSignature(List(
      ("numRows", MInt),
      ("numCols", MInt),
      ("nzElements", MArray(T)),
      ("rowIndices", MArray(MInt)),
      ("colIndices", MArray(MInt)),
      ("nnz", MInt), ("nodeProp", MArray(T))), SparseCOO(T))) implements allocates(SparseCOO, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5}, ${$6})

    val SparseCOOOps = withTpe(SparseCOO)
    SparseCOOOps {
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
      compiler ("sparsecoo_get_data"      ) (Nil          :: MArray(T)   ) implements getter(0,       "_data")
      compiler ("sparsecoo_get_rowindices") (Nil          :: MArray(MInt)) implements getter(0, "_rowIndices")
      compiler ("sparsecoo_get_colindices") (Nil          :: MArray(MInt)) implements getter(0, "_colIndices")
      compiler ("sparsecoo_get_nodeprop"  ) (Nil          :: MArray(T)   ) implements getter(0,   "_nodeProp")
      compiler ("sparsecoo_set_numrows"   ) (MInt         :: MUnit, effect = write(0)) implements setter(0,    "_numRows", ${$1})
      compiler ("sparsecoo_set_numcols"   ) (MInt         :: MUnit, effect = write(0)) implements setter(0,    "_numCols", ${$1})
      compiler ("sparsecoo_set_data"      ) (MArray(T)    :: MUnit, effect = write(0)) implements setter(0,       "_data", ${$1})
      compiler ("sparsecoo_set_nodeprop"  ) (MArray(T)    :: MUnit, effect = write(0)) implements setter(0,   "_nodeProp", ${$1})
      compiler ("sparsecoo_set_rowindices") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_rowIndices", ${$1})
      compiler ("sparsecoo_set_colindices") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_colIndices", ${$1})
      compiler ("sparsecoo_set_nnz"       ) (MInt         :: MUnit, effect = write(0)) implements setter(0,        "_nnz", ${$1})

      /**
       * Miscellaneous
       */
      // $self.toString doesn't work in Delite, since there is no 'self' instance
      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ println($self.makeStr + "\\n") }

      infix ("makeString") (Nil :: MString, TStringable(T)) implements single ${
        val rowIndices = sparsecoo_get_rowindices($self)
        val colIndices = sparsecoo_get_colindices($self)
        val data       = sparsecoo_get_data($self)
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
       * Conversion to matrix or graph
       */
      infix ("to_sparsematrix") (Nil :: SparseMatrix(T), TArith(T)) implements composite ${ 
        val data = sparsecoo_get_data($self)
        val rowIndices = sparsecoo_get_rowindices($self)
        val colIndices = sparsecoo_get_colindices($self)
        val csr = sparsecoo_to_sparsecsr($self)
        sparse_matrix_alloc_raw[T](csr.unsafeImmutable)
      }
      // sizes in KB, Row_density = number of columns / number of nz per row
      // infix ("to_sparseblockmat") ((("L1_size", MInt), ("L2_size", MInt), ("L3_size", MInt), ("Row_density", MInt)) :: SparseBlockMatrix(T), TArith(T)) implements composite ${ 
      //   val numCols = $self.numCols
      //   // assuming T >= 4 bytes
      //   val l1_s = L1_size * 1024 / 4
      //   val l2_s = L2_size * 1024 / 4
      //   val l3_s = L3_size * 1024 / 4
      //   val l1_density = l1_s / Row_density
      //   val l2_density = l2_s / Row_density
      //   val l3_density = l3_s / Row_density
      //   var block_size = numCols
      //   // 100: Somewaht arbitary constant, computed from experiments
      //   // There might be a better way to bound block_size
      //   if (l3_density > 100)
      //     block_size = L3_size
      //   if (l2_density > 100)
      //     block_size = L2_size
      //   if (l1_density > 100)
      //     block_size = L1_size
      //   var numBlocks = numCols / block_size
      //   if (numCols % block_size > 0)
      //     numBlocks = numBlocks + 1
      //   val data = sparsecoo_get_data($self)
      //   val rowIndices = sparsecoo_get_rowindices($self)
      //   val colIndices = sparsecoo_get_colindices($self)

      //   // sort out COO so it's ready to be blocked
      //   val elems = SHashMap[Long,T]()
      //   // remove duplicates by preferring elements further to the right in the array
      //   var i = 0
      //   while (i < $self.nnz) {
      //     if (rowIndices(i) >= 0) {  // removed elements are represented by a negative index
      //       val key = (rowIndices(i).toLong << 32) + colIndices(i).toLong
      //       elems(key) = data(i)
      //     }
      //     i += 1
      //   }
      //   val indices       = array_sort(elems.keys)
      //   val outData       = array_empty[T](array_length(indices))
      //   val outColIndices = array_empty[Int](array_length(indices))
      //   val outRowIndices = array_empty[Int](array_length(indices))
      //   // write to output in sorted order without duplicates
      //   // left-to-right, top-to-bottom
      //   i = 0
      //   while (i < array_length(indices)) {
      //     val colIdx = (indices(i) & unit(0x00000000ffffffffL)).toInt
      //     array_update(outColIndices, i, colIdx)
      //     array_update(outRowIndices, i, (indices(i) >>> 32).toInt)
      //     array_update(outData, i, elems(indices(i)))
      //     i += 1
      //   }

      //   val out_data_blocks = array_empty[ForgeArray[T]](numBlocks)
      //   val out_colIndices_blocks = array_empty[ForgeArray[Int]](numBlocks)
      //   val out_rowIndices_blocks = array_empty[ForgeArray[Int]](numBlocks)
      //   i = 0
      //   var j = 0
      //   var k = 0
      //   while (i < numBlocks) {
      //     val out_data_block = array_empty[T](block_size)
      //     val out_colIndices_block = array_empty[Int](block_size)
      //     val out_rowIndices_block = array_empty[Int](block_size)
      //     while (j < block_size && k < array_length(indices)) {
      //       array_update(out_data_block, j, outData(i))
      //       array_update(out_colIndices_block, j, outColIndices(i))
      //       array_update(out_rowIndices_block, j, outRowIndices(i))
      //       j += 1
      //       k += 1
      //     }
      //     array_update(out_data_blocks, i, out_data_block)
      //     array_update(out_colIndices_blocks, i, out_colIndices_block)
      //     array_update(out_rowIndices_blocks, i, out_rowIndices_block)
      //     j = 0
      //     i += 1
      //   }
      //   val csr_blocks = (0::numBlocks).map({e => 
      //       sparsecoo_to_sparsecsr(sparse_coo_alloc_raw[T](block_size,
      //                                                      $self.numRows,
      //                                                      out_data_blocks(e).unsafeImmutable,
      //                                                      out_rowIndices_blocks(e).unsafeImmutable,
      //                                                      out_colIndices_blocks(e).unsafeImmutable,
      //                                                      out_data_blocks(e).length,
      //                                                      array_empty[T](unit(32))))
      //     })
      //   sparse_block_mat_alloc_raw[T](csr_blocks.unsafeImmutable)
      // }
      infix ("to_sparsedirectedgraph") (Nil :: SparseDirectedGraph(T)) implements composite ${ 
        val data = sparsecoo_get_data($self)
        val rowIndices = sparsecoo_get_rowindices($self)
        val colIndices = sparsecoo_get_colindices($self)
        val nodeProp = sparsecoo_get_nodeprop($self)
        val csr = sparsecoo_to_sparsecsr($self)
        val csc = sparsecoo_to_sparsecsr(sparse_coo_alloc_raw[T]($self.numCols,$self.numRows,data.unsafeImmutable,rowIndices.unsafeImmutable,colIndices.unsafeImmutable,$self.nnz,nodeProp))
        sparse_directed_graph_alloc_raw[T](csr.unsafeImmutable, csc.unsafeImmutable, densevector_fromarray(nodeProp, true))
      }
      infix ("to_sparseundirectedgraph") (Nil :: SparseUndirectedGraph(T)) implements composite ${ 
        val data = sparsecoo_get_data($self)
        val rowIndices = sparsecoo_get_rowindices($self)
        val colIndices = sparsecoo_get_colindices($self)
        val nodeProp = sparsecoo_get_nodeprop($self)
        val csr = sparsecoo_to_sparsecsr($self)
        sparse_undirected_graph_alloc_raw[T](csr.unsafeImmutable, densevector_fromarray(nodeProp, true))
      }

      compiler ("sparsecoo_to_sparsecsr") (Nil :: SparseCSR(T)) implements single ${
        if (sparsecoo_ordered($self, $self.nnz, sparsecoo_get_rowindices($self),sparsecoo_get_colindices($self)))
          sparsecoo_to_csr_ordered($self)
        else
          sparsecoo_to_csr_unordered($self)
      }

      compiler ("sparsecoo_ordered") ((("nnz",MInt),("rowIndices",MArray(MInt)),("colIndices",MArray(MInt))) :: MBoolean) implements single ${
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

      compiler ("sparsecoo_to_csr_ordered") (Nil :: SparseCSR(T)) implements single ${
        val data          = sparsecoo_get_data($self)
        val rowIndices    = sparsecoo_get_rowindices($self)
        val colIndices    = sparsecoo_get_colindices($self)

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

        sparsecoo_to_csr_finalize($self,outData,outColIndices,outRowPtr)
      }

      compiler ("sparsecoo_to_csr_unordered") (Nil :: SparseCSR(T)) implements single ${
        val data          = sparsecoo_get_data($self)
        val rowIndices    = sparsecoo_get_rowindices($self)
        val colIndices    = sparsecoo_get_colindices($self)

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

        sparsecoo_to_csr_finalize($self,outData,outColIndices,outRowPtr)
      }

      compiler ("sparsecoo_to_csr_finalize") ((("outData",MArray(T)),("outColIndices",MArray(MInt)),("outRowPtr",MArray(MInt))) :: SparseCSR(T)) implements single ${
        // finalize rowPtr
        var i = 0
        var acc = 0
        while (i < $self.numRows) {
          acc += outRowPtr(i)
          array_update(outRowPtr, i, acc)
          i += 1
        }
        array_update(outRowPtr, $self.numRows, array_length(outData))

        // -- debug
        // println("indices.length: " + array_length(indices))
        // println("input to coo->csr: ")
        // println("data: " + data)
        // println("colIndices: " + colIndices)
        // println("rowIndices: " + rowIndices)
        //
        // println("output of coo->csr: ")
        // println("data: " + outData)
        // println("colIndices: " + outColIndices)
        // println("rowPtr: " + outRowPtr)

        sparse_csr_alloc_raw[T]($self.numRows,$self.numCols,outData.unsafeImmutable,outColIndices.unsafeImmutable,outRowPtr.unsafeImmutable,array_length(outData))

        // val rowSlicer = array_empty[Int](2*$self.numRows)
        // i = 0
        // while (i < $self.numRows) {
        //   array_update(rowSlicer,   2*i, outRowPtr(i  ))
        //   array_update(rowSlicer, 2*i+1, outRowPtr(i+1))
        //   i += 1
        // }
        // sparse_csr_alloc_raw[T]($self.numRows,$self.numCols,outData.unsafeImmutable,outColIndices.unsafeImmutable,outRowPtr.unsafeImmutable,array_length(outData),rowSlicer.unsafeImmutable)
      }
    }
  }

  def importSparseCSROps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")

    val SparseCOO = lookupTpe("SparseCOO")
    val Sparse = lookupTpe("SparseCSR")
    val SparseRowView = lookupTpe("SparseRowView")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseVector = lookupTpe("SparseVector")
    val SparseVectorView = lookupTpe("SparseVectorView")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val SparseDirectedGraph = lookupTpe("SparseDirectedGraph")
    val SparseUndirectedGraph = lookupTpe("SparseUndirectedGraph")
    
    // TODO: Add CSC fields and allocations

    // data fields
    data(Sparse, ("_numRows", MInt), ("_numCols", MInt), ("_data", MArray(T)), ("_colIndices",MArray(MInt)), ("_rowPtr",MArray(MInt)), ("_nnz",MInt))

    // static methods
    static (Sparse) ("apply", T, MethodSignature(List(MInt, MInt, MArray(T), MArray(MInt), MArray(MInt), MInt), SparseCSR(T))) implements composite ${ sparse_csr_from_raw($0,$1,$2,$3,$4,$5) }
    // helper
    compiler (Sparse) ("sparse_csr_alloc_raw", T, MethodSignature(List(MInt, MInt, MArray(T), MArray(MInt), MArray(MInt), MInt), SparseCSR(T))) implements allocates(Sparse, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5})
    direct (Sparse) ("sparse_csr_from_raw", T, MethodSignature(List(MInt, MInt, MArray(T), MArray(MInt), MArray(MInt), MInt), SparseCSR(T))) implements allocates(Sparse, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5})

    val SparseOps = withTpe (Sparse)
    SparseOps {
      /**
       * Accessors
       */
      infix ("numRows") (Nil :: MInt) implements getter(0, "_numRows")
      infix ("numCols") (Nil :: MInt) implements getter(0, "_numCols")
      infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }
      infix ("nnz") (Nil :: MInt) implements getter(0, "_nnz")
      infix ("nz") (Nil :: DenseVector(T)) implements composite ${ densevector_fromarray(sparse_csr_get_data($self), unit(true)) }

      infix ("toDense") (Nil :: DenseMatrix(T)) implements composite ${
        val out = DenseMatrix[T]($self.numRows, $self.numCols)
        val rowPtr = sparse_csr_get_rowptr($self)
        val colIndices = sparse_csr_get_colindices($self)
        val data = sparse_csr_get_data($self)

        for (i <- 0 :: $self.numRows) {
          for (j <- rowPtr(i) :: rowPtr(i+1)) {
            out(i,colIndices(j)) = data(j)
          }
        }
        // parallel (but irregular), disjoint writes to rows
        // (0::$self.numRows).foreach { i =>
        //   for (j <- rowPtr(i) until rowPtr(i+1)) {
        //     out(i,colIndices(j)) = data(j)
        //   }
        // }
        out.unsafeImmutable
      }

      // $self.toString doesn't work in Delite, since there is no 'self' instance
      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ println($self.makeStr + "\\n") }

      infix ("makeString") (Nil :: MString, TStringable(T)) implements single ${
        val rowPtr = sparse_csr_get_rowptr($self)
        val colIndices = sparse_csr_get_colindices($self)
        val data = sparse_csr_get_data($self)
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
      compiler ("sparse_csr_get_data"      ) (Nil          :: MArray(T)   ) implements getter(0,       "_data")
      compiler ("sparse_csr_get_rowptr"    ) (Nil          :: MArray(MInt)) implements getter(0,     "_rowPtr")
      compiler ("sparse_csr_get_colindices") (Nil          :: MArray(MInt)) implements getter(0, "_colIndices")
      compiler ("sparse_csr_set_numrows"   ) (MInt         :: MUnit, effect = write(0)) implements setter(0,    "_numRows", ${$1})
      compiler ("sparse_csr_set_numcols"   ) (MInt         :: MUnit, effect = write(0)) implements setter(0,    "_numCols", ${$1})
      compiler ("sparse_csr_set_data"      ) (MArray(T)    :: MUnit, effect = write(0)) implements setter(0,       "_data", ${$1})
      compiler ("sparse_csr_set_rowptr"    ) (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0,     "_rowPtr", ${$1})
      compiler ("sparse_csr_set_colindices") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_colIndices", ${$1})
      compiler ("sparse_csr_set_nnz"       ) (MInt         :: MUnit, effect = write(0)) implements setter(0,        "_nnz", ${$1})
      
      infix ("to_coo") (Nil :: SparseCOO(T)) implements composite ${
        val out        = Sparse[T]($self.numCols, $self.numRows)
        val rowPtr     = sparse_csr_get_rowptr($self)
        val colIndices = sparse_csr_get_colindices($self)
        val data       = sparse_csr_get_data($self)
        val rowIndices = DenseVector[Int](array_length(colIndices), false)
        for (i <- 0 until $self.numRows) {
          for (j <- rowPtr(i) until rowPtr(i+1)-1) {
            rowIndices(j) = j - rowPtr(i)
          }
        }
        sparsecoo_set_rowindices(out, rowIndices.get_data)
        sparsecoo_set_colindices(out, colIndices)
        sparsecoo_set_data(out, data)
        sparsecoo_set_nnz(out, $self.nnz)
        out
      }

      // Note the following 3 should only be called if we loaded CSR from file.
      // Note we don't check if M and M^T are the same
      infix ("to_sparsemat") (Nil :: SparseMatrix(T), TArith(T)) implements composite ${
        sparse_matrix_alloc_raw[T]($self.unsafeImmutable)
      }
      infix ("to_sparsedirectedgraph") ((("csrt", SparseCSR(T)), ("nodeProp", DenseVector(T))) :: SparseDirectedGraph(T)) implements composite ${ 
        sparse_directed_graph_alloc_raw[T]($self.unsafeImmutable, csrt.unsafeImmutable, nodeProp)
      }
      // $1 is nodeProp
      infix ("to_sparseundirectedgraph") (("nodeProp", DenseVector(T)) :: SparseUndirectedGraph(T)) implements composite ${ 
        sparse_undirected_graph_alloc_raw[T]($self.unsafeImmutable, nodeProp)
      }

      /**
       * Math
       */

       // SparseMatrixrix implementation of Sparse Math, need to be improved
      compiler ("zipSparseUnion") ((SparseCSR(B), (T,B) ==> R) :: SparseCSR(R), addTpePars = (B,R)) implements single ${
        val aData         = sparse_get_data($self)
        val aColIndices   = sparse_get_colindices($self)
        val aRowPtr       = sparse_get_rowptr($self)
        val bData         = sparse_get_data($1)
        val bColIndices   = sparse_get_colindices($1)
        val bRowPtr       = sparse_get_rowptr($1)
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

        sparse_csr_alloc_raw[R]($self.numRows, $self.numCols, outData.unsafeImmutable, outColIndices.unsafeImmutable, outRowPtr.unsafeImmutable, nnz)
      }
      compiler ("zipSparseIntersect") ((SparseCSR(B), (T,B) ==> R) :: SparseCSR(R), addTpePars = (B,R)) implements single ${
        val aData         = sparse_get_data($self)
        val aColIndices   = sparse_get_colindices($self)
        val aRowPtr       = sparse_get_rowptr($self)
        val bData         = sparse_get_data($1)
        val bColIndices   = sparse_get_colindices($1)
        val bRowPtr       = sparse_get_rowptr($1)
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

        sparse_csr_alloc_raw[R]($self.numRows, $self.numCols, outData.unsafeImmutable, outColIndices.unsafeImmutable, outRowPtr.unsafeImmutable, nnz)
      }
      infix ("mul_vector_helper") ( DenseVector(T) :: DenseVector(T), TArith(T) ) implements composite ${
        fassert($self.numCols == $1.length && !$1.isRow, "dimension mismatch: matrix * vector")
        val out        = DenseVector[T]($self.numRows, false)
        val data       = sparse_get_data($self)
        val rowPtr     = sparse_get_rowptr($self)
        val colIndices = sparse_get_colindices($self)
        val vec        = $1.get_data
        for (i <- 0 :: $self.numRows) {
          for (j <- rowPtr(i) :: rowPtr(i+1)) {
            out(i) = out(i) + data(j) * vec(colIndices(j))
          }
        }
        out
      }

      // Math
      infix ("+")   (SparseCSR(T)      ::      SparseCSR(T), TArith(T)) implements composite ${     zipSparseUnion[T,T,T]($self, $1, (a,b) => a+b) }
      infix ("-")   (SparseCSR(T)      ::      SparseCSR(T), TArith(T)) implements composite ${     zipSparseUnion[T,T,T]($self, $1, (a,b) => a-b) }
      infix ("*:*") (SparseCSR(T)      ::      SparseCSR(T), TArith(T)) implements composite ${ zipSparseIntersect[T,T,T]($self, $1, (a,b) => a*b) }

      infix ("+")   (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense + $1 }
      infix ("-")   (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense - $1 }
      infix ("*:*") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense * $1 }

      infix ("*")   (DenseVector(T) :: DenseVector(T), TArith(T)) implements composite ${$self.mul_vector_helper($1)}

      infix ("+")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense + $1 }
      infix ("-")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense - $1 }
      infix ("*")   (T :: SparseCSR(T), TArith(T)) implements composite ${
        val out_data = $self.nz.map(e => e*$1)
        sparse_csr_alloc_raw($self.numRows, $self.numCols, densevector_raw_data(out_data), sparse_get_colindices($self), sparse_get_rowptr($self), $self.nnz)
      }
      infix ("/")   (T :: SparseCSR(T), TArith(T)) implements composite ${
        val out_data = $self.nz.map(e => e/$1)
        sparse_csr_alloc_raw($self.numRows, $self.numCols, densevector_raw_data(out_data), sparse_get_colindices($self), sparse_get_rowptr($self), $self.nnz)
      }

      infix ("sum") (Nil :: T, TArith(T)) implements composite ${ $self.nz.sum }
      infix ("abs") (Nil :: SparseCSR(T), TArith(T)) implements composite ${
        val out_data = $self.nz.map(e => e.abs)
        sparse_csr_alloc_raw($self.numRows, $self.numCols, densevector_raw_data(out_data), sparse_get_colindices($self), sparse_get_rowptr($self), $self.nnz)
      }

      // TODO: compare different implementations of getRow
      infix ("getRow") ( MInt :: SparseRowView(T) ) implements composite ${
        val data       = sparse_get_data($self)
        val rowPtr     = sparse_get_rowptr($self)
        val colIndices = sparse_get_colindices($self)
        SparseRowView[T](data, colIndices(rowPtr($1)), colIndices(rowPtr($1+1))-colIndices(rowPtr($1)))
      }
      infix ("getRowIndices") ( MInt :: SparseRowView(MInt) ) implements composite ${
        val rowPtr     = sparse_get_rowptr($self)
        val colIndices = sparse_get_colindices($self)
        SparseRowView[Int](colIndices, colIndices(rowPtr($1)), colIndices(rowPtr($1+1))-colIndices(rowPtr($1)))
      }
    }
  }
}
