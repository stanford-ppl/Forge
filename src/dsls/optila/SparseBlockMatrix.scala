package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait SparseBlockMatrixOps {
  this: OptiLADSL =>

  def importSparseBlockMatrixOps() {
    val T = tpePar("T")
    val CSR = lookupTpe("CSR")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val SparseBlockMatrix = lookupTpe("SparseBlockMatrix")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    data(SparseBlockMatrix, ("_csr_blocks", DenseVector(SparseMatrix(T))))
    compiler (SparseBlockMatrix) ("sparse_block_matrix_alloc_raw", T, MethodSignature(DenseVector(SparseMatrix(T)), SparseBlockMatrix(T))) implements allocates(SparseBlockMatrix, ${$0})

    val SparseBlockMatrixOps = withTpe (SparseBlockMatrix)
    SparseBlockMatrixOps {
      compiler ("get_csr_blocks" ) (Nil :: DenseVector(SparseMatrix(T))) implements getter(0, "_csr_blocks")
      infix ("numBlocks") (Nil :: MInt) implements composite ${ get_csr_blocks($self).length }
      infix ("numRows") (Nil :: MInt) implements composite ${
        val csr_blocks = get_csr_blocks($self)
        (0::$self.numBlocks).map({e => csr_blocks(e).numRows}).max
      }
      infix ("numCols") (Nil :: MInt) implements composite ${
        val csr_blocks = get_csr_blocks($self)
        (0::$self.numBlocks).map({e => csr_blocks(e).numCols}).sum
      }
      infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }
      infix ("nnz") (Nil :: MInt) implements composite ${
        val csr_blocks = get_csr_blocks($self)
        (0::$self.numBlocks).map({e => csr_blocks(e).nnz}).sum
      }
      infix ("toDense") (Nil :: DenseMatrix(T)) implements composite ${
        val csr_blocks = get_csr_blocks($self)
        val out_raw_blocks = (0::$self.numBlocks).map({e => csr_blocks(e).toDense})
        DenseMatrix.block(out_raw_blocks)
      }
      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${
        val csr_blocks = get_csr_blocks($self)
        var i = 0
        while (i < $self.numBlocks) {
          csr_blocks(i).pprint
        }
      }
      // These operations no longer make sense, unless we want to allow converting back and forth
      // infix ("+")   (SparseMat(T)   ::   SparseMat(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) + get_csr($1)) }
      // infix ("-")   (SparseMat(T)   ::   SparseMat(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) - get_csr($1)) }
      // infix ("*:*") (SparseMat(T)   ::   SparseMat(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) *:* get_csr($1)) }
      infix ("+")   (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense + $1 }
      infix ("-")   (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense - $1 }
      infix ("*:*") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense * $1 }
      infix ("*")   (DenseVector(T) :: DenseVector(T), TArith(T)) implements composite ${
        val csr_blocks = get_csr_blocks($self)
        val block_size = csr_blocks(0).numCols
        val vector_blocks = (0::$self.numBlocks).map({e =>
            $1.slice(e*block_size, (e+1)*block_size)
          })
        val out_block = csr_blocks(0).asInstanceOf[Rep[SparseMatrix[T]]] * vector_blocks(0).asInstanceOf[Rep[DenseVector[T]]]
        var i = 1
        while (i < $self.numBlocks) {
          out_block += csr_blocks(i).asInstanceOf[Rep[SparseMatrix[T]]] * vector_blocks(i).asInstanceOf[Rep[DenseVector[T]]]
          i += 1
        }
        out_block
      }
      infix ("+")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense + $1 }
      infix ("-")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense - $1 }
      infix ("*")   (T :: SparseBlockMatrix(T), TArith(T)) implements composite ${
        val csr_blocks = get_csr_blocks($self)
        val out_raw_blocks = (0::$self.numBlocks).map({e => csr_blocks(e) * $1})
        sparse_block_matrix_alloc_raw[T](out_raw_blocks)
      }
      infix ("/")   (T :: SparseBlockMatrix(T), TArith(T)) implements composite ${
        val csr_blocks = get_csr_blocks($self)
        val out_raw_blocks = (0::$self.numBlocks).map({e => csr_blocks(e) / $1})
        sparse_block_matrix_alloc_raw[T](out_raw_blocks)
      }
      infix ("sum") (Nil :: T, TArith(T)) implements composite ${
        val csr_blocks = get_csr_blocks($self)
        (0::$self.numBlocks).map({e => csr_blocks(e).sum}).sum
      }
      infix ("abs") (Nil :: SparseBlockMatrix(T), TArith(T)) implements composite ${
        val csr_blocks = get_csr_blocks($self)
        val out_raw_blocks = (0::$self.numBlocks).map({e => csr_blocks(e).abs})
        sparse_block_matrix_alloc_raw[T](out_raw_blocks)
      }
    }
  }
}
