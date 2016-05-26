package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait SparseMatrixNoTransposeOps {
  this: OptiLADSL =>

  def importSparseMatrixNoTransposeOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val CSR = lookupTpe("CSR")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val SparseMatrixNoTranspose = lookupTpe("SparseMatrixNoTranspose")
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseRowView = lookupTpe("SparseRowView")
    val SparseVectorView = lookupTpe("SparseVectorView")
    data(SparseMatrixNoTranspose, ("_csr", CSR(T)))
    compiler (SparseMatrixNoTranspose) ("sparse_matrix_no_transpose_alloc_raw", T, MethodSignature(CSR(T), SparseMatrixNoTranspose(T))) implements allocates(SparseMatrixNoTranspose, ${$0})

    val SparseMatrixNoTransposeOps = withTpe (SparseMatrixNoTranspose)
    SparseMatrixNoTransposeOps {
      compiler ("get_csr_no_transpose" ) (Nil :: CSR(T)) implements getter(0, "_csr")
      compiler ("set_csr_no_transpose" ) (CSR(T) :: MUnit, effect = write(0)) implements setter(0,  "_csr", ${$1})
      infix ("apply") ((MInt,MInt) :: T) implements composite ${ get_csr_no_transpose($self).apply($1,$2) }
      infix ("apply") (MInt :: SparseVectorView(T)) implements single ${ SparseVectorView[T](get_csr_no_transpose($self), $1.toLong*$self.numCols, 1, $self.numCols, true) }
      infix ("getRow") (MInt :: SparseVectorView(T)) implements composite ${ $self.apply($1) }
      infix ("numRows") (Nil :: MInt) implements composite ${ get_csr_no_transpose($self).numRows }
      infix ("numCols") (Nil :: MInt) implements composite ${ get_csr_no_transpose($self).numCols }
      infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }
      infix ("nnz") (Nil :: MInt) implements composite ${ get_csr_no_transpose($self).nnz }
      infix ("toDense") (Nil :: DenseMatrix(T)) implements composite ${ get_csr_no_transpose($self).toDense }
      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ get_csr_no_transpose($self).pprint }
      infix ("+")   (SparseMatrixNoTranspose(T) :: SparseMatrixNoTranspose(T), TArith(T)) implements composite ${ sparse_matrix_no_transpose_alloc_raw[T](get_csr_no_transpose($self) + get_csr_no_transpose($1)) }
      infix ("-")   (SparseMatrixNoTranspose(T) :: SparseMatrixNoTranspose(T), TArith(T)) implements composite ${ sparse_matrix_no_transpose_alloc_raw[T](get_csr_no_transpose($self) - get_csr_no_transpose($1)) }
      infix ("*:*") (SparseMatrixNoTranspose(T) :: SparseMatrixNoTranspose(T), TArith(T)) implements composite ${ sparse_matrix_no_transpose_alloc_raw[T](get_csr_no_transpose($self) *:* get_csr_no_transpose($1)) }
      infix ("+")   (DenseMatrix(T)  :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr_no_transpose($self) + $1 }
      infix ("-")   (DenseMatrix(T)  :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr_no_transpose($self) - $1 }
      infix ("*:*") (DenseMatrix(T)  :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr_no_transpose($self).toDense * $1 }
      infix ("*")   (DenseVector(T)  :: DenseVector(T), TArith(T)) implements composite ${ get_csr_no_transpose($self)*$1 }
      infix ("+")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr_no_transpose($self).toDense + $1 }
      infix ("-")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr_no_transpose($self).toDense - $1 }
      infix ("*")   (T :: SparseMatrixNoTranspose(T), TArith(T)) implements composite ${ sparse_matrix_no_transpose_alloc_raw[T](get_csr_no_transpose($self)*$1) }
      infix ("/")   (T :: SparseMatrixNoTranspose(T), TArith(T)) implements composite ${ sparse_matrix_no_transpose_alloc_raw[T](get_csr_no_transpose($self)/$1) }
      infix ("sum") (Nil :: T, TArith(T)) implements composite ${ get_csr_no_transpose($self).sum }
      infix ("abs") (Nil :: SparseMatrixNoTranspose(T), TArith(T)) implements composite ${ sparse_matrix_no_transpose_alloc_raw[T](get_csr_no_transpose($self).abs) }
      infix ("mean") (Nil :: MDouble, ("conv",T ==> MDouble)) implements composite ${ get_csr_no_transpose($self).mapnz(conv).sum / $self.size }
      infix ("min") (Nil :: T, (TOrdering(T), THasMinMax(T))) implements composite ${ get_csr_no_transpose($self).min }
      infix ("max") (Nil :: T, (TOrdering(T), THasMinMax(T))) implements composite ${ get_csr_no_transpose($self).max }
      infix ("mapnz") ((T ==> R) :: SparseMatrixNoTranspose(R), addTpePars = R) implements composite ${
        sparse_matrix_no_transpose_alloc_raw( get_csr_no_transpose($self).mapnz($1) )
      }
      infix ("makeDimsStr") (Nil :: MString) implements single ${
        $self.numRows + " x " + $self.numCols + ", " + $self.nnz + " nnz"
      }
      infix ("nzRows") (Nil :: IndexVector) implements composite ${ get_csr_no_transpose($self).nzRows }
      infix ("nzCols") (Nil :: IndexVector) implements composite ${ get_csr_no_transpose($self).nzCols }
      direct ("__equal") (SparseMatrix(T) :: MBoolean) implements composite ${ get_csr_no_transpose($self) == get_csr($1) }
      direct ("__equal") (SparseMatrixNoTranspose(T) :: MBoolean) implements composite ${ get_csr_no_transpose($self) == get_csr_no_transpose($1) }
      direct ("__equal") (DenseMatrix(T) :: MBoolean) implements composite ${ get_csr_no_transpose($self) == $1 }
      infix ("foreachnz") ((T ==> MUnit) :: MUnit) implements composite ${ get_csr_no_transpose($self).foreachnz($1) }
      infix ("countnz") ((T ==> MBoolean) :: MInt) implements composite ${ get_csr_no_transpose($self).countnz($1) }
      infix ("mapRowsToDenseVector") ((SparseRowView(T) ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
         get_csr_no_transpose($self).mapRowsToDenseVector($1)
      }
    }
  }
}
