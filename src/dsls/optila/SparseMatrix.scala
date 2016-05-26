package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait SparseMatrixOps {
  this: OptiLADSL =>

  def importSparseMatrixOps() {
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
    data(SparseMatrix, ("_csr", CSR(T)), ("_csc", CSR(T)))
    compiler (SparseMatrix) ("sparse_matrix_alloc_raw", T, MethodSignature(List(CSR(T), CSR(T)), SparseMatrix(T))) implements allocates(SparseMatrix, ${$0}, ${$1})

    val SparseMatrixOps = withTpe (SparseMatrix)
    SparseMatrixOps {
      compiler ("get_csr" ) (Nil :: CSR(T)) implements getter(0, "_csr")
      compiler ("get_csc" ) (Nil :: CSR(T)) implements getter(0, "_csc")
      compiler ("set_csr" ) (CSR(T) :: MUnit, effect = write(0)) implements setter(0,  "_csr", ${$1})
      compiler ("set_csc" ) (CSR(T) :: MUnit, effect = write(0)) implements setter(0,  "_csc", ${$1})
      infix ("apply") ((MInt,MInt) :: T) implements composite ${ get_csr($self).apply($1,$2) }
      infix ("apply") (MInt :: SparseVectorView(T)) implements single ${ SparseVectorView[T](get_csr($self), $1.toLong*$self.numCols, 1, $self.numCols, true) }
      infix ("getRow") (MInt :: SparseVectorView(T)) implements composite ${ $self.apply($1) }
      infix ("getCol") (MInt :: SparseVectorView(T)) implements composite ${ SparseVectorView[T](get_csc($self), $1.toLong*$self.numRows, 1, $self.numRows, true) }
      infix ("numRows") (Nil :: MInt) implements composite ${ get_csr($self).numRows }
      infix ("numCols") (Nil :: MInt) implements composite ${ get_csr($self).numCols }
      infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }
      infix ("nnz") (Nil :: MInt) implements composite ${ get_csr($self).nnz }
      infix ("toDense") (Nil :: DenseMatrix(T)) implements composite ${ get_csr($self).toDense }
      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ get_csr($self).pprint }
      infix ("pprint_t") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ get_csc($self).pprint }
      infix ("t") (Nil :: SparseMatrix(T)) implements composite ${
        sparse_matrix_alloc_raw(get_csc($self).unsafeImmutable, get_csr($self).unsafeImmutable)
      }
      infix ("+")   (SparseMatrix(T) :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) + get_csr($1), get_csc($self) + get_csc($1)) }
      infix ("-")   (SparseMatrix(T) :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) - get_csr($1), get_csc($self) - get_csc($1)) }
      infix ("*:*") (SparseMatrix(T) :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self) *:* get_csr($1), get_csc($self) *:* get_csc($1)) }
      infix ("+")   (DenseMatrix(T)  :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self) + $1 }
      infix ("-")   (DenseMatrix(T)  :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self) - $1 }
      infix ("*:*") (DenseMatrix(T)  :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self).toDense * $1 }
      infix ("*")   (DenseVector(T)  :: DenseVector(T), TArith(T)) implements composite ${ get_csr($self)*$1 }
      infix ("+")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self).toDense + $1 }
      infix ("-")   (T :: DenseMatrix(T), TArith(T)) implements composite ${ get_csr($self).toDense - $1 }
      infix ("*")   (T :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self)*$1, get_csc($self)*$1) }
      infix ("/")   (T :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self)/$1, get_csc($self)/$1) }
      infix ("sum") (Nil :: T, TArith(T)) implements composite ${ get_csr($self).sum }
      infix ("abs") (Nil :: SparseMatrix(T), TArith(T)) implements composite ${ sparse_matrix_alloc_raw[T](get_csr($self).abs, get_csc($self).abs) }
      infix ("mean") (Nil :: MDouble, ("conv",T ==> MDouble)) implements composite ${ get_csr($self).mapnz(conv).sum / $self.size }
      infix ("min") (Nil :: T, (TOrdering(T), THasMinMax(T))) implements composite ${ get_csr($self).min }
      infix ("max") (Nil :: T, (TOrdering(T), THasMinMax(T))) implements composite ${ get_csr($self).max }
      infix ("mapnz") ((T ==> R) :: SparseMatrix(R), addTpePars = R) implements composite ${
        sparse_matrix_alloc_raw( get_csr($self).mapnz($1), get_csc($self).mapnz($1) )
      }
      infix ("makeDimsStr") (Nil :: MString) implements single ${
        $self.numRows + " x " + $self.numCols + ", " + $self.nnz + " nnz"
      }
      infix ("nzRows") (Nil :: IndexVector) implements composite ${ get_csc($self).nzCols }
      infix ("nzCols") (Nil :: IndexVector) implements composite ${ get_csr($self).nzCols }
      direct ("__equal") (SparseMatrix(T) :: MBoolean) implements composite ${ get_csr($self) == get_csr($1) }
      direct ("__equal") (SparseMatrixNoTranspose(T) :: MBoolean) implements composite ${ get_csr($self) == get_csr_no_transpose($1) }
      direct ("__equal") (DenseMatrix(T) :: MBoolean) implements composite ${ get_csr($self) == $1 }
      infix ("foreachnz") ((T ==> MUnit) :: MUnit) implements composite ${
        get_csr($self).foreachnz($1)
        get_csc($self).foreachnz($1)
      }
      infix ("countnz") ((T ==> MBoolean) :: MInt) implements composite ${ get_csr($self).countnz($1) }
      infix ("mapRowsToDenseVector") ((SparseRowView(T) ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
         get_csr($self).mapRowsToDenseVector($1)
      }
      infix ("mapColsToDenseVector") ((SparseRowView(T) ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
         get_csc($self).mapRowsToDenseVector($1)
      }
      infix ("findRows") ((SparseVectorView(T) ==> MBoolean) :: IndexVector) implements composite ${
        IndexVector($self.nzRows.filter(i => $1($self(i))))
      }
      infix ("findCols") ((SparseVectorView(T) ==> MBoolean) :: IndexVector) implements composite ${
        IndexVector($self.nzCols.filter(i => $1($self.getCol(i))))
      }
    }
  }
}
