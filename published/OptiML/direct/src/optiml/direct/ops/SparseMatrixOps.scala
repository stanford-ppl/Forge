package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait SparseMatrixOps extends Base {
  this: OptiML => 

  object SparseMatrix {
    def apply[T:Typ](__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload24) = sparsematrix_object_apply[T](__arg0,__arg1)(implicitly[Typ[T]],__pos)
    def fromElements[T:Typ](numRows: Rep[Int],numCols: Rep[Int],nzElements: Rep[DenseVector[T]],nzRowIndices: Rep[DenseVector[Int]],nzColIndices: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrix_object_fromelements[T](numRows,numCols,nzElements,nzRowIndices,nzColIndices)(implicitly[Typ[T]],__pos)
    def diag[T:Arith:Typ](__arg0: Rep[Int],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrix_object_diag[T](__arg0,__arg1)(implicitly[Arith[T]],implicitly[Typ[T]],__pos)
    def identity(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_object_identity(__arg0,__arg1)(__pos,overload3)
    def identity(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload4): Rep[SparseMatrix[Double]] = { SparseMatrix.identity(__arg0,__arg0) }
    def zeros(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_object_zeros(__arg0,__arg1)(__pos)
    def zerosf(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_object_zerosf(__arg0,__arg1)(__pos)
    def rand(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_object_rand(__arg0,__arg1,__arg2)(__pos)
    def randf(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_object_randf(__arg0,__arg1,__arg2)(__pos)
    def randn(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrix_object_randn(__arg0,__arg1,__arg2)(__pos)
    def randnf(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrix_object_randnf(__arg0,__arg1,__arg2)(__pos)
  }

  def __equal[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload19) = sparsematrix___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload19)
  def __equal[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload20) = sparsematrix___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload20)

  implicit def repToSparseMatrixSparseMatrixOpsCls[T:Typ](x: Rep[SparseMatrix[T]])(implicit __pos: SourceContext) = new SparseMatrixSparseMatrixOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToSparseMatrixSparseMatrixOpsCls[T:Typ](x: Var[SparseMatrix[T]])(implicit __pos: SourceContext) = new SparseMatrixSparseMatrixOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class SparseMatrixSparseMatrixOpsCls[T:Typ](val self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext) {
    def numRows(implicit __pos: SourceContext,__imp1: Overload4) = sparsematrix_numrows[T](self)(implicitly[Typ[T]],__pos)
    def numCols(implicit __pos: SourceContext,__imp1: Overload4) = sparsematrix_numcols[T](self)(implicitly[Typ[T]],__pos)
    def size(implicit __pos: SourceContext,__imp1: Overload13) = sparsematrix_size[T](self)(implicitly[Typ[T]],__pos)
    def nnz(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_nnz[T](self)(implicitly[Typ[T]],__pos)
    def nz(asRow: Rep[Boolean] = unit(true))(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_nz[T](self,asRow)(implicitly[Typ[T]],__pos)
    def apply(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload28) = sparsematrix_apply[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos,overload28)
    def apply(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload29) = { self.getRow(__arg1) }
    def apply(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload30) = { self.getRows(__arg1) }
    def apply(__arg1: Rep[IndexVector],__arg2: IndexWildcard)(implicit __pos: SourceContext,__imp1: ROverload31) = { self.getRows(__arg1) }
    def apply(rows: Rep[IndexVector],cols: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload32) = sparsematrix_apply[T](self,rows,cols)(implicitly[Typ[T]],__pos,overload32)
    def apply(__arg1: IndexWildcard,__arg2: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload33) = { self.getCols(__arg2) }
    def rowIndices(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_rowindices[T](self)(implicitly[Typ[T]],__pos)
    def colIndices(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_colindices[T](self)(implicitly[Typ[T]],__pos)
    def nzRows(implicit __pos: SourceContext) = { IndexVector(self.rowIndices.distinct) }
    def nzCols(implicit __pos: SourceContext) = { IndexVector(self.colIndices.distinct) }
    def getRow(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_getrow[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def getRows(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload5) = sparsematrix_getrows[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def getCol(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_getcol[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def getCols(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload5) = sparsematrix_getcols[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def slice(startRow: Rep[Int],endRow: Rep[Int],startCol: Rep[Int],endCol: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload6) = { self(startRow::endRow, startCol::endCol) }
    def sliceRows(start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload3) = { self.getRows(start::end) }
    def sliceCols(start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload3) = { self.getCols(start::end) }
    def t(implicit __pos: SourceContext,__imp1: Overload4) = sparsematrix_t[T](self)(implicitly[Typ[T]],__pos)
    def Clone(implicit __pos: SourceContext,__imp1: Overload6) = sparsematrix_clone[T](self)(implicitly[Typ[T]],__pos)
    def mutable()(implicit __pos: SourceContext,__imp1: Overload12) = sparsematrix_mutable[T](self)(implicitly[Typ[T]],__pos)
    def toDense(implicit __pos: SourceContext,__imp1: Overload5) = sparsematrix_todense[T](self)(implicitly[Typ[T]],__pos)
    def pprint()(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload6) = sparsematrix_pprint[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def makeDimsStr(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_makedimsstr[T](self)(implicitly[Typ[T]],__pos)
    def makeString(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload5) = sparsematrix_makestring[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def toString(implicit __imp0: Overload9) = sparsematrix_tostring[T](self)
    def +(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload196) = sparsematrix_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload196)
    def +(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload197) = sparsematrix_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload197)
    def +(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload198) = sparsematrix_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload198)
    def -(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload180) = sparsematrix_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload180)
    def -(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload181) = sparsematrix_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload181)
    def -(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload182) = sparsematrix_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload182)
    def *:*(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload23) = sparsematrix_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload23)
    def *:*(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload24) = sparsematrix_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload24)
    def *(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload200) = sparsematrix_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload200)
    def *(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload201) = sparsematrix_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload201)
    def *(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload202) = sparsematrix_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload202)
    def /(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload171) = sparsematrix_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload171)
    def /(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload172) = sparsematrix_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload172)
    def /(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload173) = sparsematrix_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload173)
    def sum(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5) = sparsematrix_sum[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def mean(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload5) = sparsematrix_mean[T](self)(implicitly[Typ[T]],__pos,conv)
    def abs(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload15) = sparsematrix_abs[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def sumRows(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = sparsematrix_sumrows[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def sumCols(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = sparsematrix_sumcols[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def min(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload11) = sparsematrix_min[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def max(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload11) = sparsematrix_max[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def minRows(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload3) = sparsematrix_minrows[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def minCols(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload3) = sparsematrix_mincols[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def maxRows(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload3) = sparsematrix_maxrows[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def maxCols(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload3) = sparsematrix_maxcols[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def mapnz[R:Typ](__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrix_mapnz[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def foreachnz(__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrix_foreachnz[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def countnz(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrix_countnz[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def mapRowsToVector[R:Typ](__arg1: (Rep[SparseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_maprowstovector[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def mapRowsToDenseVector[R:Typ](__arg1: (Rep[SparseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext) = sparsematrix_maprowstodensevector[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def mapColsToVector[R:Typ](__arg1: (Rep[SparseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_mapcolstovector[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def findRows(__arg1: (Rep[SparseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_findrows[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def findCols(__arg1: (Rep[SparseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_findcols[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def filterRows(__arg1: (Rep[SparseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_filterrows[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def filterCols(__arg1: (Rep[SparseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_filtercols[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def foreachRow(__arg1: (Rep[SparseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_foreachrow[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def foreachCol(__arg1: (Rep[SparseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrix_foreachcol[T](self,__arg1)(implicitly[Typ[T]],__pos)
  }



  def sparsematrix_object_apply[T:Typ](__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[SparseMatrixBuildable[T]]
  def sparsematrix_object_fromelements[T:Typ](numRows: Rep[Int],numCols: Rep[Int],nzElements: Rep[DenseVector[T]],nzRowIndices: Rep[DenseVector[Int]],nzColIndices: Rep[DenseVector[Int]])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def sparsematrix_object_diag[T:Arith:Typ](__arg0: Rep[Int],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def sparsematrix_object_identity(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3): Rep[SparseMatrix[Double]]
  def sparsematrix_object_zeros(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[SparseMatrix[Double]]
  def sparsematrix_object_zerosf(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[SparseMatrix[Float]]
  def sparsematrix_object_rand(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Double])(implicit __pos: SourceContext): Rep[SparseMatrix[Double]]
  def sparsematrix_object_randf(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Double])(implicit __pos: SourceContext): Rep[SparseMatrix[Float]]
  def sparsematrix_object_randn(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Double])(implicit __pos: SourceContext): Rep[SparseMatrix[Double]]
  def sparsematrix_object_randnf(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Double])(implicit __pos: SourceContext): Rep[SparseMatrix[Float]]
  def sparsematrix_numrows[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsematrix_numcols[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsematrix_size[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsematrix_nnz[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsematrix_nz[T:Typ](self: Rep[SparseMatrix[T]],asRow: Rep[Boolean] = unit(true))(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def sparsematrix_apply[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload28): Rep[T]
  def sparsematrix_apply[T:Typ](self: Rep[SparseMatrix[T]],rows: Rep[IndexVector],cols: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload32): Rep[SparseMatrix[T]]
  def sparsematrix_rowindices[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def sparsematrix_colindices[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def sparsematrix_getrow[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[SparseVectorView[T]]
  def sparsematrix_getrows[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def sparsematrix_getcol[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[SparseVectorView[T]]
  def sparsematrix_getcols[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def sparsematrix_t[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def sparsematrix_clone[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def sparsematrix_mutable[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[SparseMatrixBuildable[T]]
  def sparsematrix_todense[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def sparsematrix_pprint[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[Unit]
  def sparsematrix_makedimsstr[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[String]
  def sparsematrix_makestring[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[String]
  def sparsematrix_tostring[T:Typ](self: Rep[SparseMatrix[T]]): Rep[String]
  def sparsematrix_pl[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload196): Rep[SparseMatrix[T]]
  def sparsematrix_pl[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload197): Rep[DenseMatrix[T]]
  def sparsematrix_pl[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload198): Rep[DenseMatrix[T]]
  def sparsematrix_sub[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload180): Rep[SparseMatrix[T]]
  def sparsematrix_sub[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload181): Rep[DenseMatrix[T]]
  def sparsematrix_sub[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload182): Rep[DenseMatrix[T]]
  def sparsematrix_mulclnmul[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload23): Rep[SparseMatrix[T]]
  def sparsematrix_mulclnmul[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload24): Rep[DenseMatrix[T]]
  def sparsematrix_mul[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload200): Rep[SparseMatrix[T]]
  def sparsematrix_mul[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload201): Rep[DenseVector[T]]
  def sparsematrix_mul[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload202): Rep[DenseMatrix[T]]
  def sparsematrix_div[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload171): Rep[SparseMatrix[T]]
  def sparsematrix_div[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload172): Rep[DenseMatrix[T]]
  def sparsematrix_div[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload173): Rep[SparseMatrix[T]]
  def sparsematrix_sum[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def sparsematrix_mean[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double]): Rep[Double]
  def sparsematrix_abs[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[SparseMatrix[T]]
  def sparsematrix_sumrows[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[SparseVector[T]]
  def sparsematrix_sumcols[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[SparseVector[T]]
  def sparsematrix_min[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def sparsematrix_max[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def sparsematrix_minrows[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[SparseVector[T]]
  def sparsematrix_mincols[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[SparseVector[T]]
  def sparsematrix_maxrows[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[SparseVector[T]]
  def sparsematrix_maxcols[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[SparseVector[T]]
  def sparsematrix___equal[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload19): Rep[Boolean]
  def sparsematrix___equal[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload20): Rep[Boolean]
  def sparsematrix_mapnz[T:Typ,R:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext): Rep[SparseMatrix[R]]
  def sparsematrix_foreachnz[T:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_countnz[T:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Int]
  def sparsematrix_maprowstovector[T:Typ,R:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[SparseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext): Rep[SparseVector[R]]
  def sparsematrix_maprowstodensevector[T:Typ,R:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[SparseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def sparsematrix_mapcolstovector[T:Typ,R:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[SparseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext): Rep[SparseVector[R]]
  def sparsematrix_findrows[T:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[SparseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def sparsematrix_findcols[T:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[SparseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def sparsematrix_filterrows[T:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[SparseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def sparsematrix_filtercols[T:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[SparseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def sparsematrix_foreachrow[T:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[SparseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_foreachcol[T:Typ](self: Rep[SparseMatrix[T]],__arg1: (Rep[SparseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
}
trait SparseMatrixCompilerOps extends SparseMatrixOps {
  this: OptiML => 

  def sparsematrix_coo_alloc_raw[T:Typ](numRows: Rep[Int],numCols: Rep[Int],nzElements: Rep[ForgeArray[T]],rowIndices: Rep[ForgeArray[Int]],colIndices: Rep[ForgeArray[Int]],nnz: Rep[Int])(implicit __pos: SourceContext): Rep[SparseMatrixBuildable[T]]
  def sparsematrix_csr_alloc_raw[T:Typ](__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]],__arg3: Rep[ForgeArray[Int]],__arg4: Rep[ForgeArray[Int]],__arg5: Rep[Int])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def sparsematrix_rand[T:Typ](numRows: Rep[Int],numCols: Rep[Int],sparsity: Rep[Double],gen: (Rep[Int]) => Rep[T])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def sparsematrix_csr_find_offset[T:Typ](self: Rep[SparseMatrix[T]],row: Rep[Int],col: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def sparsematrix_vview[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[Long],__arg2: Rep[Int],__arg3: Rep[Int],__arg4: Rep[Boolean])(implicit __pos: SourceContext): Rep[SparseVectorView[T]]
  def sparsematrix_csr_data[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[ForgeArray[T]]
  def sparsematrix_csr_rowptr[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def sparsematrix_csr_colindices[T:Typ](self: Rep[SparseMatrix[T]])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def sparsematrix_csr_set_numrows[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_csr_set_numcols[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_csr_set_data[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[ForgeArray[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_csr_set_rowptr[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[ForgeArray[Int]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_csr_set_colindices[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[ForgeArray[Int]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_csr_set_nnz[T:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparematrix_csr_update[T:Typ](self: Rep[SparseMatrix[T]],i: Rep[Int],j: Rep[Int],y: Rep[T])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_csr_ensureextra[T:Typ](self: Rep[SparseMatrix[T]],extra: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_csr_realloc[T:Typ](self: Rep[SparseMatrix[T]],minLen: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_csr_insertspace[T:Typ](self: Rep[SparseMatrix[T]],pos: Rep[Int],len: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def zipMatrixUnion[T:Typ,B:Typ,R:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[SparseMatrix[B]],__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext): Rep[SparseMatrix[R]]
  def zipMatrixIntersect[T:Typ,B:Typ,R:Typ](self: Rep[SparseMatrix[T]],__arg1: Rep[SparseMatrix[B]],__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext): Rep[SparseMatrix[R]]
}

