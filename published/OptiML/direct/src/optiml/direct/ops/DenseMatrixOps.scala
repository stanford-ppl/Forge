package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DenseMatrixOpsBase extends Base {
  this: OptiML => 

  implicit def dist(__arg0: Rep[DenseMatrix[Double]],__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload3): Rep[Double] = { dist(__arg0,__arg1,ABS) }

}

trait DenseMatrixOps extends DenseMatrixOpsBase {
  this: OptiML => 

  object DenseMatrix {
    def apply[T:Typ](__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload6) = densematrix_object_apply[T](__arg0,__arg1)(implicitly[Typ[T]],__pos,overload6)
    def apply[T:Typ](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload7) = densematrix_object_apply[T](__arg0,__arg1,__arg2)(implicitly[Typ[T]],__pos,overload7)
    def apply[T:Typ](__arg0: Rep[DenseVector[DenseVector[T]]])(implicit __pos: SourceContext,__imp1: Overload8) = densematrix_object_apply[T](__arg0)(implicitly[Typ[T]],__pos,overload8)
    def apply[T:Typ](__arg0: Rep[DenseVector[DenseVectorView[T]]])(implicit __pos: SourceContext,__imp1: Overload9) = densematrix_object_apply[T](__arg0)(implicitly[Typ[T]],__pos,overload9)
    def apply[T:Typ](__arg0: Rep[DenseVector[T]]*)(implicit __pos: SourceContext,__imp1: Overload10) = densematrix_object_apply[T](__arg0)(implicitly[Typ[T]],__pos,overload10)
    def block[T:Typ](__arg0: Rep[DenseVector[DenseMatrix[T]]]*)(implicit __pos: SourceContext) = densematrix_object_block[T](__arg0)(implicitly[Typ[T]],__pos)
    def diag[T:Arith:Typ](__arg0: Rep[Int],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_object_diag[T](__arg0,__arg1)(implicitly[Arith[T]],implicitly[Typ[T]],__pos)
    def identity(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_object_identity(__arg0,__arg1)(__pos,overload1)
    def identity(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload2): Rep[DenseMatrix[Double]] = { DenseMatrix.identity(__arg0,__arg0) }
    def zeros(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_object_zeros(__arg0,__arg1)(__pos)
    def zerosf(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_object_zerosf(__arg0,__arg1)(__pos)
    def ones(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_object_ones(__arg0,__arg1)(__pos)
    def onesf(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_object_onesf(__arg0,__arg1)(__pos)
    def rand(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_object_rand(__arg0,__arg1)(__pos)
    def randf(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_object_randf(__arg0,__arg1)(__pos)
    def randn(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_object_randn(__arg0,__arg1)(__pos)
    def randnf(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_object_randnf(__arg0,__arg1)(__pos)
  }

  def dist(__arg0: Rep[DenseMatrix[Double]],__arg1: Rep[DenseMatrix[Double]],__arg2: DistanceMetric)(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_dist(__arg0,__arg1,__arg2)(__pos)
  def norm(__arg0: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload1): Rep[Double] = { norm(__arg0,L2) }
  def norm(__arg0: Rep[DenseMatrix[Double]],__arg1: NormId)(implicit __pos: SourceContext,__imp1: Overload2) = densematrix_norm(__arg0,__arg1)(__pos,overload2)
  def densematrix_fromarray[T:Typ](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext) = densematrix_densematrix_fromarray[T](__arg0,__arg1,__arg2)(implicitly[Typ[T]],__pos)
  def densematrix_fromfunc[T:Typ](__arg0: Rep[Int],__arg1: Rep[Int],__arg2: (Rep[Int],Rep[Int]) => Rep[T])(implicit __pos: SourceContext) = densematrix_densematrix_fromfunc[T](__arg0,__arg1,__arg2)(implicitly[Typ[T]],__pos)
  def diag[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]] = { __arg0.diag }
  def triu[T:Arith:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]] = { __arg0.triu }
  def tril[T:Arith:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]] = { __arg0.tril }
  def __equal[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload1)
  def __equal[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp1: Overload2) = densematrix___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload2)
  def __equal[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload3) = densematrix___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload3)
  def densematrix_raw_apply[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext) = densematrix_densematrix_raw_apply[T](self,__arg1)(implicitly[Typ[T]],__pos)
  def densematrix_raw_update[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __pos: SourceContext) = densematrix_densematrix_raw_update[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)

  implicit def repToDenseMatrixDenseMatrixOpsCls[T:Typ](x: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = new DenseMatrixDenseMatrixOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToDenseMatrixDenseMatrixOpsCls[T:Typ](x: Var[DenseMatrix[T]])(implicit __pos: SourceContext) = new DenseMatrixDenseMatrixOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class DenseMatrixDenseMatrixOpsCls[T:Typ](val self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) {
    def flattenToVector(implicit __pos: SourceContext) = densematrix_flattentovector[T](self)(implicitly[Typ[T]],__pos)
    def numRows(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_numrows[T](self)(implicitly[Typ[T]],__pos)
    def numCols(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_numcols[T](self)(implicitly[Typ[T]],__pos)
    def size(implicit __pos: SourceContext,__imp1: Overload3) = densematrix_size[T](self)(implicitly[Typ[T]],__pos)
    def apply(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3) = densematrix_apply[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos,overload3)
    def vview(start: Rep[Int],stride: Rep[Int],length: Rep[Int],isRow: Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_vectorview[T](self,start,stride,length,isRow)(implicitly[Typ[T]],__pos)
    def mview(startRow: Rep[Int],endRow: Rep[Int],startCol: Rep[Int],endCol: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_mview[T](self,startRow,endRow,startCol,endCol)(implicitly[Typ[T]],__pos)
    def getRow(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_getrow[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def getCol(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_getcol[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def slice(startRow: Rep[Int],endRow: Rep[Int],startCol: Rep[Int],endCol: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_slice[T](self,startRow,endRow,startCol,endCol)(implicitly[Typ[T]],__pos)
    def diag(implicit __pos: SourceContext) = densematrix_diag[T](self)(implicitly[Typ[T]],__pos)
    def triu(implicit __pos: SourceContext,__imp0: Arith[T]) = densematrix_triu[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def tril(implicit __pos: SourceContext,__imp0: Arith[T]) = densematrix_tril[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def Clone(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_clone[T](self)(implicitly[Typ[T]],__pos)
    def toSparse(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_tosparse[T](self)(implicitly[Typ[T]],__pos)
    def toArray(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_toarray[T](self)(implicitly[Typ[T]],__pos)
    def update(__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[T])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_update[T](self,__arg1,__arg2,__arg3)(implicitly[Typ[T]],__pos,overload1)
    def update(__arg1: Rep[Int],__arg2: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload2) = densematrix_update[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos,overload2)
    def updateRow(__arg1: Rep[Int],__arg2: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_updaterow[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos,overload1)
    def updateCol(__arg1: Rep[Int],__arg2: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_updatecol[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos,overload1)
    def update(__arg1: Rep[Int],__arg2: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload3) = densematrix_update[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos,overload3)
    def updateRow(__arg1: Rep[Int],__arg2: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload2) = densematrix_updaterow[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos,overload2)
    def updateCol(__arg1: Rep[Int],__arg2: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload2) = densematrix_updatecol[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos,overload2)
    def update(__arg1: Rep[IndexVector],__arg2: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload4) = densematrix_update[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos,overload4)
    def updateRows(__arg1: Rep[IndexVector],__arg2: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = densematrix_updaterows[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)
    def updateCols(__arg1: Rep[IndexVector],__arg2: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = densematrix_updatecols[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)
    def <<(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload3) = densematrix_ltlt[T](self,__arg1)(implicitly[Typ[T]],__pos,overload3)
    def <<(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload4) = densematrix_ltlt[T](self,__arg1)(implicitly[Typ[T]],__pos,overload4)
    def <<|(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_ltltor[T](self,__arg1)(implicitly[Typ[T]],__pos,overload1)
    def <<|(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload2) = densematrix_ltltor[T](self,__arg1)(implicitly[Typ[T]],__pos,overload2)
    def <<=(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_ltlteq[T](self,__arg1)(implicitly[Typ[T]],__pos,overload1)
    def <<=(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload2) = densematrix_ltlteq[T](self,__arg1)(implicitly[Typ[T]],__pos,overload2)
    def <<|=(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_ltltoreq[T](self,__arg1)(implicitly[Typ[T]],__pos,overload1)
    def <<|=(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload2) = densematrix_ltltoreq[T](self,__arg1)(implicitly[Typ[T]],__pos,overload2)
    def insertRow(pos: Rep[Int],y: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_insertrow[T](self,pos,y)(implicitly[Typ[T]],__pos)
    def insertAllRows(pos: Rep[Int],xs: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = densematrix_insertallrows[T](self,pos,xs)(implicitly[Typ[T]],__pos)
    def insertCol(pos: Rep[Int],y: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_insertcol[T](self,pos,y)(implicitly[Typ[T]],__pos)
    def insertAllCols(pos: Rep[Int],xs: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = densematrix_insertallcols[T](self,pos,xs)(implicitly[Typ[T]],__pos)
    def trim()(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_trim[T](self)(implicitly[Typ[T]],__pos)
    def removeRow(pos: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_removerow[T](self,pos)(implicitly[Typ[T]],__pos)
    def removeCol(pos: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_removecol[T](self,pos)(implicitly[Typ[T]],__pos)
    def removeRows(pos: Rep[Int],num: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_removerows[T](self,pos,num)(implicitly[Typ[T]],__pos)
    def removeCols(pos: Rep[Int],num: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_removecols[T](self,pos,num)(implicitly[Typ[T]],__pos)
    def +=(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densematrix_pleq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload1)
    def +=(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densematrix_pleq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload2)
    def -=(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densematrix_subeq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload1)
    def -=(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densematrix_subeq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload2)
    def *=(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densematrix_muleq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload1)
    def *=(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densematrix_muleq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload2)
    def /=(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densematrix_diveq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload1)
    def /=(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densematrix_diveq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload2)
    def sortRowsBy[B:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[B])(implicit __pos: SourceContext,__imp0: Ordering[B]) = densematrix_sortrowsby[T,B](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[B]],__pos,__imp0)
    def sortColsBy[B:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[B])(implicit __pos: SourceContext,__imp0: Ordering[B]) = densematrix_sortcolsby[T,B](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[B]],__pos,__imp0)
    def :>(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload1) = densematrix_clngt[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def :<(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload1) = densematrix_clnlt[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def groupRowsBy[K:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[K])(implicit __pos: SourceContext,__imp1: Overload2) = densematrix_grouprowsby[T,K](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[K]],__pos)
    def groupColsBy[K:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[K])(implicit __pos: SourceContext) = densematrix_groupcolsby[T,K](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[K]],__pos)
    def toBoolean(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Boolean],__imp2: Overload1) = densematrix_toboolean[T](self)(implicitly[Typ[T]],__pos,conv)
    def toDouble(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload2) = densematrix_todouble[T](self)(implicitly[Typ[T]],__pos,conv)
    def toFloat(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Float],__imp2: Overload2) = densematrix_tofloat[T](self)(implicitly[Typ[T]],__pos,conv)
    def toInt(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Int],__imp2: Overload2) = densematrix_toint[T](self)(implicitly[Typ[T]],__pos,conv)
    def apply(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload4) = { self.getRow(__arg1) }
    def apply(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload5) = { self.getRows(__arg1) }
    def apply(__arg1: Rep[IndexVector],__arg2: IndexWildcard)(implicit __pos: SourceContext,__imp1: ROverload6) = { self.getRows(__arg1) }
    def apply(rows: Rep[IndexVector],cols: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload7) = densematrix_apply[T](self,rows,cols)(implicitly[Typ[T]],__pos,overload7)
    def apply(__arg1: IndexWildcard,__arg2: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload8) = { self.getCols(__arg2) }
    def indices(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_indices[T](self)(implicitly[Typ[T]],__pos)
    def rowIndices(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_rowindices[T](self)(implicitly[Typ[T]],__pos)
    def colIndices(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_colindices[T](self)(implicitly[Typ[T]],__pos)
    def getRows(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_getrows[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def getCols(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_getcols[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def sliceRows(start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_slicerows[T](self,start,end)(implicitly[Typ[T]],__pos)
    def sliceCols(start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_slicecols[T](self,start,end)(implicitly[Typ[T]],__pos)
    def pprint()(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload1) = densematrix_pprint[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def makeDimsStr(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_makedimsstr[T](self)(implicitly[Typ[T]],__pos)
    def makeString(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload1) = densematrix_makestring[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def toString(implicit __imp0: Overload1) = densematrix_tostring[T](self)
    def t(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_t[T](self)(implicitly[Typ[T]],__pos)
    def mutable()(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_mutable[T](self)(implicitly[Typ[T]],__pos)
    def replicate(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_replicate[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)
    def +(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload131) = densematrix_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload131)
    def +(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload132) = densematrix_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload132)
    def -(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload131) = densematrix_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload131)
    def -(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload132) = densematrix_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload132)
    def *:*(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densematrix_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload1)
    def *(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload131) = densematrix_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload131)
    def *(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload132) = densematrix_matmult[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def *(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload133) = densematrix_sparse_matmult[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def *(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload134) = densematrix_matvecmult[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def *(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload135) = densematrix_sparse_matvecmult[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def /(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload131) = densematrix_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload131)
    def /(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload132) = densematrix_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload132)
    def +(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload133) = densematrix_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload133)
    def -(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload133) = densematrix_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload133)
    def *:*(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densematrix_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload2)
    def /(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload133) = densematrix_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload133)
    def sum(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densematrix_sum[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def prod(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densematrix_prod[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def mean(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload1) = densematrix_mean[T](self)(implicitly[Typ[T]],__pos,conv)
    def abs(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densematrix_abs[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def exp(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densematrix_exp[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def log(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densematrix_log[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def sumRows(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densematrix_sumrows[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def sumCols(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densematrix_sumcols[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def minRows(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload1) = densematrix_minrows[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def minCols(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload1) = densematrix_mincols[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def maxRows(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload1) = densematrix_maxrows[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def maxCols(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload1) = densematrix_maxcols[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def min(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload4) = densematrix_min[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def max(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload4) = densematrix_max[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def minIndex(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload1) = densematrix_minindex[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def maxIndex(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload1) = densematrix_maxindex[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def map[R:Typ](__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload2) = densematrix_map[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def reduce(__arg1: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densematrix_reduce[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def foreach(__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload2) = densematrix_foreach[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def zip[B:Typ,R:Typ](__arg1: Rep[DenseMatrix[B]])(__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_zip[T,B,R](self,__arg1,__arg2)(implicitly[Typ[T]],implicitly[Typ[B]],implicitly[Typ[R]],__pos)
    def count(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_count[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def mapRowsToVector[R:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_maprowstovector[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def mapColsToVector[R:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_mapcolstovector[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def findRows(__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_findrows[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def findCols(__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_findcols[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def filterRows(__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_filterrows[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def filterCols(__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_filtercols[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def foreachRow(__arg1: (Rep[DenseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_foreachrow[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def foreachCol(__arg1: (Rep[DenseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_foreachcol[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def mapRows[R:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[DenseVector[R]])(implicit __pos: SourceContext,__imp1: Overload2) = densematrix_maprows[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def mapCols[R:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[DenseVector[R]])(implicit __pos: SourceContext,__imp1: Overload1) = densematrix_mapcols[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def reduceRows(__arg1: (Rep[DenseVector[T]],Rep[DenseVector[T]]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densematrix_reducerows[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def reduceCols(__arg1: (Rep[DenseVector[T]],Rep[DenseVector[T]]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densematrix_reducecols[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
  }

  implicit def repToDenseMatrixIntOpsCls(x: Rep[Int])(implicit __pos: SourceContext) = new DenseMatrixIntOpsCls(x)(__pos)
  implicit def varToDenseMatrixIntOpsCls(x: Var[Int])(implicit __pos: SourceContext) = new DenseMatrixIntOpsCls(readVar(x))(__pos)

  class DenseMatrixIntOpsCls(val self: Rep[Int])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload134) = { densematrix_pl[Int](__arg1,self) }
    def +(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload135) = { densematrix_pl[Float](__arg1,self.toFloat) }
    def +(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload136) = { densematrix_pl[Double](__arg1,self.toDouble) }
    def -(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload134) = { densematrix_map[Int,Int](__arg1, e => forge_int_minus(self,e)) }
    def -(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload135) = { densematrix_map[Float,Float](__arg1, e => forge_float_minus(self.toFloat,e)) }
    def -(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload136) = { densematrix_map[Double,Double](__arg1, e => forge_double_minus(self.toDouble,e)) }
    def *(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload136) = { densematrix_mul[Int](__arg1,self) }
    def *(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload137) = { densematrix_mul[Float](__arg1,self.toFloat) }
    def *(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload138) = { densematrix_mul[Double](__arg1,self.toDouble) }
  }

  implicit def repToDenseMatrixFloatOpsCls(x: Rep[Float])(implicit __pos: SourceContext) = new DenseMatrixFloatOpsCls(x)(__pos)
  implicit def varToDenseMatrixFloatOpsCls(x: Var[Float])(implicit __pos: SourceContext) = new DenseMatrixFloatOpsCls(readVar(x))(__pos)

  class DenseMatrixFloatOpsCls(val self: Rep[Float])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload137) = { densematrix_pl[Float](__arg1.toFloat,self) }
    def +(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload138) = { densematrix_pl[Float](__arg1,self) }
    def +(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload139) = { densematrix_pl[Double](__arg1,self.toDouble) }
    def -(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload137) = { densematrix_map[Int,Float](__arg1, e => forge_float_minus(self,e.toFloat)) }
    def -(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload138) = { densematrix_map[Float,Float](__arg1, e => forge_float_minus(self,e)) }
    def -(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload139) = { densematrix_map[Double,Double](__arg1, e => forge_double_minus(self.toDouble,e)) }
    def *(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload139) = { densematrix_mul[Float](__arg1.toFloat,self) }
    def *(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload140) = { densematrix_mul[Float](__arg1,self) }
    def *(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload141) = { densematrix_mul[Double](__arg1,self.toDouble) }
  }

  implicit def repToDenseMatrixDoubleOpsCls(x: Rep[Double])(implicit __pos: SourceContext) = new DenseMatrixDoubleOpsCls(x)(__pos)
  implicit def varToDenseMatrixDoubleOpsCls(x: Var[Double])(implicit __pos: SourceContext) = new DenseMatrixDoubleOpsCls(readVar(x))(__pos)

  class DenseMatrixDoubleOpsCls(val self: Rep[Double])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload140) = { densematrix_pl[Double](__arg1.toDouble,self) }
    def +(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload141) = { densematrix_pl[Double](__arg1.toDouble,self) }
    def +(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload142) = { densematrix_pl[Double](__arg1,self) }
    def -(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload140) = { densematrix_map[Int,Double](__arg1, e => forge_double_minus(self,e.toDouble)) }
    def -(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload141) = { densematrix_map[Float,Double](__arg1, e => forge_double_minus(self,e.toDouble)) }
    def -(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload142) = { densematrix_map[Double,Double](__arg1, e => forge_double_minus(self,e)) }
    def *(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload142) = { densematrix_mul[Double](__arg1.toDouble,self) }
    def *(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload143) = { densematrix_mul[Double](__arg1.toDouble,self) }
    def *(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload144) = { densematrix_mul[Double](__arg1,self) }
  }

  implicit def repToDenseMatrixDenseMatrixIntOpsCls(x: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext) = new DenseMatrixDenseMatrixIntOpsCls(x)(__pos)
  implicit def varToDenseMatrixDenseMatrixIntOpsCls(x: Var[DenseMatrix[Int]])(implicit __pos: SourceContext) = new DenseMatrixDenseMatrixIntOpsCls(readVar(x))(__pos)

  class DenseMatrixDenseMatrixIntOpsCls(val self: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload143) = { densematrix_pl[Int](self,__arg1) }
    def +(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload144) = { densematrix_pl[Float](self.toFloat,__arg1) }
    def +(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload145) = { densematrix_pl[Double](self.toDouble,__arg1) }
    def +(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload152) = { densematrix_pl[Int](self,__arg1) }
    def +(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload153) = { densematrix_pl[Float](self.toFloat,__arg1) }
    def +(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload154) = { densematrix_pl[Double](self.toDouble,__arg1) }
    def -(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload143) = { densematrix_sub[Int](self,__arg1) }
    def -(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload144) = { densematrix_sub[Float](self.toFloat,__arg1) }
    def -(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload145) = { densematrix_sub[Double](self.toDouble,__arg1) }
    def -(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload152) = { densematrix_sub[Int](self,__arg1) }
    def -(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload153) = { densematrix_sub[Float](self.toFloat,__arg1) }
    def -(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload154) = { densematrix_sub[Double](self.toDouble,__arg1) }
    def unary_-(implicit __pos: SourceContext,__imp1: ROverload5) = { densematrix_mul[Int](self,unit(-1)) }
    def *(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload145) = { densematrix_mul[Int](self,__arg1) }
    def *(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload146) = { densematrix_mul[Float](self.toFloat,__arg1) }
    def *(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload147) = { densematrix_mul[Double](self.toDouble,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload154) = { densematrix_matmult[Int](self,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload155) = { densematrix_matmult[Float](self.toFloat,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload156) = { densematrix_matmult[Double](self.toDouble,__arg1) }
    def *(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload163) = { densematrix_matvecmult[Int](self,__arg1) }
    def *(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload164) = { densematrix_matvecmult[Float](self.toFloat,__arg1) }
    def *(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload165) = { densematrix_matvecmult[Double](self.toDouble,__arg1) }
    def *:*(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload3) = { densematrix_mulclnmul[Int](self,__arg1) }
    def *:*(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload4) = { densematrix_mulclnmul[Float](self.toFloat,__arg1) }
    def *:*(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload5) = { densematrix_mulclnmul[Double](self.toDouble,__arg1) }
    def /(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload134) = { densematrix_div[Int](self,__arg1) }
    def /(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload135) = { densematrix_div[Float](self.toFloat,__arg1) }
    def /(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload136) = { densematrix_div[Double](self.toDouble,__arg1) }
    def /(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload143) = { densematrix_div[Int](self,__arg1) }
    def /(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload144) = { densematrix_div[Float](self.toFloat,__arg1) }
    def /(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload145) = { densematrix_div[Double](self.toDouble,__arg1) }
  }

  implicit def repToDenseMatrixDenseMatrixFloatOpsCls(x: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext) = new DenseMatrixDenseMatrixFloatOpsCls(x)(__pos)
  implicit def varToDenseMatrixDenseMatrixFloatOpsCls(x: Var[DenseMatrix[Float]])(implicit __pos: SourceContext) = new DenseMatrixDenseMatrixFloatOpsCls(readVar(x))(__pos)

  class DenseMatrixDenseMatrixFloatOpsCls(val self: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload146) = { densematrix_pl[Float](self,__arg1.toFloat) }
    def +(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload147) = { densematrix_pl[Float](self,__arg1) }
    def +(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload148) = { densematrix_pl[Double](self.toDouble,__arg1) }
    def +(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload155) = { densematrix_pl[Float](self,__arg1.toFloat) }
    def +(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload156) = { densematrix_pl[Float](self,__arg1) }
    def +(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload157) = { densematrix_pl[Double](self.toDouble,__arg1) }
    def -(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload146) = { densematrix_sub[Float](self,__arg1.toFloat) }
    def -(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload147) = { densematrix_sub[Float](self,__arg1) }
    def -(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload148) = { densematrix_sub[Double](self.toDouble,__arg1) }
    def -(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload155) = { densematrix_sub[Float](self,__arg1.toFloat) }
    def -(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload156) = { densematrix_sub[Float](self,__arg1) }
    def -(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload157) = { densematrix_sub[Double](self.toDouble,__arg1) }
    def unary_-(implicit __pos: SourceContext,__imp1: ROverload6) = { densematrix_mul[Float](self,unit(-1f)) }
    def *(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload148) = { densematrix_mul[Float](self,__arg1.toFloat) }
    def *(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload149) = { densematrix_mul[Float](self,__arg1) }
    def *(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload150) = { densematrix_mul[Double](self.toDouble,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload157) = { densematrix_matmult[Float](self,__arg1.toFloat) }
    def *(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload158) = { densematrix_matmult[Float](self,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload159) = { densematrix_matmult[Double](self.toDouble,__arg1) }
//    def *(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload166) = { densematrix_matvecmult[Float](self,__arg1.toFloat) }
    def *(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload167) = { densematrix_matvecmult[Float](self,__arg1) }
    def *(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload168) = { densematrix_matvecmult[Double](self.toDouble,__arg1) }
    def *:*(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload6) = { densematrix_mulclnmul[Float](self,__arg1.toFloat) }
    def *:*(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload7) = { densematrix_mulclnmul[Float](self,__arg1) }
    def *:*(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload8) = { densematrix_mulclnmul[Double](self.toDouble,__arg1) }
    def /(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload137) = { densematrix_div[Float](self,__arg1.toFloat) }
    def /(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload138) = { densematrix_div[Float](self,__arg1) }
    def /(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload139) = { densematrix_div[Double](self.toDouble,__arg1) }
    def /(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload146) = { densematrix_div[Float](self,__arg1.toFloat) }
    def /(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload147) = { densematrix_div[Float](self,__arg1) }
    def /(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload148) = { densematrix_div[Double](self.toDouble,__arg1) }
  }

  implicit def repToDenseMatrixDenseMatrixDoubleOpsCls(x: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext) = new DenseMatrixDenseMatrixDoubleOpsCls(x)(__pos)
  implicit def varToDenseMatrixDenseMatrixDoubleOpsCls(x: Var[DenseMatrix[Double]])(implicit __pos: SourceContext) = new DenseMatrixDenseMatrixDoubleOpsCls(readVar(x))(__pos)

  class DenseMatrixDenseMatrixDoubleOpsCls(val self: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload149) = { densematrix_pl[Double](self,__arg1.toDouble) }
    def +(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload150) = { densematrix_pl[Double](self.toDouble,__arg1) }
    def +(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload151) = { densematrix_pl[Double](self,__arg1) }
    def +(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload158) = { densematrix_pl[Double](self,__arg1.toDouble) }
    def +(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload159) = { densematrix_pl[Double](self,__arg1.toDouble) }
    def +(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload160) = { densematrix_pl[Double](self,__arg1) }
    def -(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload149) = { densematrix_sub[Double](self,__arg1.toDouble) }
    def -(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload150) = { densematrix_sub[Double](self.toDouble,__arg1) }
    def -(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload151) = { densematrix_sub[Double](self,__arg1) }
    def -(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload158) = { densematrix_sub[Double](self,__arg1.toDouble) }
    def -(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload159) = { densematrix_sub[Double](self,__arg1.toDouble) }
    def -(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload160) = { densematrix_sub[Double](self,__arg1) }
    def unary_-(implicit __pos: SourceContext,__imp1: ROverload7) = { densematrix_mul[Double](self,unit(-1.0)) }
    def *(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload151) = { densematrix_mul[Double](self,__arg1.toDouble) }
    def *(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload152) = { densematrix_mul[Double](self.toDouble,__arg1) }
    def *(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload153) = { densematrix_mul[Double](self,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload160) = { densematrix_matmult[Double](self,__arg1.toDouble) }
    def *(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload161) = { densematrix_matmult[Double](self,__arg1.toDouble) }
    def *(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload162) = { densematrix_matmult[Double](self,__arg1) }
//    def *(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload169) = { densematrix_matvecmult[Double](self,__arg1.toDouble) }
//    def *(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload170) = { densematrix_matvecmult[Double](self,__arg1.toDouble) }
    def *(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload171) = { densematrix_matvecmult[Double](self,__arg1) }
    def *:*(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload9) = { densematrix_mulclnmul[Double](self,__arg1.toDouble) }
    def *:*(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload10) = { densematrix_mulclnmul[Double](self,__arg1.toDouble) }
    def *:*(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload11) = { densematrix_mulclnmul[Double](self,__arg1) }
    def /(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload140) = { densematrix_div[Double](self,__arg1.toDouble) }
    def /(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload141) = { densematrix_div[Double](self.toDouble,__arg1) }
    def /(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload142) = { densematrix_div[Double](self,__arg1) }
    def /(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload149) = { densematrix_div[Double](self,__arg1.toDouble) }
    def /(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload150) = { densematrix_div[Double](self,__arg1.toDouble) }
    def /(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload151) = { densematrix_div[Double](self,__arg1) }
  }



  def densematrix_dist(__arg0: Rep[DenseMatrix[Double]],__arg1: Rep[DenseMatrix[Double]],__arg2: DistanceMetric)(implicit __pos: SourceContext): Rep[Double]
  def densematrix_norm(__arg0: Rep[DenseMatrix[Double]],__arg1: NormId)(implicit __pos: SourceContext,__imp1: Overload2): Rep[Double]
  def densematrix_object_apply[T:Typ](__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload6): Rep[DenseMatrix[T]]
  def densematrix_object_apply[T:Typ](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload7): Rep[DenseMatrix[T]]
  def densematrix_object_apply[T:Typ](__arg0: Rep[DenseVector[DenseVector[T]]])(implicit __pos: SourceContext,__imp1: Overload8): Rep[DenseMatrix[T]]
  def densematrix_object_apply[T:Typ](__arg0: Rep[DenseVector[DenseVectorView[T]]])(implicit __pos: SourceContext,__imp1: Overload9): Rep[DenseMatrix[T]]
  def densematrix_object_apply[T:Typ](__arg0: Seq[Rep[DenseVector[T]]])(implicit __pos: SourceContext,__imp1: Overload10): Rep[DenseMatrix[T]]
  def densematrix_object_block[T:Typ](__arg0: Seq[Rep[DenseVector[DenseMatrix[T]]]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_object_diag[T:Arith:Typ](__arg0: Rep[Int],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_object_identity(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1): Rep[DenseMatrix[Double]]
  def densematrix_densematrix_fromarray[T:Typ](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_densematrix_fromfunc[T:Typ](__arg0: Rep[Int],__arg1: Rep[Int],__arg2: (Rep[Int],Rep[Int]) => Rep[T])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_object_zeros(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_object_zerosf(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[Float]]
  def densematrix_object_ones(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_object_onesf(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[Float]]
  def densematrix_object_rand(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_object_randf(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[Float]]
  def densematrix_object_randn(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_object_randnf(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[Float]]
  def densematrix_flattentovector[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densematrix_numrows[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrix_numcols[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrix_size[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrix_apply[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3): Rep[T]
  def densematrix_vectorview[T:Typ](self: Rep[DenseMatrix[T]],start: Rep[Int],stride: Rep[Int],length: Rep[Int],isRow: Rep[Boolean])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def densematrix_mview[T:Typ](self: Rep[DenseMatrix[T]],startRow: Rep[Int],endRow: Rep[Int],startCol: Rep[Int],endCol: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrixView[T]]
  def densematrix_getrow[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def densematrix_getcol[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def densematrix_slice[T:Typ](self: Rep[DenseMatrix[T]],startRow: Rep[Int],endRow: Rep[Int],startCol: Rep[Int],endCol: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrixView[T]]
  def densematrix_diag[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densematrix_triu[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]]
  def densematrix_tril[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]]
  def densematrix_clone[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_tosparse[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def densematrix_toarray[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[ForgeArray[T]]
  def densematrix_update[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[T])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Unit]
  def densematrix_update[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload2): Rep[Unit]
  def densematrix_updaterow[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Unit]
  def densematrix_updatecol[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Unit]
  def densematrix_update[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload3): Rep[Unit]
  def densematrix_updaterow[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload2): Rep[Unit]
  def densematrix_updatecol[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload2): Rep[Unit]
  def densematrix_update[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[IndexVector],__arg2: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload4): Rep[Unit]
  def densematrix_updaterows[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[IndexVector],__arg2: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_updatecols[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[IndexVector],__arg2: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_ltlt[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload3): Rep[DenseMatrix[T]]
  def densematrix_ltlt[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload4): Rep[DenseMatrix[T]]
  def densematrix_ltltor[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1): Rep[DenseMatrix[T]]
  def densematrix_ltltor[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload2): Rep[DenseMatrix[T]]
  def densematrix_ltlteq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Unit]
  def densematrix_ltlteq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload2): Rep[Unit]
  def densematrix_ltltoreq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Unit]
  def densematrix_ltltoreq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload2): Rep[Unit]
  def densematrix_insertrow[T:Typ](self: Rep[DenseMatrix[T]],pos: Rep[Int],y: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_insertallrows[T:Typ](self: Rep[DenseMatrix[T]],pos: Rep[Int],xs: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_insertcol[T:Typ](self: Rep[DenseMatrix[T]],pos: Rep[Int],y: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_insertallcols[T:Typ](self: Rep[DenseMatrix[T]],pos: Rep[Int],xs: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_trim[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_removerow[T:Typ](self: Rep[DenseMatrix[T]],pos: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_removecol[T:Typ](self: Rep[DenseMatrix[T]],pos: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_removerows[T:Typ](self: Rep[DenseMatrix[T]],pos: Rep[Int],num: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_removecols[T:Typ](self: Rep[DenseMatrix[T]],pos: Rep[Int],num: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_pleq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1): Rep[Unit]
  def densematrix_pleq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2): Rep[Unit]
  def densematrix_subeq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1): Rep[Unit]
  def densematrix_subeq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2): Rep[Unit]
  def densematrix_muleq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1): Rep[Unit]
  def densematrix_muleq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2): Rep[Unit]
  def densematrix_diveq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1): Rep[Unit]
  def densematrix_diveq[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2): Rep[Unit]
  def densematrix_sortrowsby[T:Typ,B:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[B])(implicit __pos: SourceContext,__imp0: Ordering[B]): Rep[DenseMatrix[T]]
  def densematrix_sortcolsby[T:Typ,B:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[B])(implicit __pos: SourceContext,__imp0: Ordering[B]): Rep[DenseMatrix[T]]
  def densematrix_clngt[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[DenseMatrix[Boolean]]
  def densematrix_clnlt[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[DenseMatrix[Boolean]]
  def densematrix___equal[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Boolean]
  def densematrix___equal[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp1: Overload2): Rep[Boolean]
  def densematrix___equal[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload3): Rep[Boolean]
  def densematrix_grouprowsby[T:Typ,K:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[K])(implicit __pos: SourceContext): Rep[ForgeHashMap[K,DenseMatrix[T]]]
  def densematrix_groupcolsby[T:Typ,K:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[K])(implicit __pos: SourceContext): Rep[ForgeHashMap[K,DenseMatrix[T]]]
  def densematrix_densematrix_raw_apply[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[T]
  def densematrix_densematrix_raw_update[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_toboolean[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Boolean]): Rep[DenseMatrix[Boolean]]
  def densematrix_todouble[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double]): Rep[DenseMatrix[Double]]
  def densematrix_tofloat[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Float]): Rep[DenseMatrix[Float]]
  def densematrix_toint[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Int]): Rep[DenseMatrix[Int]]
  def densematrix_apply[T:Typ](self: Rep[DenseMatrix[T]],rows: Rep[IndexVector],cols: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload7): Rep[DenseMatrix[T]]
  def densematrix_indices[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def densematrix_rowindices[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def densematrix_colindices[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def densematrix_getrows[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_getcols[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_slicerows[T:Typ](self: Rep[DenseMatrix[T]],start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrixView[T]]
  def densematrix_slicecols[T:Typ](self: Rep[DenseMatrix[T]],start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrixView[T]]
  def densematrix_pprint[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[Unit]
  def densematrix_makedimsstr[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[String]
  def densematrix_makestring[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[String]
  def densematrix_tostring[T:Typ](self: Rep[DenseMatrix[T]]): Rep[String]
  def densematrix_t[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_mutable[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_replicate[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_pl[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload131): Rep[DenseMatrix[T]]
  def densematrix_pl[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload132): Rep[DenseMatrix[T]]
  def densematrix_sub[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload131): Rep[DenseMatrix[T]]
  def densematrix_sub[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload132): Rep[DenseMatrix[T]]
  def densematrix_mulclnmul[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1): Rep[DenseMatrix[T]]
  def densematrix_mul[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload131): Rep[DenseMatrix[T]]
  def densematrix_matmult[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]]
  def densematrix_sparse_matmult[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]]
  def densematrix_matvecmult[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densematrix_sparse_matvecmult[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densematrix_div[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload131): Rep[DenseMatrix[T]]
  def densematrix_div[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload132): Rep[DenseMatrix[T]]
  def densematrix_pl[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload133): Rep[DenseMatrix[T]]
  def densematrix_sub[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload133): Rep[DenseMatrix[T]]
  def densematrix_mulclnmul[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2): Rep[DenseMatrix[T]]
  def densematrix_div[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload133): Rep[DenseMatrix[T]]
  def densematrix_sum[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densematrix_prod[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densematrix_mean[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double]): Rep[Double]
  def densematrix_abs[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]]
  def densematrix_exp[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]]
  def densematrix_log[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]]
  def densematrix_sumrows[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densematrix_sumcols[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densematrix_minrows[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[DenseVector[T]]
  def densematrix_mincols[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[DenseVector[T]]
  def densematrix_maxrows[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[DenseVector[T]]
  def densematrix_maxcols[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[DenseVector[T]]
  def densematrix_min[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def densematrix_max[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def densematrix_minindex[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[Tup2[Int,Int]]
  def densematrix_maxindex[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[Tup2[Int,Int]]
  def densematrix_map[T:Typ,R:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseMatrix[R]]
  def densematrix_reduce[T:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densematrix_foreach[T:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_zip[T:Typ,B:Typ,R:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[B]],__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseMatrix[R]]
  def densematrix_count[T:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Int]
  def densematrix_maprowstovector[T:Typ,R:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densematrix_mapcolstovector[T:Typ,R:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densematrix_findrows[T:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def densematrix_findcols[T:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def densematrix_filterrows[T:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_filtercols[T:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrix_foreachrow[T:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_foreachcol[T:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_maprows[T:Typ,R:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[DenseVector[R]])(implicit __pos: SourceContext): Rep[DenseMatrix[R]]
  def densematrix_mapcols[T:Typ,R:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[DenseVector[R]])(implicit __pos: SourceContext): Rep[DenseMatrix[R]]
  def densematrix_reducerows[T:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVector[T]],Rep[DenseVector[T]]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densematrix_reducecols[T:Typ](self: Rep[DenseMatrix[T]],__arg1: (Rep[DenseVector[T]],Rep[DenseVector[T]]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
}
trait DenseMatrixCompilerOps extends DenseMatrixOps {
  this: OptiML => 

  def matrix_shapeindex(idx: Rep[Long],numCols: Rep[Long])(implicit __pos: SourceContext): Rep[Tup2[Int,Int]]
  def densematrix_grouprowsby_helper[T:Typ,K:Typ,V:Typ](__arg0: Rep[IndexVector],__arg1: Rep[DenseMatrix[T]],__arg2: (Rep[DenseVectorView[T]]) => Rep[K],__arg3: (Rep[DenseVectorView[T]]) => Rep[V])(implicit __pos: SourceContext): Rep[ForgeHashMap[K,ForgeArrayBuffer[V]]]
  def densematrix_groupcolsby_helper[T:Typ,K:Typ,V:Typ](__arg0: Rep[IndexVector],__arg1: Rep[DenseMatrix[T]],__arg2: (Rep[DenseVectorView[T]]) => Rep[K],__arg3: (Rep[DenseVectorView[T]]) => Rep[V])(implicit __pos: SourceContext): Rep[ForgeHashMap[K,ForgeArrayBuffer[V]]]
  def densematrix_dc_alloc[R:Typ,CR:Typ](__arg0: Rep[CR],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[R]]
  def densematrix_index[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def densematrix_raw_data[T:Typ](self: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[ForgeArray[T]]
  def densematrix_set_raw_data[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[ForgeArray[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_set_numrows[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_set_numcols[T:Typ](self: Rep[DenseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_insertspace[T:Typ](self: Rep[DenseMatrix[T]],pos: Rep[Int],len: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_ensureextra[T:Typ](self: Rep[DenseMatrix[T]],extra: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densematrix_realloc[T:Typ](self: Rep[DenseMatrix[T]],minLen: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
}

