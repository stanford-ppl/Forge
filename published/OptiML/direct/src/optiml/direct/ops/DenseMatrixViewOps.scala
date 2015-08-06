package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DenseMatrixViewOpsBase extends Base {
  this: OptiML => 

  implicit def viewToDense[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp1: Overload2): Rep[DenseMatrix[T]] = densematrixview_viewtodense[T](self)(implicitly[Typ[T]],__pos)
  implicit def chainViewToDenseOps[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp1: Overload2): DenseMatrixDenseMatrixOpsCls[T] = densematrixview_chainviewtodenseops[T](self)(implicitly[Typ[T]],__pos)

  def densematrixview_viewtodense[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrixview_chainviewtodenseops[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): DenseMatrixDenseMatrixOpsCls[T]
}

trait DenseMatrixViewOps extends DenseMatrixViewOpsBase {
  this: OptiML => 

  object DenseMatrixView {
    def apply[T:Typ](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[Int],__arg4: Rep[Int],__arg5: Rep[Int],__arg6: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload17) = densematrixview_object_apply[T](__arg0,__arg1,__arg2,__arg3,__arg4,__arg5,__arg6)(implicitly[Typ[T]],__pos)
  }

  def norm(__arg0: Rep[DenseMatrixView[Double]])(implicit __pos: SourceContext,__imp1: ROverload5): Rep[Double] = { norm(__arg0,L2) }
  def norm(__arg0: Rep[DenseMatrixView[Double]],__arg1: NormId)(implicit __pos: SourceContext,__imp1: Overload6) = densematrixview_norm(__arg0,__arg1)(__pos,overload6)
  def __equal[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp1: Overload5) = densematrixview___equal[T](self,__arg1)(implicitly[Typ[T]],__pos)
  def densematrixview_raw_apply[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[Int])(implicit __pos: SourceContext) = densematrixview_densematrixview_raw_apply[T](self,__arg1)(implicitly[Typ[T]],__pos)

  implicit def repToDenseMatrixViewDenseMatrixViewOpsCls[T:Typ](x: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext) = new DenseMatrixViewDenseMatrixViewOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToDenseMatrixViewDenseMatrixViewOpsCls[T:Typ](x: Var[DenseMatrixView[T]])(implicit __pos: SourceContext) = new DenseMatrixViewDenseMatrixViewOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class DenseMatrixViewDenseMatrixViewOpsCls[T:Typ](val self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext) {
    def numRows(implicit __pos: SourceContext,__imp1: Overload3) = densematrixview_numrows[T](self)(implicitly[Typ[T]],__pos)
    def numCols(implicit __pos: SourceContext,__imp1: Overload3) = densematrixview_numcols[T](self)(implicitly[Typ[T]],__pos)
    def size(implicit __pos: SourceContext,__imp1: Overload9) = densematrixview_size[T](self)(implicitly[Typ[T]],__pos)
    def apply(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload13) = densematrixview_apply[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos,overload13)
    def vview(start: Rep[Int],stride: Rep[Int],length: Rep[Int],isRow: Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_vview[T](self,start,stride,length,isRow)(implicitly[Typ[T]],__pos)
    def mview(startRow: Rep[Int],endRow: Rep[Int],startCol: Rep[Int],endCol: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_mview[T](self,startRow,endRow,startCol,endCol)(implicitly[Typ[T]],__pos)
    def getRow(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_getrow[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def getCol(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_getcol[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def slice(startRow: Rep[Int],endRow: Rep[Int],startCol: Rep[Int],endCol: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4) = densematrixview_slice[T](self,startRow,endRow,startCol,endCol)(implicitly[Typ[T]],__pos)
    def Clone(implicit __pos: SourceContext,__imp1: ROverload3) = { self.toDense }
    def toDense(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_todense[T](self)(implicitly[Typ[T]],__pos)
    def toBoolean(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Boolean],__imp2: Overload4) = densematrixview_toboolean[T](self)(implicitly[Typ[T]],__pos,conv)
    def toDouble(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload5) = densematrixview_todouble[T](self)(implicitly[Typ[T]],__pos,conv)
    def toFloat(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Float],__imp2: Overload5) = densematrixview_tofloat[T](self)(implicitly[Typ[T]],__pos,conv)
    def toInt(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Int],__imp2: Overload5) = densematrixview_toint[T](self)(implicitly[Typ[T]],__pos,conv)
    def apply(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload14) = { self.getRow(__arg1) }
    def apply(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload15) = { self.getRows(__arg1) }
    def apply(__arg1: Rep[IndexVector],__arg2: IndexWildcard)(implicit __pos: SourceContext,__imp1: ROverload16) = { self.getRows(__arg1) }
    def apply(rows: Rep[IndexVector],cols: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload17) = densematrixview_apply[T](self,rows,cols)(implicitly[Typ[T]],__pos,overload17)
    def apply(__arg1: IndexWildcard,__arg2: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload18) = { self.getCols(__arg2) }
    def indices(implicit __pos: SourceContext,__imp1: Overload3) = densematrixview_indices[T](self)(implicitly[Typ[T]],__pos)
    def rowIndices(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_rowindices[T](self)(implicitly[Typ[T]],__pos)
    def colIndices(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_colindices[T](self)(implicitly[Typ[T]],__pos)
    def getRows(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload3) = densematrixview_getrows[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def getCols(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload3) = densematrixview_getcols[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def sliceRows(start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_slicerows[T](self,start,end)(implicitly[Typ[T]],__pos)
    def sliceCols(start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_slicecols[T](self,start,end)(implicitly[Typ[T]],__pos)
    def pprint()(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload3) = densematrixview_pprint[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def makeDimsStr(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_makedimsstr[T](self)(implicitly[Typ[T]],__pos)
    def makeString(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload3) = densematrixview_makestring[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def toString(implicit __imp0: Overload4) = densematrixview_tostring[T](self)
    def t(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_t[T](self)(implicitly[Typ[T]],__pos)
    def mutable()(implicit __pos: SourceContext,__imp1: Overload8) = densematrixview_mutable[T](self)(implicitly[Typ[T]],__pos)
    def replicate(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3) = densematrixview_replicate[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)
    def +(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload188) = densematrixview_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload188)
    def +(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload189) = densematrixview_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload189)
    def -(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload172) = densematrixview_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload172)
    def -(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload173) = densematrixview_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload173)
    def *:*(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload16) = densematrixview_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload16)
    def *(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload184) = densematrixview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload184)
    def *(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload185) = densematrixview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload185)
    def *(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload186) = densematrixview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload186)
    def *(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload187) = densematrixview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload187)
    def *(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload188) = densematrixview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload188)
    def /(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload163) = densematrixview_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload163)
    def /(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload164) = densematrixview_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload164)
    def +(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload190) = densematrixview_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload190)
    def -(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload174) = densematrixview_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload174)
    def *:*(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload17) = densematrixview_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload17)
    def /(__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload165) = densematrixview_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload165)
    def sum(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densematrixview_sum[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def prod(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densematrixview_prod[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def mean(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload3) = densematrixview_mean[T](self)(implicitly[Typ[T]],__pos,conv)
    def abs(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload11) = densematrixview_abs[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def exp(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload11) = densematrixview_exp[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def log(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload11) = densematrixview_log[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def sumRows(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densematrixview_sumrows[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def sumCols(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densematrixview_sumcols[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def minRows(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload2) = densematrixview_minrows[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def minCols(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload2) = densematrixview_mincols[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def maxRows(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload2) = densematrixview_maxrows[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def maxCols(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload2) = densematrixview_maxcols[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def min(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload6) = densematrixview_min[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def max(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload6) = densematrixview_max[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def minIndex(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload3) = densematrixview_minindex[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def maxIndex(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload3) = densematrixview_maxindex[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def map[R:Typ](__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload4) = densematrixview_map[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def reduce(__arg1: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4) = densematrixview_reduce[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def foreach(__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload4) = densematrixview_foreach[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def zip[B:Typ,R:Typ](__arg1: Rep[DenseMatrix[B]])(__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload4) = densematrixview_zip[T,B,R](self,__arg1,__arg2)(implicitly[Typ[T]],implicitly[Typ[B]],implicitly[Typ[R]],__pos)
    def count(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload3) = densematrixview_count[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def mapRowsToVector[R:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_maprowstovector[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def mapColsToVector[R:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_mapcolstovector[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def findRows(__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_findrows[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def findCols(__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_findcols[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def filterRows(__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_filterrows[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def filterCols(__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_filtercols[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def foreachRow(__arg1: (Rep[DenseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_foreachrow[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def foreachCol(__arg1: (Rep[DenseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_foreachcol[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def mapRows[R:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[DenseVector[R]])(implicit __pos: SourceContext,__imp1: Overload3) = densematrixview_maprows[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def mapCols[R:Typ](__arg1: (Rep[DenseVectorView[T]]) => Rep[DenseVector[R]])(implicit __pos: SourceContext,__imp1: Overload2) = densematrixview_mapcols[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def reduceRows(__arg1: (Rep[DenseVector[T]],Rep[DenseVector[T]]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densematrixview_reducerows[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def reduceCols(__arg1: (Rep[DenseVector[T]],Rep[DenseVector[T]]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densematrixview_reducecols[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
  }



  def densematrixview_norm(__arg0: Rep[DenseMatrixView[Double]],__arg1: NormId)(implicit __pos: SourceContext,__imp1: Overload6): Rep[Double]
  def densematrixview_object_apply[T:Typ](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[Int],__arg4: Rep[Int],__arg5: Rep[Int],__arg6: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrixView[T]]
  def densematrixview_numrows[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrixview_numcols[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrixview_size[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrixview_apply[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload13): Rep[T]
  def densematrixview_vview[T:Typ](self: Rep[DenseMatrixView[T]],start: Rep[Int],stride: Rep[Int],length: Rep[Int],isRow: Rep[Boolean])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def densematrixview_mview[T:Typ](self: Rep[DenseMatrixView[T]],startRow: Rep[Int],endRow: Rep[Int],startCol: Rep[Int],endCol: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrixView[T]]
  def densematrixview_getrow[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def densematrixview_getcol[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def densematrixview_slice[T:Typ](self: Rep[DenseMatrixView[T]],startRow: Rep[Int],endRow: Rep[Int],startCol: Rep[Int],endCol: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrixView[T]]
  def densematrixview_todense[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrixview___equal[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[Boolean]
  def densematrixview_densematrixview_raw_apply[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[T]
  def densematrixview_toboolean[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Boolean]): Rep[DenseMatrix[Boolean]]
  def densematrixview_todouble[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double]): Rep[DenseMatrix[Double]]
  def densematrixview_tofloat[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Float]): Rep[DenseMatrix[Float]]
  def densematrixview_toint[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Int]): Rep[DenseMatrix[Int]]
  def densematrixview_apply[T:Typ](self: Rep[DenseMatrixView[T]],rows: Rep[IndexVector],cols: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload17): Rep[DenseMatrix[T]]
  def densematrixview_indices[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def densematrixview_rowindices[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def densematrixview_colindices[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def densematrixview_getrows[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrixview_getcols[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrixview_slicerows[T:Typ](self: Rep[DenseMatrixView[T]],start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrixView[T]]
  def densematrixview_slicecols[T:Typ](self: Rep[DenseMatrixView[T]],start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrixView[T]]
  def densematrixview_pprint[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[Unit]
  def densematrixview_makedimsstr[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[String]
  def densematrixview_makestring[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[String]
  def densematrixview_tostring[T:Typ](self: Rep[DenseMatrixView[T]]): Rep[String]
  def densematrixview_t[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrixview_mutable[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrixview_replicate[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrixview_pl[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload188): Rep[DenseMatrix[T]]
  def densematrixview_pl[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload189): Rep[DenseMatrix[T]]
  def densematrixview_sub[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload172): Rep[DenseMatrix[T]]
  def densematrixview_sub[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload173): Rep[DenseMatrix[T]]
  def densematrixview_mulclnmul[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload16): Rep[DenseMatrix[T]]
  def densematrixview_mul[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload184): Rep[DenseMatrix[T]]
  def densematrixview_mul[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload185): Rep[DenseMatrix[T]]
  def densematrixview_mul[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload186): Rep[DenseMatrix[T]]
  def densematrixview_mul[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload187): Rep[DenseVector[T]]
  def densematrixview_mul[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload188): Rep[DenseVector[T]]
  def densematrixview_div[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload163): Rep[DenseMatrix[T]]
  def densematrixview_div[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload164): Rep[DenseMatrix[T]]
  def densematrixview_pl[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload190): Rep[DenseMatrix[T]]
  def densematrixview_sub[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload174): Rep[DenseMatrix[T]]
  def densematrixview_mulclnmul[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload17): Rep[DenseMatrix[T]]
  def densematrixview_div[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload165): Rep[DenseMatrix[T]]
  def densematrixview_sum[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densematrixview_prod[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densematrixview_mean[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double]): Rep[Double]
  def densematrixview_abs[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]]
  def densematrixview_exp[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]]
  def densematrixview_log[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]]
  def densematrixview_sumrows[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densematrixview_sumcols[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densematrixview_minrows[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[DenseVector[T]]
  def densematrixview_mincols[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[DenseVector[T]]
  def densematrixview_maxrows[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[DenseVector[T]]
  def densematrixview_maxcols[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[DenseVector[T]]
  def densematrixview_min[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def densematrixview_max[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def densematrixview_minindex[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[Tup2[Int,Int]]
  def densematrixview_maxindex[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[Tup2[Int,Int]]
  def densematrixview_map[T:Typ,R:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseMatrix[R]]
  def densematrixview_reduce[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densematrixview_foreach[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def densematrixview_zip[T:Typ,B:Typ,R:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[DenseMatrix[B]],__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseMatrix[R]]
  def densematrixview_count[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Int]
  def densematrixview_maprowstovector[T:Typ,R:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densematrixview_mapcolstovector[T:Typ,R:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densematrixview_findrows[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def densematrixview_findcols[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def densematrixview_filterrows[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrixview_filtercols[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densematrixview_foreachrow[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def densematrixview_foreachcol[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def densematrixview_maprows[T:Typ,R:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[DenseVector[R]])(implicit __pos: SourceContext): Rep[DenseMatrix[R]]
  def densematrixview_mapcols[T:Typ,R:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[DenseVector[R]])(implicit __pos: SourceContext): Rep[DenseMatrix[R]]
  def densematrixview_reducerows[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVector[T]],Rep[DenseVector[T]]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densematrixview_reducecols[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: (Rep[DenseVector[T]],Rep[DenseVector[T]]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
}
trait DenseMatrixViewCompilerOps extends DenseMatrixViewOps {
  this: OptiML => 

  def densematrixview_data[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[ForgeArray[T]]
  def densematrixview_startrow[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrixview_endrow[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrixview_startcol[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrixview_endcol[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrixview_srcnumrows[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrixview_srcnumcols[T:Typ](self: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densematrixview_illegalalloc[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Nothing]
  def densematrixview_illegalupdate[T:Typ](self: Rep[DenseMatrixView[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __pos: SourceContext): Rep[Nothing]
}

