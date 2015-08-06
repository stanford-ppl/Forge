package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DenseVectorViewOpsBase extends Base {
  this: OptiML => 

  implicit def dist(__arg0: Rep[DenseVectorView[Double]],__arg1: Rep[DenseVectorView[Double]])(implicit __pos: SourceContext,__imp1: ROverload4): Rep[Double] = { dist(__arg0,__arg1,ABS) }
  implicit def viewToDense[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload1): Rep[DenseVector[T]] = densevectorview_viewtodense[T](self)(implicitly[Typ[T]],__pos)
  implicit def chainViewToDenseOps[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload1): DenseVectorDenseVectorOpsCls[T] = densevectorview_chainviewtodenseops[T](self)(implicitly[Typ[T]],__pos)

  def densevectorview_viewtodense[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevectorview_chainviewtodenseops[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): DenseVectorDenseVectorOpsCls[T]
}

trait DenseVectorViewOps extends DenseVectorViewOpsBase {
  this: OptiML => 

  object DenseVectorView {
    def apply[T:Typ](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[Int],__arg4: Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload13) = densevectorview_object_apply[T](__arg0,__arg1,__arg2,__arg3,__arg4)(implicitly[Typ[T]],__pos)
  }

  def dist(__arg0: Rep[DenseVectorView[Double]],__arg1: Rep[DenseVectorView[Double]],__arg2: DistanceMetric)(implicit __pos: SourceContext,__imp1: Overload2) = densevectorview_dist(__arg0,__arg1,__arg2)(__pos)
  def norm(__arg0: Rep[DenseVectorView[Double]])(implicit __pos: SourceContext,__imp1: ROverload3): Rep[Double] = { norm(__arg0,L2) }
  def norm(__arg0: Rep[DenseVectorView[Double]],__arg1: NormId)(implicit __pos: SourceContext,__imp1: Overload4) = densevectorview_norm(__arg0,__arg1)(__pos,overload4)
  def __equal[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload4) = densevectorview___equal[T](self,__arg1)(implicitly[Typ[T]],__pos)

  implicit def repToDenseVectorViewDenseVectorViewOpsCls[T:Typ](x: Rep[DenseVectorView[T]])(implicit __pos: SourceContext) = new DenseVectorViewDenseVectorViewOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToDenseVectorViewDenseVectorViewOpsCls[T:Typ](x: Var[DenseVectorView[T]])(implicit __pos: SourceContext) = new DenseVectorViewDenseVectorViewOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class DenseVectorViewDenseVectorViewOpsCls[T:Typ](val self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext) {
    def length(implicit __pos: SourceContext,__imp1: Overload2) = densevectorview_length[T](self)(implicitly[Typ[T]],__pos)
    def isRow(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_isrow[T](self)(implicitly[Typ[T]],__pos)
    def apply(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload10) = densevectorview_apply_int[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def apply(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload11) = densevectorview_apply[T](self,__arg1)(implicitly[Typ[T]],__pos,overload11)
    def slice(start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3) = densevectorview_slice[T](self,start,end)(implicitly[Typ[T]],__pos)
    def Clone(implicit __pos: SourceContext,__imp1: ROverload2) = { self.toDense }
    def toDense(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_todense[T](self)(implicitly[Typ[T]],__pos)
    def filter(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_filter[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def toBoolean(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Boolean],__imp2: Overload3) = densevectorview_toboolean[T](self)(implicitly[Typ[T]],__pos,conv)
    def toDouble(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload4) = densevectorview_todouble[T](self)(implicitly[Typ[T]],__pos,conv)
    def toFloat(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Float],__imp2: Overload4) = densevectorview_tofloat[T](self)(implicitly[Typ[T]],__pos,conv)
    def toInt(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Int],__imp2: Overload4) = densevectorview_toint[T](self)(implicitly[Typ[T]],__pos,conv)
    def indices(implicit __pos: SourceContext,__imp1: Overload2) = densevectorview_indices[T](self)(implicitly[Typ[T]],__pos)
    def isEmpty(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_isempty[T](self)(implicitly[Typ[T]],__pos)
    def first(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_first[T](self)(implicitly[Typ[T]],__pos)
    def last(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_last[T](self)(implicitly[Typ[T]],__pos)
    def drop(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_drop[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def take(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_take[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def contains(__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload4) = densevectorview_contains[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def histogram(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_histogram[T](self)(implicitly[Typ[T]],__pos)
    def distinct(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_distinct[T](self)(implicitly[Typ[T]],__pos)
    def mutable()(implicit __pos: SourceContext,__imp1: Overload5) = densevectorview_mutable[T](self)(implicitly[Typ[T]],__pos)
    def replicate(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = densevectorview_replicate[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)
    def makeString(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload2) = densevectorview_makestring[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def toString(implicit __imp0: Overload2) = densevectorview_tostring[T](self)
    def pprint()(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload2) = densevectorview_pprint[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def makeStrWithDelim(delim: Rep[String])(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload1) = densevectorview_makestrwithdelim[T](self,delim)(implicitly[Typ[T]],__pos,__imp0)
    def +(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload180) = densevectorview_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload180)
    def -(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload164) = densevectorview_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload164)
    def *(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload175) = densevectorview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload175)
    def *:*(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload12) = densevectorview_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload12)
    def **(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densevectorview_mulmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload1)
    def /(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload155) = densevectorview_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload155)
    def +(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload181) = densevectorview_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload181)
    def -(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload165) = densevectorview_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload165)
    def *(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload176) = densevectorview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload176)
    def *:*(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload13) = densevectorview_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload13)
    def **(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densevectorview_mulmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload2)
    def /(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload156) = densevectorview_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload156)
    def +(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload182) = densevectorview_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload182)
    def -(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload166) = densevectorview_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload166)
    def *(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload177) = densevectorview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload177)
    def *:*(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload14) = densevectorview_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload14)
    def **(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densevectorview_mulmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload3)
    def /(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload157) = densevectorview_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload157)
    def +(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload183) = densevectorview_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload183)
    def -(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload167) = densevectorview_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload167)
    def *(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload178) = densevectorview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload178)
    def *:*(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload15) = densevectorview_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload15)
    def **(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4) = densevectorview_mulmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload4)
    def /(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload158) = densevectorview_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload158)
    def zip[B:Typ,R:Typ](__arg1: Rep[DenseVector[B]])(__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload2) = densevectorview_zip[T,B,R](self,__arg1,__arg2)(implicitly[Typ[T]],implicitly[Typ[B]],implicitly[Typ[R]],__pos,overload2)
    def zip[B:Typ,R:Typ](__arg1: Rep[DenseVectorView[B]])(__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload3) = densevectorview_zip[T,B,R](self,__arg1,__arg2)(implicitly[Typ[T]],implicitly[Typ[B]],implicitly[Typ[R]],__pos,overload3)
    def +(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload184) = densevectorview_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload184)
    def -(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload168) = densevectorview_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload168)
    def *(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload179) = densevectorview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload179)
    def *(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload180) = densevectorview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload180)
    def /(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload159) = densevectorview_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload159)
    def abs(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload7) = densevectorview_abs[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def exp(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload7) = densevectorview_exp[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def log(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload7) = densevectorview_log[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def sum(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densevectorview_sum[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def prod(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = densevectorview_prod[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def mean(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload2) = densevectorview_mean[T](self)(implicitly[Typ[T]],__pos,conv)
    def min(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload5) = densevectorview_min[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def max(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload5) = densevectorview_max[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def minIndex(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload2) = densevectorview_minindex[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def maxIndex(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload2) = densevectorview_maxindex[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def map[R:Typ](__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload3) = densevectorview_map[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def reduce(__arg1: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densevectorview_reduce[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def foreach(__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload3) = densevectorview_foreach[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def forall(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_forall[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def find(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_find[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def count(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload2) = densevectorview_count[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def partition(pred: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_partition[T](self,pred)(implicitly[Typ[T]],__pos)
    def flatMap[R:Typ](__arg1: (Rep[T]) => Rep[DenseVector[R]])(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_flatmap[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def scanLeft[R:Typ](zero: Rep[R])(__arg2: (Rep[R],Rep[T]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_scanleft[T,R](self,zero,__arg2)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def scanRight[R:Typ](zero: Rep[R])(__arg2: (Rep[T],Rep[R]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload1) = densevectorview_scanright[T,R](self,zero,__arg2)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def prefixSum(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1) = densevectorview_prefixsum[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def toArray(implicit __pos: SourceContext,__imp1: Overload2) = densevectorview_toarray[T](self)(implicitly[Typ[T]],__pos)
  }



  def densevectorview_dist(__arg0: Rep[DenseVectorView[Double]],__arg1: Rep[DenseVectorView[Double]],__arg2: DistanceMetric)(implicit __pos: SourceContext): Rep[Double]
  def densevectorview_norm(__arg0: Rep[DenseVectorView[Double]],__arg1: NormId)(implicit __pos: SourceContext,__imp1: Overload4): Rep[Double]
  def densevectorview_object_apply[T:Typ](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[Int],__arg4: Rep[Boolean])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def densevectorview_length[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densevectorview_isrow[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[Boolean]
  def densevectorview_apply_int[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[T]
  def densevectorview_apply[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload11): Rep[DenseVector[T]]
  def densevectorview_slice[T:Typ](self: Rep[DenseVectorView[T]],start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def densevectorview_todense[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevectorview___equal[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[Boolean]
  def densevectorview_filter[T:Typ](self: Rep[DenseVectorView[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevectorview_toboolean[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Boolean]): Rep[DenseVector[Boolean]]
  def densevectorview_todouble[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double]): Rep[DenseVector[Double]]
  def densevectorview_tofloat[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Float]): Rep[DenseVector[Float]]
  def densevectorview_toint[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Int]): Rep[DenseVector[Int]]
  def densevectorview_indices[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def densevectorview_isempty[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[Boolean]
  def densevectorview_first[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[T]
  def densevectorview_last[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[T]
  def densevectorview_drop[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def densevectorview_take[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def densevectorview_contains[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[T])(implicit __pos: SourceContext): Rep[Boolean]
  def densevectorview_histogram[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[ForgeHashMap[T,Int]]
  def densevectorview_distinct[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevectorview_mutable[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevectorview_replicate[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densevectorview_makestring[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[String]
  def densevectorview_tostring[T:Typ](self: Rep[DenseVectorView[T]]): Rep[String]
  def densevectorview_pprint[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[Unit]
  def densevectorview_makestrwithdelim[T:Typ](self: Rep[DenseVectorView[T]],delim: Rep[String])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[String]
  def densevectorview_pl[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload180): Rep[DenseVector[T]]
  def densevectorview_sub[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload164): Rep[DenseVector[T]]
  def densevectorview_mul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload175): Rep[DenseVector[T]]
  def densevectorview_mulclnmul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload12): Rep[T]
  def densevectorview_mulmul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload1): Rep[DenseMatrix[T]]
  def densevectorview_div[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload155): Rep[DenseVector[T]]
  def densevectorview_pl[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload181): Rep[DenseVector[T]]
  def densevectorview_sub[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload165): Rep[DenseVector[T]]
  def densevectorview_mul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload176): Rep[DenseVector[T]]
  def densevectorview_mulclnmul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload13): Rep[T]
  def densevectorview_mulmul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2): Rep[DenseMatrix[T]]
  def densevectorview_div[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload156): Rep[DenseVector[T]]
  def densevectorview_pl[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload182): Rep[DenseVector[T]]
  def densevectorview_sub[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload166): Rep[DenseVector[T]]
  def densevectorview_mul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload177): Rep[SparseVector[T]]
  def densevectorview_mulclnmul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload14): Rep[T]
  def densevectorview_mulmul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3): Rep[DenseMatrix[T]]
  def densevectorview_div[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload157): Rep[DenseVector[T]]
  def densevectorview_pl[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload183): Rep[DenseVector[T]]
  def densevectorview_sub[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload167): Rep[DenseVector[T]]
  def densevectorview_mul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload178): Rep[SparseVector[T]]
  def densevectorview_mulclnmul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload15): Rep[T]
  def densevectorview_mulmul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4): Rep[DenseMatrix[T]]
  def densevectorview_div[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload158): Rep[DenseVector[T]]
  def densevectorview_zip[T:Typ,B:Typ,R:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVector[B]],__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload2): Rep[DenseVector[R]]
  def densevectorview_zip[T:Typ,B:Typ,R:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseVectorView[B]],__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload3): Rep[DenseVector[R]]
  def densevectorview_pl[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload184): Rep[DenseVector[T]]
  def densevectorview_sub[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload168): Rep[DenseVector[T]]
  def densevectorview_mul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload179): Rep[DenseVector[T]]
  def densevectorview_mul[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload180): Rep[DenseVector[T]]
  def densevectorview_div[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload159): Rep[DenseVector[T]]
  def densevectorview_abs[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densevectorview_exp[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densevectorview_log[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densevectorview_sum[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densevectorview_prod[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densevectorview_mean[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double]): Rep[Double]
  def densevectorview_min[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def densevectorview_max[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def densevectorview_minindex[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[Int]
  def densevectorview_maxindex[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[Int]
  def densevectorview_map[T:Typ,R:Typ](self: Rep[DenseVectorView[T]],__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densevectorview_reduce[T:Typ](self: Rep[DenseVectorView[T]],__arg1: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densevectorview_foreach[T:Typ](self: Rep[DenseVectorView[T]],__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def densevectorview_forall[T:Typ](self: Rep[DenseVectorView[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Boolean]
  def densevectorview_find[T:Typ](self: Rep[DenseVectorView[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def densevectorview_count[T:Typ](self: Rep[DenseVectorView[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Int]
  def densevectorview_partition[T:Typ](self: Rep[DenseVectorView[T]],pred: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Tup2[DenseVector[T],DenseVector[T]]]
  def densevectorview_flatmap[T:Typ,R:Typ](self: Rep[DenseVectorView[T]],__arg1: (Rep[T]) => Rep[DenseVector[R]])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densevectorview_scanleft[T:Typ,R:Typ](self: Rep[DenseVectorView[T]],zero: Rep[R],__arg2: (Rep[R],Rep[T]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densevectorview_scanright[T:Typ,R:Typ](self: Rep[DenseVectorView[T]],zero: Rep[R],__arg2: (Rep[T],Rep[R]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densevectorview_prefixsum[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densevectorview_toarray[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[ForgeArray[T]]
}
trait DenseVectorViewCompilerOps extends DenseVectorViewOps {
  this: OptiML => 

  def densevectorview_data[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[ForgeArray[T]]
  def densevectorview_start[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densevectorview_stride[T:Typ](self: Rep[DenseVectorView[T]])(implicit __pos: SourceContext): Rep[Int]
  def densevectorview_illegalalloc[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Nothing]
  def densevectorview_illegalupdate[T:Typ](self: Rep[DenseVectorView[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __pos: SourceContext): Rep[Nothing]
  def densevectorview_densevector_filter_map[T:Typ,R:Typ](self: Rep[DenseVectorView[T]],__arg1: (Rep[T]) => Rep[Boolean],__arg2: (Rep[T]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
}

