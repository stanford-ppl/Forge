package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DenseVectorOpsBase extends Base {
  this: OptiML => 

  implicit def dist(__arg0: Rep[DenseVector[Double]],__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload6): Rep[Double] = { dist(__arg0,__arg1,ABS) }

}

trait DenseVectorOps extends DenseVectorOpsBase {
  this: OptiML => 

  object DenseVector {
    def apply[T:Typ](__arg0: Rep[Int],__arg1: Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload34) = densevector_object_apply[T](__arg0,__arg1)(implicitly[Typ[T]],__pos,overload34)
    def apply[T:Typ](__arg0: Rep[T]*)(implicit __pos: SourceContext,__imp1: Overload35) = densevector_object_apply[T](__arg0)(implicitly[Typ[T]],__pos,overload35)
    def apply[T:Typ](__arg0: Rep[ForgeArray[T]],isRow: Rep[Boolean] = unit(true))(implicit __pos: SourceContext,__imp1: ROverload36): Rep[DenseVector[T]] = { densevector_fromarray(__arg0, isRow) }
    def zeros(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4) = densevector_object_zeros(__arg0)(__pos)
    def zerosf(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4) = densevector_object_zerosf(__arg0)(__pos)
    def ones(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = densevector_object_ones(__arg0)(__pos)
    def onesf(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = densevector_object_onesf(__arg0)(__pos)
    def rand(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4) = densevector_object_rand(__arg0)(__pos)
    def randf(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4) = densevector_object_randf(__arg0)(__pos)
    def uniform(start: Rep[Double],step_size: Rep[Double],end: Rep[Double],isRow: Rep[Boolean] = unit(true))(implicit __pos: SourceContext) = densevector_object_uniform(start,step_size,end,isRow)(__pos)
    def flatten[T:Typ](pieces: Rep[DenseVector[DenseVector[T]]])(implicit __pos: SourceContext) = densevector_object_flatten[T](pieces)(implicitly[Typ[T]],__pos)
  }

  def dist(__arg0: Rep[DenseVector[Double]],__arg1: Rep[DenseVector[Double]],__arg2: DistanceMetric)(implicit __pos: SourceContext,__imp1: Overload4) = densevector_dist(__arg0,__arg1,__arg2)(__pos)
  def norm(__arg0: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload7): Rep[Double] = { norm(__arg0,L2) }
  def norm(__arg0: Rep[DenseVector[Double]],__arg1: NormId)(implicit __pos: SourceContext,__imp1: Overload8) = densevector_norm(__arg0,__arg1)(__pos,overload8)
  def densevector_fromarray[T:Typ](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Boolean])(implicit __pos: SourceContext) = densevector_densevector_fromarray[T](__arg0,__arg1)(implicitly[Typ[T]],__pos)
  def densevector_fromfunc[T:Typ](__arg0: Rep[Int],__arg1: Rep[Boolean],__arg2: (Rep[Int]) => Rep[T])(implicit __pos: SourceContext) = densevector_densevector_fromfunc[T](__arg0,__arg1,__arg2)(implicitly[Typ[T]],__pos)
  def __equal[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload24) = densevector___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload24)
  def __equal[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload25) = densevector___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload25)
  def __equal[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload26) = densevector___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload26)
  def __equal[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload27) = densevector___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload27)

  implicit def repToDenseVectorForgeHashMapOpsCls[T:Typ,R:Typ](x: Rep[ForgeHashMap[T,R]])(implicit __pos: SourceContext) = new DenseVectorForgeHashMapOpsCls(x)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
  implicit def varToDenseVectorForgeHashMapOpsCls[T:Typ,R:Typ](x: Var[ForgeHashMap[T,R]])(implicit __pos: SourceContext) = new DenseVectorForgeHashMapOpsCls(readVar(x))(implicitly[Typ[T]],implicitly[Typ[R]],__pos)

  class DenseVectorForgeHashMapOpsCls[T:Typ,R:Typ](val self: Rep[ForgeHashMap[T,R]])(implicit __pos: SourceContext) {
    def toVector(implicit __pos: SourceContext) = densevector_tovector[T,R](self)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
  }

  implicit def repToDenseVectorDenseVectorOpsCls[T:Typ](x: Rep[DenseVector[T]])(implicit __pos: SourceContext) = new DenseVectorDenseVectorOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToDenseVectorDenseVectorOpsCls[T:Typ](x: Var[DenseVector[T]])(implicit __pos: SourceContext) = new DenseVectorDenseVectorOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class DenseVectorDenseVectorOpsCls[T:Typ](val self: Rep[DenseVector[T]])(implicit __pos: SourceContext) {
    def length(implicit __pos: SourceContext,__imp1: Overload6) = densevector_length[T](self)(implicitly[Typ[T]],__pos)
    def isRow(implicit __pos: SourceContext,__imp1: Overload5) = densevector_isrow[T](self)(implicitly[Typ[T]],__pos)
    def apply(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload44) = densevector_apply[T](self,__arg1)(implicitly[Typ[T]],__pos,overload44)
    def apply(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload45) = densevector_apply[T](self,__arg1)(implicitly[Typ[T]],__pos,overload45)
    def slice(start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload8) = { self(start::end) }
    def t(implicit __pos: SourceContext,__imp1: Overload6) = densevector_t[T](self)(implicitly[Typ[T]],__pos)
    def mt()(implicit __pos: SourceContext,__imp1: Overload2) = densevector_mt[T](self)(implicitly[Typ[T]],__pos)
    def toMat(implicit __pos: SourceContext) = densevector_tomat[T](self)(implicitly[Typ[T]],__pos)
    def toDense(implicit __pos: SourceContext,__imp1: ROverload7) = { self }
    def Clone(implicit __pos: SourceContext,__imp1: Overload8) = densevector_clone[T](self)(implicitly[Typ[T]],__pos)
    def update(i: Rep[Int],e: Rep[T])(implicit __pos: SourceContext,__imp1: Overload11) = densevector_update[T](self,i,e)(implicitly[Typ[T]],__pos,overload11)
    def update(indices: Rep[IndexVector],e: Rep[T])(implicit __pos: SourceContext,__imp1: Overload12) = densevector_update[T](self,indices,e)(implicitly[Typ[T]],__pos,overload12)
    def update(indices: Rep[IndexVector],v: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload13) = densevector_update[T](self,indices,v)(implicitly[Typ[T]],__pos,overload13)
    def <<(__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload7) = densevector_ltlt[T](self,__arg1)(implicitly[Typ[T]],__pos,overload7)
    def <<(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload8) = densevector_ltlt[T](self,__arg1)(implicitly[Typ[T]],__pos,overload8)
    def <<=(__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload6) = densevector_ltlteq[T](self,__arg1)(implicitly[Typ[T]],__pos,overload6)
    def <<=(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload7) = densevector_ltlteq[T](self,__arg1)(implicitly[Typ[T]],__pos,overload7)
    def insert(__arg1: Rep[Int],__arg2: Rep[T])(implicit __pos: SourceContext,__imp1: Overload2) = densevector_insert[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)
    def insertAll(__arg1: Rep[Int],__arg2: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload2) = densevector_insertall[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)
    def remove(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = densevector_remove[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def removeAll(pos: Rep[Int],len: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = densevector_removeall[T](self,pos,len)(implicitly[Typ[T]],__pos)
    def copyFrom(__arg1: Rep[Int],__arg2: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload2) = densevector_copyfrom[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)
    def trim()(implicit __pos: SourceContext,__imp1: Overload4) = densevector_trim[T](self)(implicitly[Typ[T]],__pos)
    def clear()(implicit __pos: SourceContext,__imp1: Overload2) = densevector_clear[T](self)(implicitly[Typ[T]],__pos)
    def +=(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densevector_pleq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload3)
    def +=(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4) = densevector_pleq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload4)
    def +=(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5) = densevector_pleq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload5)
    def *=(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densevector_muleq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload3)
    def *=(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4) = densevector_muleq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload4)
    def *=(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5) = densevector_muleq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload5)
    def -=(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densevector_subeq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload3)
    def -=(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4) = densevector_subeq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload4)
    def -=(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5) = densevector_subeq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload5)
    def /=(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densevector_diveq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload3)
    def /=(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4) = densevector_diveq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload4)
    def /=(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5) = densevector_diveq[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload5)
    def sort(implicit __pos: SourceContext,__imp0: Ordering[T]) = densevector_sort[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def sortBy[B:Typ](__arg1: (Rep[T]) => Rep[B])(implicit __pos: SourceContext,__imp0: Ordering[B]) = densevector_sortby[T,B](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[B]],__pos,__imp0)
    def sortWithIndex(implicit __pos: SourceContext,__imp0: Ordering[T]) = densevector_sortwithindex[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def median(implicit __pos: SourceContext,__imp0: Numeric[T],__imp1: Ordering[T]) = densevector_median[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def :>(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload2) = densevector_clngt[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def :<(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload2) = densevector_clnlt[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def groupByReduce[K:Typ,V:Typ](__arg1: (Rep[T]) => Rep[K],__arg2: (Rep[T]) => Rep[V],__arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __pos: SourceContext,__imp0: Arith[V]) = densevector_groupbyreduce[T,K,V](self,__arg1,__arg2,__arg3)(implicitly[Typ[T]],implicitly[Typ[K]],implicitly[Typ[V]],__pos,__imp0)
    def groupBy[K:Typ,V:Typ](__arg1: (Rep[T]) => Rep[K],__arg2: (Rep[T]) => Rep[V])(implicit __pos: SourceContext) = densevector_groupby[T,K,V](self,__arg1,__arg2)(implicitly[Typ[T]],implicitly[Typ[K]],implicitly[Typ[V]],__pos)
    def filter(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload3) = densevector_filter[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def toBoolean(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Boolean],__imp2: Overload6) = densevector_toboolean[T](self)(implicitly[Typ[T]],__pos,conv)
    def toDouble(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload7) = densevector_todouble[T](self)(implicitly[Typ[T]],__pos,conv)
    def toFloat(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Float],__imp2: Overload7) = densevector_tofloat[T](self)(implicitly[Typ[T]],__pos,conv)
    def toInt(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Int],__imp2: Overload7) = densevector_toint[T](self)(implicitly[Typ[T]],__pos,conv)
    def indices(implicit __pos: SourceContext,__imp1: Overload7) = densevector_indices[T](self)(implicitly[Typ[T]],__pos)
    def isEmpty(implicit __pos: SourceContext,__imp1: Overload4) = densevector_isempty[T](self)(implicitly[Typ[T]],__pos)
    def first(implicit __pos: SourceContext,__imp1: Overload4) = densevector_first[T](self)(implicitly[Typ[T]],__pos)
    def last(implicit __pos: SourceContext,__imp1: Overload4) = densevector_last[T](self)(implicitly[Typ[T]],__pos)
    def drop(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4) = densevector_drop[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def take(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4) = densevector_take[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def contains(__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload11) = densevector_contains[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def histogram(implicit __pos: SourceContext,__imp1: Overload4) = densevector_histogram[T](self)(implicitly[Typ[T]],__pos)
    def distinct(implicit __pos: SourceContext,__imp1: Overload4) = densevector_distinct[T](self)(implicitly[Typ[T]],__pos)
    def mutable()(implicit __pos: SourceContext,__imp1: Overload20) = densevector_mutable[T](self)(implicitly[Typ[T]],__pos)
    def replicate(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload5) = densevector_replicate[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)
    def makeString(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload8) = densevector_makestring[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def toString(implicit __imp0: Overload17) = densevector_tostring[T](self)
    def pprint()(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload9) = densevector_pprint[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def makeStrWithDelim(delim: Rep[String])(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload3) = densevector_makestrwithdelim[T](self,delim)(implicitly[Typ[T]],__pos,__imp0)
    def +(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload210) = densevector_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload210)
    def -(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload194) = densevector_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload194)
    def *(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload215) = densevector_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload215)
    def *:*(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload29) = densevector_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload29)
    def **(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload10) = densevector_mulmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload10)
    def /(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload185) = densevector_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload185)
    def +(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload211) = densevector_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload211)
    def -(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload195) = densevector_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload195)
    def *(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload216) = densevector_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload216)
    def *:*(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload30) = densevector_dot_densevectorview[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def **(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload11) = densevector_mulmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload11)
    def /(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload186) = densevector_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload186)
    def +(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload212) = densevector_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload212)
    def -(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload196) = densevector_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload196)
    def *(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload217) = densevector_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload217)
    def *:*(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload31) = densevector_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload31)
    def **(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload12) = densevector_mulmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload12)
    def /(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload187) = densevector_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload187)
    def +(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload213) = densevector_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload213)
    def -(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload197) = densevector_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload197)
    def *(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload218) = densevector_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload218)
    def *:*(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload32) = densevector_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload32)
    def **(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload13) = densevector_mulmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload13)
    def /(__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload188) = densevector_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload188)
    def zip[B:Typ,R:Typ](__arg1: Rep[DenseVector[B]])(__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload7) = densevector_zip[T,B,R](self,__arg1,__arg2)(implicitly[Typ[T]],implicitly[Typ[B]],implicitly[Typ[R]],__pos,overload7)
    def zip[B:Typ,R:Typ](__arg1: Rep[DenseVectorView[B]])(__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload8) = densevector_zip[T,B,R](self,__arg1,__arg2)(implicitly[Typ[T]],implicitly[Typ[B]],implicitly[Typ[R]],__pos,overload8)
    def +(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload214) = densevector_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload214)
    def -(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload198) = densevector_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload198)
    def *(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload219) = densevector_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload219)
    def *(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload220) = densevector_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload220)
    def /(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload189) = densevector_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload189)
    def abs(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload23) = densevector_abs[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def exp(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload21) = densevector_exp[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def log(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload21) = densevector_log[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def sum(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload7) = densevector_sum[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def prod(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5) = densevector_prod[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def mean(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload7) = densevector_mean[T](self)(implicitly[Typ[T]],__pos,conv)
    def min(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload14) = densevector_min[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def max(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload14) = densevector_max[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def minIndex(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload5) = densevector_minindex[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def maxIndex(implicit __pos: SourceContext,__imp0: Ordering[T],__imp2: Overload5) = densevector_maxindex[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def map[R:Typ](__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload6) = densevector_map[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def reduce(__arg1: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload6) = densevector_reduce[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def foreach(__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload7) = densevector_foreach[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def forall(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload3) = densevector_forall[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def find(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload3) = densevector_find[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def count(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload5) = densevector_count[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def partition(pred: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload3) = densevector_partition[T](self,pred)(implicitly[Typ[T]],__pos)
    def flatMap[R:Typ](__arg1: (Rep[T]) => Rep[DenseVector[R]])(implicit __pos: SourceContext,__imp1: Overload3) = densevector_flatmap[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def scanLeft[R:Typ](zero: Rep[R])(__arg2: (Rep[R],Rep[T]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload3) = densevector_scanleft[T,R](self,zero,__arg2)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def scanRight[R:Typ](zero: Rep[R])(__arg2: (Rep[T],Rep[R]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload3) = densevector_scanright[T,R](self,zero,__arg2)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def prefixSum(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = densevector_prefixsum[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def toArray(implicit __pos: SourceContext,__imp1: Overload4) = densevector_toarray[T](self)(implicitly[Typ[T]],__pos)
  }

  implicit def repToDenseVectorIntOpsCls(x: Rep[Int])(implicit __pos: SourceContext) = new DenseVectorIntOpsCls(x)(__pos)
  implicit def varToDenseVectorIntOpsCls(x: Var[Int])(implicit __pos: SourceContext) = new DenseVectorIntOpsCls(readVar(x))(__pos)

  class DenseVectorIntOpsCls(val self: Rep[Int])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload215) = { densevector_pl[Int](__arg1,self) }
    def +(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload216) = { densevector_pl[Float](__arg1,self.toFloat) }
    def +(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload217) = { densevector_pl[Double](__arg1,self.toDouble) }
    def -(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload199) = { densevector_map[Int,Int](__arg1, e => forge_int_minus(self,e)) }
    def -(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload200) = { densevector_map[Float,Float](__arg1, e => forge_float_minus(self.toFloat,e)) }
    def -(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload201) = { densevector_map[Double,Double](__arg1, e => forge_double_minus(self.toDouble,e)) }
    def *(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload221) = { densevector_mul[Int](__arg1,self) }
    def *(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload222) = { densevector_mul[Float](__arg1,self.toFloat) }
    def *(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload223) = { densevector_mul[Double](__arg1,self.toDouble) }
  }

  implicit def repToDenseVectorFloatOpsCls(x: Rep[Float])(implicit __pos: SourceContext) = new DenseVectorFloatOpsCls(x)(__pos)
  implicit def varToDenseVectorFloatOpsCls(x: Var[Float])(implicit __pos: SourceContext) = new DenseVectorFloatOpsCls(readVar(x))(__pos)

  class DenseVectorFloatOpsCls(val self: Rep[Float])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload218) = { densevector_pl[Float](__arg1.toFloat,self) }
    def +(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload219) = { densevector_pl[Float](__arg1,self) }
    def +(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload220) = { densevector_pl[Double](__arg1,self.toDouble) }
    def -(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload202) = { densevector_map[Int,Float](__arg1, e => forge_float_minus(self,e)) }
    def -(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload203) = { densevector_map[Float,Float](__arg1, e => forge_float_minus(self,e)) }
    def -(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload204) = { densevector_map[Double,Double](__arg1, e => forge_double_minus(self.toDouble,e)) }
    def *(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload224) = { densevector_mul[Float](__arg1.toFloat,self) }
    def *(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload225) = { densevector_mul[Float](__arg1,self) }
    def *(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload226) = { densevector_mul[Double](__arg1,self.toDouble) }
  }

  implicit def repToDenseVectorDoubleOpsCls(x: Rep[Double])(implicit __pos: SourceContext) = new DenseVectorDoubleOpsCls(x)(__pos)
  implicit def varToDenseVectorDoubleOpsCls(x: Var[Double])(implicit __pos: SourceContext) = new DenseVectorDoubleOpsCls(readVar(x))(__pos)

  class DenseVectorDoubleOpsCls(val self: Rep[Double])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload221) = { densevector_pl[Double](__arg1.toDouble,self) }
    def +(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload222) = { densevector_pl[Double](__arg1.toDouble,self) }
    def +(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload223) = { densevector_pl[Double](__arg1,self) }
    def -(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload205) = { densevector_map[Int,Double](__arg1, e => forge_double_minus(self,e.toDouble)) }
    def -(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload206) = { densevector_map[Float,Double](__arg1, e => forge_double_minus(self,e.toDouble)) }
    def -(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload207) = { densevector_map[Double,Double](__arg1, e => forge_double_minus(self,e)) }
    def *(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload227) = { densevector_mul[Double](__arg1.toDouble,self) }
    def *(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload228) = { densevector_mul[Double](__arg1.toDouble,self) }
    def *(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload229) = { densevector_mul[Double](__arg1,self) }
  }

  implicit def repToDenseVectorDenseVectorIntOpsCls(x: Rep[DenseVector[Int]])(implicit __pos: SourceContext) = new DenseVectorDenseVectorIntOpsCls(x)(__pos)
  implicit def varToDenseVectorDenseVectorIntOpsCls(x: Var[DenseVector[Int]])(implicit __pos: SourceContext) = new DenseVectorDenseVectorIntOpsCls(readVar(x))(__pos)

  class DenseVectorDenseVectorIntOpsCls(val self: Rep[DenseVector[Int]])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload224) = { densevector_pl[Int](self,__arg1) }
    def +(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload225) = { densevector_pl[Float](self.toFloat,__arg1) }
    def +(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload226) = { densevector_pl[Double](self.toDouble,__arg1) }
    def +(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload233) = { densevector_pl[Int](self,__arg1) }
    def +(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload234) = { densevector_pl[Float](self.toFloat,__arg1) }
    def +(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload235) = { densevector_pl[Double](self.toDouble,__arg1) }
    def -(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload208) = { densevector_sub[Int](self,__arg1) }
    def -(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload209) = { densevector_sub[Float](self.toFloat,__arg1) }
    def -(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload210) = { densevector_sub[Double](self.toDouble,__arg1) }
    def -(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload217) = { densevector_sub[Int](self,__arg1) }
    def -(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload218) = { densevector_sub[Float](self.toFloat,__arg1) }
    def -(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload219) = { densevector_sub[Double](self.toDouble,__arg1) }
    def unary_-(implicit __pos: SourceContext,__imp1: ROverload8) = { densevector_mul[Int](self,unit(-1)) }
    def *(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload230) = { densevector_mul[Int](self,__arg1) }
    def *(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload231) = { densevector_mul[Float](self.toFloat,__arg1) }
    def *(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload232) = { densevector_mul[Double](self.toDouble,__arg1) }
    def *(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload239) = { densevector_mul[Int](self,__arg1) }
    def *(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload240) = { densevector_mul[Float](self.toFloat,__arg1) }
    def *(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload241) = { densevector_mul[Double](self.toDouble,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload248) = { densevector_mul[Int](self,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload249) = { densevector_mul[Float](self.toFloat,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload250) = { densevector_mul[Double](self.toDouble,__arg1) }
    def /(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload190) = { densevector_div[Int](self,__arg1) }
    def /(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload191) = { densevector_div[Float](self.toFloat,__arg1) }
    def /(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload192) = { densevector_div[Double](self.toDouble,__arg1) }
    def /(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload199) = { densevector_div[Int](self,__arg1) }
    def /(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload200) = { densevector_div[Float](self.toFloat,__arg1) }
    def /(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload201) = { densevector_div[Double](self.toDouble,__arg1) }
  }

  implicit def repToDenseVectorDenseVectorFloatOpsCls(x: Rep[DenseVector[Float]])(implicit __pos: SourceContext) = new DenseVectorDenseVectorFloatOpsCls(x)(__pos)
  implicit def varToDenseVectorDenseVectorFloatOpsCls(x: Var[DenseVector[Float]])(implicit __pos: SourceContext) = new DenseVectorDenseVectorFloatOpsCls(readVar(x))(__pos)

  class DenseVectorDenseVectorFloatOpsCls(val self: Rep[DenseVector[Float]])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload227) = { densevector_pl[Float](self,__arg1.toFloat) }
    def +(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload228) = { densevector_pl[Float](self,__arg1) }
    def +(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload229) = { densevector_pl[Double](self.toDouble,__arg1) }
    def +(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload236) = { densevector_pl[Float](self,__arg1.toFloat) }
    def +(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload237) = { densevector_pl[Float](self,__arg1) }
    def +(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload238) = { densevector_pl[Double](self.toDouble,__arg1) }
    def -(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload211) = { densevector_sub[Float](self,__arg1.toFloat) }
    def -(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload212) = { densevector_sub[Float](self,__arg1) }
    def -(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload213) = { densevector_sub[Double](self.toDouble,__arg1) }
    def -(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload220) = { densevector_sub[Float](self,__arg1.toFloat) }
    def -(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload221) = { densevector_sub[Float](self,__arg1) }
    def -(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload222) = { densevector_sub[Double](self.toDouble,__arg1) }
    def unary_-(implicit __pos: SourceContext,__imp1: ROverload9) = { densevector_mul[Float](self,unit(-1f)) }
    def *(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload233) = { densevector_mul[Float](self,__arg1.toFloat) }
    def *(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload234) = { densevector_mul[Float](self,__arg1) }
    def *(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload235) = { densevector_mul[Double](self.toDouble,__arg1) }
    def *(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload242) = { densevector_mul[Float](self,__arg1.toFloat) }
    def *(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload243) = { densevector_mul[Float](self,__arg1) }
    def *(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload244) = { densevector_mul[Double](self.toDouble,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload251) = { densevector_mul[Float](self,__arg1.toFloat) }
    def *(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload252) = { densevector_mul[Float](self,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload253) = { densevector_mul[Double](self.toDouble,__arg1) }
    def /(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload193) = { densevector_div[Float](self,__arg1.toFloat) }
    def /(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload194) = { densevector_div[Float](self,__arg1) }
    def /(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload195) = { densevector_div[Double](self.toDouble,__arg1) }
    def /(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload202) = { densevector_div[Float](self,__arg1.toFloat) }
    def /(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload203) = { densevector_div[Float](self,__arg1) }
    def /(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload204) = { densevector_div[Double](self.toDouble,__arg1) }
  }

  implicit def repToDenseVectorDenseVectorDoubleOpsCls(x: Rep[DenseVector[Double]])(implicit __pos: SourceContext) = new DenseVectorDenseVectorDoubleOpsCls(x)(__pos)
  implicit def varToDenseVectorDenseVectorDoubleOpsCls(x: Var[DenseVector[Double]])(implicit __pos: SourceContext) = new DenseVectorDenseVectorDoubleOpsCls(readVar(x))(__pos)

  class DenseVectorDenseVectorDoubleOpsCls(val self: Rep[DenseVector[Double]])(implicit __pos: SourceContext) {
    def +(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload230) = { densevector_pl[Double](self,__arg1.toDouble) }
    def +(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload231) = { densevector_pl[Double](self.toDouble,__arg1) }
    def +(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload232) = { densevector_pl[Double](self,__arg1) }
    def +(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload239) = { densevector_pl[Double](self,__arg1.toDouble) }
    def +(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload240) = { densevector_pl[Double](self,__arg1.toDouble) }
    def +(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload241) = { densevector_pl[Double](self,__arg1) }
    def -(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload214) = { densevector_sub[Double](self,__arg1.toDouble) }
    def -(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload215) = { densevector_sub[Double](self.toDouble,__arg1) }
    def -(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload216) = { densevector_sub[Double](self,__arg1) }
    def -(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload223) = { densevector_sub[Double](self,__arg1.toDouble) }
    def -(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload224) = { densevector_sub[Double](self,__arg1.toDouble) }
    def -(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload225) = { densevector_sub[Double](self,__arg1) }
    def unary_-(implicit __pos: SourceContext,__imp1: ROverload10) = { densevector_mul[Double](self,unit(-1.0)) }
    def *(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload236) = { densevector_mul[Double](self,__arg1.toDouble) }
    def *(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload237) = { densevector_mul[Double](self.toDouble,__arg1) }
    def *(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload238) = { densevector_mul[Double](self,__arg1) }
    def *(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload245) = { densevector_mul[Double](self,__arg1.toDouble) }
    def *(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload246) = { densevector_mul[Double](self,__arg1.toDouble) }
    def *(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload247) = { densevector_mul[Double](self,__arg1) }
    def *(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: ROverload254) = { densevector_mul[Double](self,__arg1.toDouble) }
    def *(__arg1: Rep[DenseMatrix[Float]])(implicit __pos: SourceContext,__imp1: ROverload255) = { densevector_mul[Double](self,__arg1.toDouble) }
    def *(__arg1: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload256) = { densevector_mul[Double](self,__arg1) }
    def /(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload196) = { densevector_div[Double](self,__arg1.toDouble) }
    def /(__arg1: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload197) = { densevector_div[Double](self.toDouble,__arg1) }
    def /(__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload198) = { densevector_div[Double](self,__arg1) }
    def /(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload205) = { densevector_div[Double](self,__arg1.toDouble) }
    def /(__arg1: Rep[DenseVector[Float]])(implicit __pos: SourceContext,__imp1: ROverload206) = { densevector_div[Double](self,__arg1.toDouble) }
    def /(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload207) = { densevector_div[Double](self,__arg1) }
  }



  def densevector_dist(__arg0: Rep[DenseVector[Double]],__arg1: Rep[DenseVector[Double]],__arg2: DistanceMetric)(implicit __pos: SourceContext): Rep[Double]
  def densevector_norm(__arg0: Rep[DenseVector[Double]],__arg1: NormId)(implicit __pos: SourceContext,__imp1: Overload8): Rep[Double]
  def densevector_object_apply[T:Typ](__arg0: Rep[Int],__arg1: Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload34): Rep[DenseVector[T]]
  def densevector_object_apply[T:Typ](__arg0: Seq[Rep[T]])(implicit __pos: SourceContext,__imp1: Overload35): Rep[DenseVector[T]]
  def densevector_densevector_fromarray[T:Typ](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Boolean])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevector_densevector_fromfunc[T:Typ](__arg0: Rep[Int],__arg1: Rep[Boolean],__arg2: (Rep[Int]) => Rep[T])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevector_object_zeros(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVector[Double]]
  def densevector_object_zerosf(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVector[Float]]
  def densevector_object_ones(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVector[Double]]
  def densevector_object_onesf(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVector[Float]]
  def densevector_object_rand(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVector[Double]]
  def densevector_object_randf(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVector[Float]]
  def densevector_object_uniform(start: Rep[Double],step_size: Rep[Double],end: Rep[Double],isRow: Rep[Boolean] = unit(true))(implicit __pos: SourceContext): Rep[DenseVector[Double]]
  def densevector_object_flatten[T:Typ](pieces: Rep[DenseVector[DenseVector[T]]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevector_tovector[T:Typ,R:Typ](__arg0: Rep[ForgeHashMap[T,R]])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densevector_length[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[Int]
  def densevector_isrow[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[Boolean]
  def densevector_apply[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload44): Rep[T]
  def densevector_apply[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload45): Rep[DenseVector[T]]
  def densevector_t[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevector_mt[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_tomat[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densevector_clone[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevector_update[T:Typ](self: Rep[DenseVector[T]],i: Rep[Int],e: Rep[T])(implicit __pos: SourceContext,__imp1: Overload11): Rep[Unit]
  def densevector_update[T:Typ](self: Rep[DenseVector[T]],indices: Rep[IndexVector],e: Rep[T])(implicit __pos: SourceContext,__imp1: Overload12): Rep[Unit]
  def densevector_update[T:Typ](self: Rep[DenseVector[T]],indices: Rep[IndexVector],v: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload13): Rep[Unit]
  def densevector_ltlt[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload7): Rep[DenseVector[T]]
  def densevector_ltlt[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload8): Rep[DenseVector[T]]
  def densevector_ltlteq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload6): Rep[Unit]
  def densevector_ltlteq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload7): Rep[Unit]
  def densevector_insert[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_insertall[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int],__arg2: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_remove[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_removeall[T:Typ](self: Rep[DenseVector[T]],pos: Rep[Int],len: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_copyfrom[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int],__arg2: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_trim[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_clear[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_pleq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3): Rep[Unit]
  def densevector_pleq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4): Rep[Unit]
  def densevector_pleq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5): Rep[Unit]
  def densevector_muleq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3): Rep[Unit]
  def densevector_muleq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4): Rep[Unit]
  def densevector_muleq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5): Rep[Unit]
  def densevector_subeq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3): Rep[Unit]
  def densevector_subeq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4): Rep[Unit]
  def densevector_subeq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5): Rep[Unit]
  def densevector_diveq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3): Rep[Unit]
  def densevector_diveq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4): Rep[Unit]
  def densevector_diveq[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5): Rep[Unit]
  def densevector_sort[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[DenseVector[T]]
  def densevector_sortby[T:Typ,B:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[B])(implicit __pos: SourceContext,__imp0: Ordering[B]): Rep[DenseVector[T]]
  def densevector_sortwithindex[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Tuple2[Rep[DenseVector[T]],Rep[IndexVector]]
  def densevector_median[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Numeric[T],__imp1: Ordering[T]): Rep[T]
  def densevector_clngt[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[DenseVector[Boolean]]
  def densevector_clnlt[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[DenseVector[Boolean]]
  def densevector___equal[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload24): Rep[Boolean]
  def densevector___equal[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload25): Rep[Boolean]
  def densevector___equal[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload26): Rep[Boolean]
  def densevector___equal[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload27): Rep[Boolean]
  def densevector_groupbyreduce[T:Typ,K:Typ,V:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[K],__arg2: (Rep[T]) => Rep[V],__arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __pos: SourceContext,__imp0: Arith[V]): Rep[ForgeHashMap[K,V]]
  def densevector_groupby[T:Typ,K:Typ,V:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[K],__arg2: (Rep[T]) => Rep[V])(implicit __pos: SourceContext): Rep[ForgeHashMap[K,DenseVector[V]]]
  def densevector_filter[T:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevector_toboolean[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Boolean]): Rep[DenseVector[Boolean]]
  def densevector_todouble[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double]): Rep[DenseVector[Double]]
  def densevector_tofloat[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Float]): Rep[DenseVector[Float]]
  def densevector_toint[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Int]): Rep[DenseVector[Int]]
  def densevector_indices[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def densevector_isempty[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[Boolean]
  def densevector_first[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[T]
  def densevector_last[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[T]
  def densevector_drop[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevector_take[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevector_contains[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext): Rep[Boolean]
  def densevector_histogram[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[ForgeHashMap[T,Int]]
  def densevector_distinct[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevector_mutable[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevector_replicate[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[T]]
  def densevector_makestring[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[String]
  def densevector_tostring[T:Typ](self: Rep[DenseVector[T]]): Rep[String]
  def densevector_pprint[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[Unit]
  def densevector_makestrwithdelim[T:Typ](self: Rep[DenseVector[T]],delim: Rep[String])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[String]
  def densevector_pl[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload210): Rep[DenseVector[T]]
  def densevector_sub[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload194): Rep[DenseVector[T]]
  def densevector_mul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload215): Rep[DenseVector[T]]
  def densevector_mulclnmul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload29): Rep[T]
  def densevector_mulmul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload10): Rep[DenseMatrix[T]]
  def densevector_div[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload185): Rep[DenseVector[T]]
  def densevector_pl[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload211): Rep[DenseVector[T]]
  def densevector_sub[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload195): Rep[DenseVector[T]]
  def densevector_mul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload216): Rep[DenseVector[T]]
  def densevector_dot_densevectorview[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densevector_mulmul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload11): Rep[DenseMatrix[T]]
  def densevector_div[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload186): Rep[DenseVector[T]]
  def densevector_pl[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload212): Rep[DenseVector[T]]
  def densevector_sub[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload196): Rep[DenseVector[T]]
  def densevector_mul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload217): Rep[SparseVector[T]]
  def densevector_mulclnmul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload31): Rep[T]
  def densevector_mulmul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload12): Rep[DenseMatrix[T]]
  def densevector_div[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload187): Rep[DenseVector[T]]
  def densevector_pl[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload213): Rep[DenseVector[T]]
  def densevector_sub[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload197): Rep[DenseVector[T]]
  def densevector_mul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload218): Rep[SparseVector[T]]
  def densevector_mulclnmul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload32): Rep[T]
  def densevector_mulmul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload13): Rep[DenseMatrix[T]]
  def densevector_div[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload188): Rep[DenseVector[T]]
  def densevector_zip[T:Typ,B:Typ,R:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVector[B]],__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload7): Rep[DenseVector[R]]
  def densevector_zip[T:Typ,B:Typ,R:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[B]],__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload8): Rep[DenseVector[R]]
  def densevector_pl[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload214): Rep[DenseVector[T]]
  def densevector_sub[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload198): Rep[DenseVector[T]]
  def densevector_mul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload219): Rep[DenseVector[T]]
  def densevector_mul[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload220): Rep[DenseVector[T]]
  def densevector_div[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload189): Rep[DenseVector[T]]
  def densevector_abs[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densevector_exp[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densevector_log[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densevector_sum[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densevector_prod[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densevector_mean[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double]): Rep[Double]
  def densevector_min[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def densevector_max[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def densevector_minindex[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[Int]
  def densevector_maxindex[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[Int]
  def densevector_map[T:Typ,R:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densevector_reduce[T:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def densevector_foreach[T:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_forall[T:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Boolean]
  def densevector_find[T:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def densevector_count[T:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Int]
  def densevector_partition[T:Typ](self: Rep[DenseVector[T]],pred: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Tup2[DenseVector[T],DenseVector[T]]]
  def densevector_flatmap[T:Typ,R:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[DenseVector[R]])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densevector_scanleft[T:Typ,R:Typ](self: Rep[DenseVector[T]],zero: Rep[R],__arg2: (Rep[R],Rep[T]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densevector_scanright[T:Typ,R:Typ](self: Rep[DenseVector[T]],zero: Rep[R],__arg2: (Rep[T],Rep[R]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densevector_prefixsum[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]]
  def densevector_toarray[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[ForgeArray[T]]
}
trait DenseVectorCompilerOps extends DenseVectorOps {
  this: OptiML => 

  def densevector_alloc_raw[T:Typ](__arg0: Rep[Int],__arg1: Rep[Boolean],__arg2: Rep[ForgeArray[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def densevector_precumulate[T:Typ](v: Rep[DenseVector[T]],identity: Rep[T],func: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext): Rep[Tup2[T,DenseVector[T]]]
  def densevector_dc_alloc[R:Typ,CR:Typ](__arg0: Rep[CR],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def densevector_sortindex_helper[T:Typ](__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[scala.Array[Int]]
  def densevector_groupby_helper[T:Typ,K:Typ,V:Typ](__arg0: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[K],__arg2: (Rep[T]) => Rep[V])(implicit __pos: SourceContext): Rep[ForgeHashMap[K,ForgeArrayBuffer[V]]]
  def densevector_raw_data[T:Typ](self: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[ForgeArray[T]]
  def densevector_set_raw_data[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[ForgeArray[T]])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_set_length[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_set_isrow[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Boolean])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_insertspace[T:Typ](self: Rep[DenseVector[T]],pos: Rep[Int],len: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_ensureextra[T:Typ](self: Rep[DenseVector[T]],extra: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_realloc[T:Typ](self: Rep[DenseVector[T]],minLen: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_appendable[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __pos: SourceContext): Rep[Boolean]
  def densevector_append[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __pos: SourceContext): Rep[Unit]
  def densevector_copy[T:Typ](self: Rep[DenseVector[T]],__arg1: Rep[Int],__arg2: Rep[DenseVector[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def reduce_and(__arg0: Rep[DenseVector[Boolean]])(implicit __pos: SourceContext): Rep[Boolean]
  def reduce_or(__arg0: Rep[DenseVector[Boolean]])(implicit __pos: SourceContext): Rep[Boolean]
  def densevector_densevector_filter_map[T:Typ,R:Typ](self: Rep[DenseVector[T]],__arg1: (Rep[T]) => Rep[Boolean],__arg2: (Rep[T]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
}

