package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait IndexVectorOpsBase extends Base {
  this: OptiML => 

  implicit def indexToDense(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseVector[Int]] = indexvector_indextodense(self)(__pos)
  implicit def chainIndexToDenseOps(self: Rep[IndexVector])(implicit __pos: SourceContext): DenseVectorDenseVectorOpsCls[Int] = indexvector_chainindextodenseops(self)(__pos)
  implicit def chainIndexToDenseIntOps(self: Rep[IndexVector])(implicit __pos: SourceContext): DenseVectorDenseVectorIntOpsCls = indexvector_chainindextodenseintops(self)(__pos)

  def indexvector_indextodense(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
  def indexvector_chainindextodenseops(self: Rep[IndexVector])(implicit __pos: SourceContext): DenseVectorDenseVectorOpsCls[Int]
  def indexvector_chainindextodenseintops(self: Rep[IndexVector])(implicit __pos: SourceContext): DenseVectorDenseVectorIntOpsCls
}

trait IndexVectorOps extends IndexVectorOpsBase {
  this: OptiML => 

  object IndexVector {
    def apply(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload26): Rep[IndexVector] = { IndexVector(__arg0,__arg1,unit(true)) }
    def apply(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload27) = indexvector_object_apply(__arg0,__arg1,__arg2)(__pos,overload27)
    def apply(__arg0: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: ROverload28): Rep[IndexVector] = { IndexVector(__arg0,__arg0.isRow) }
    def apply(__arg0: Rep[DenseVector[Int]],__arg1: Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload29) = indexvector_object_apply(__arg0,__arg1)(__pos,overload29)
    def apply(__arg0: Rep[ForgeArray[Int]],isRow: Rep[Boolean] = unit(true))(implicit __pos: SourceContext,__imp1: ROverload30): Rep[IndexVector] = { indexvector_fromarray(__arg0,isRow) }
  }

  def indexvector_fromarray(__arg0: Rep[ForgeArray[Int]],__arg1: Rep[Boolean])(implicit __pos: SourceContext) = indexvector_indexvector_fromarray(__arg0,__arg1)(__pos)
  def flatten(inds: Tuple2[Rep[Int],Rep[Int]],dims: Tuple2[Rep[Int],Rep[Int]])(implicit __pos: SourceContext,__imp1: ROverload1): Rep[Int] = { inds._1*dims._2*1 + inds._2*1 }
  def unflatten(i: Rep[Int],dims: Tuple2[Rep[Int],Rep[Int]])(implicit __pos: SourceContext,__imp1: ROverload1): Tuple2[Rep[Int],Rep[Int]] = { ((i / (dims._2*1)) % dims._1,(i / (1)) % dims._2) }
  def flatten(inds: Tuple3[Rep[Int],Rep[Int],Rep[Int]],dims: Tuple3[Rep[Int],Rep[Int],Rep[Int]])(implicit __pos: SourceContext,__imp1: ROverload2): Rep[Int] = { inds._1*dims._2*dims._3*1 + inds._2*dims._3*1 + inds._3*1 }
  def unflatten(i: Rep[Int],dims: Tuple3[Rep[Int],Rep[Int],Rep[Int]])(implicit __pos: SourceContext,__imp1: ROverload2): Tuple3[Rep[Int],Rep[Int],Rep[Int]] = { ((i / (dims._2*dims._3*1)) % dims._1,(i / (dims._3*1)) % dims._2,(i / (1)) % dims._3) }
  def flatten(inds: Tuple4[Rep[Int],Rep[Int],Rep[Int],Rep[Int]],dims: Tuple4[Rep[Int],Rep[Int],Rep[Int],Rep[Int]])(implicit __pos: SourceContext,__imp1: ROverload3): Rep[Int] = { inds._1*dims._2*dims._3*dims._4*1 + inds._2*dims._3*dims._4*1 + inds._3*dims._4*1 + inds._4*1 }
  def unflatten(i: Rep[Int],dims: Tuple4[Rep[Int],Rep[Int],Rep[Int],Rep[Int]])(implicit __pos: SourceContext,__imp1: ROverload3): Tuple4[Rep[Int],Rep[Int],Rep[Int],Rep[Int]] = { ((i / (dims._2*dims._3*dims._4*1)) % dims._1,(i / (dims._3*dims._4*1)) % dims._2,(i / (dims._4*1)) % dims._3,(i / (1)) % dims._4) }
  def flatten(inds: Tuple5[Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int]],dims: Tuple5[Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int]])(implicit __pos: SourceContext,__imp1: ROverload4): Rep[Int] = { inds._1*dims._2*dims._3*dims._4*dims._5*1 + inds._2*dims._3*dims._4*dims._5*1 + inds._3*dims._4*dims._5*1 + inds._4*dims._5*1 + inds._5*1 }
  def unflatten(i: Rep[Int],dims: Tuple5[Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int]])(implicit __pos: SourceContext,__imp1: ROverload4): Tuple5[Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int]] = { ((i / (dims._2*dims._3*dims._4*dims._5*1)) % dims._1,(i / (dims._3*dims._4*dims._5*1)) % dims._2,(i / (dims._4*dims._5*1)) % dims._3,(i / (dims._5*1)) % dims._4,(i / (1)) % dims._5) }
  def flatten(inds: Tuple6[Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int]],dims: Tuple6[Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int]])(implicit __pos: SourceContext,__imp1: ROverload5): Rep[Int] = { inds._1*dims._2*dims._3*dims._4*dims._5*dims._6*1 + inds._2*dims._3*dims._4*dims._5*dims._6*1 + inds._3*dims._4*dims._5*dims._6*1 + inds._4*dims._5*dims._6*1 + inds._5*dims._6*1 + inds._6*1 }
  def unflatten(i: Rep[Int],dims: Tuple6[Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int]])(implicit __pos: SourceContext,__imp1: ROverload5): Tuple6[Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int],Rep[Int]] = { ((i / (dims._2*dims._3*dims._4*dims._5*dims._6*1)) % dims._1,(i / (dims._3*dims._4*dims._5*dims._6*1)) % dims._2,(i / (dims._4*dims._5*dims._6*1)) % dims._3,(i / (dims._5*dims._6*1)) % dims._4,(i / (dims._6*1)) % dims._5,(i / (1)) % dims._6) }
  def __equal(self: Rep[IndexVector],__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload21) = indexvector___equal(self,__arg1)(__pos,overload21)
  def __equal(self: Rep[IndexVector],__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload22) = indexvector___equal(self,__arg1)(__pos,overload22)

  implicit def repToIndexVectorIndexVectorOpsCls(x: Rep[IndexVector])(implicit __pos: SourceContext) = new IndexVectorIndexVectorOpsCls(x)(__pos)
  implicit def varToIndexVectorIndexVectorOpsCls(x: Var[IndexVector])(implicit __pos: SourceContext) = new IndexVectorIndexVectorOpsCls(readVar(x))(__pos)

  class IndexVectorIndexVectorOpsCls(val self: Rep[IndexVector])(implicit __pos: SourceContext) {
    def length(implicit __pos: SourceContext,__imp1: Overload5) = indexvector_length(self)(__pos)
    def isRow(implicit __pos: SourceContext,__imp1: Overload4) = indexvector_isrow(self)(__pos)
    def apply(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload35) = indexvector_apply(self,__arg1)(__pos,overload35)
    def apply(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload36) = indexvector_apply(self,__arg1)(__pos,overload36)
    def slice(start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload7) = indexvector_slice(self,start,end)(__pos)
    def t(implicit __pos: SourceContext,__imp1: Overload5) = indexvector_t(self)(__pos)
    def Clone(implicit __pos: SourceContext,__imp1: Overload7) = indexvector_clone(self)(__pos)
    def toDense(implicit __pos: SourceContext,__imp1: Overload6) = indexvector_todense(self)(__pos)
    def filter(__arg1: (Rep[Int]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload2) = indexvector_filter(self,__arg1)(__pos)
    def toBoolean(implicit __pos: SourceContext,conv: (Rep[Int]) => Rep[Boolean],__imp2: Overload5) = indexvector_toboolean(self)(__pos,conv)
    def toDouble(implicit __pos: SourceContext,conv: (Rep[Int]) => Rep[Double],__imp2: Overload6) = indexvector_todouble(self)(__pos,conv)
    def toFloat(implicit __pos: SourceContext,conv: (Rep[Int]) => Rep[Float],__imp2: Overload6) = indexvector_tofloat(self)(__pos,conv)
    def toInt(implicit __pos: SourceContext,conv: (Rep[Int]) => Rep[Int],__imp2: Overload6) = indexvector_toint(self)(__pos,conv)
    def indices(implicit __pos: SourceContext,__imp1: Overload6) = indexvector_indices(self)(__pos)
    def isEmpty(implicit __pos: SourceContext,__imp1: Overload3) = indexvector_isempty(self)(__pos)
    def first(implicit __pos: SourceContext,__imp1: Overload3) = indexvector_first(self)(__pos)
    def last(implicit __pos: SourceContext,__imp1: Overload3) = indexvector_last(self)(__pos)
    def drop(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3) = indexvector_drop(self,__arg1)(__pos)
    def take(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload3) = indexvector_take(self,__arg1)(__pos)
    def contains(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload10) = indexvector_contains(self,__arg1)(__pos)
    def histogram(implicit __pos: SourceContext,__imp1: Overload3) = indexvector_histogram(self)(__pos)
    def distinct(implicit __pos: SourceContext,__imp1: Overload3) = indexvector_distinct(self)(__pos)
    def mutable()(implicit __pos: SourceContext,__imp1: Overload13) = indexvector_mutable(self)(__pos)
    def replicate(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4) = indexvector_replicate(self,__arg1,__arg2)(__pos)
    def makeString(implicit __pos: SourceContext,__imp1: Overload6) = indexvector_makestring(self)(__pos)
    def toString(implicit __imp0: Overload11) = indexvector_tostring(self)
    def pprint()(implicit __pos: SourceContext,__imp1: Overload7) = indexvector_pprint(self)(__pos)
    def makeStrWithDelim(delim: Rep[String])(implicit __pos: SourceContext,__imp1: Overload2) = indexvector_makestrwithdelim(self,delim)(__pos)
    def +(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload201) = indexvector_pl(self,__arg1)(__pos,overload201)
    def -(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload185) = indexvector_sub(self,__arg1)(__pos,overload185)
    def *(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload205) = indexvector_mul(self,__arg1)(__pos,overload205)
    def *:*(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload25) = indexvector_mulclnmul(self,__arg1)(__pos,overload25)
    def **(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload6) = indexvector_mulmul(self,__arg1)(__pos,overload6)
    def /(__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload176) = indexvector_div(self,__arg1)(__pos,overload176)
    def +(__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload202) = indexvector_pl(self,__arg1)(__pos,overload202)
    def -(__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload186) = indexvector_sub(self,__arg1)(__pos,overload186)
    def *(__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload206) = indexvector_mul(self,__arg1)(__pos,overload206)
    def *:*(__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload26) = indexvector_mulclnmul(self,__arg1)(__pos,overload26)
    def **(__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload7) = indexvector_mulmul(self,__arg1)(__pos,overload7)
    def /(__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload177) = indexvector_div(self,__arg1)(__pos,overload177)
    def +(__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload203) = indexvector_pl(self,__arg1)(__pos,overload203)
    def -(__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload187) = indexvector_sub(self,__arg1)(__pos,overload187)
    def *(__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload207) = indexvector_mul(self,__arg1)(__pos,overload207)
    def *:*(__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload27) = indexvector_mulclnmul(self,__arg1)(__pos,overload27)
    def **(__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload8) = indexvector_mulmul(self,__arg1)(__pos,overload8)
    def /(__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload178) = indexvector_div(self,__arg1)(__pos,overload178)
    def +(__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload204) = indexvector_pl(self,__arg1)(__pos,overload204)
    def -(__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload188) = indexvector_sub(self,__arg1)(__pos,overload188)
    def *(__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload208) = indexvector_mul(self,__arg1)(__pos,overload208)
    def *:*(__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload28) = indexvector_mulclnmul(self,__arg1)(__pos,overload28)
    def **(__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload9) = indexvector_mulmul(self,__arg1)(__pos,overload9)
    def /(__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload179) = indexvector_div(self,__arg1)(__pos,overload179)
    def zip[B:Typ,R:Typ](__arg1: Rep[DenseVector[B]])(__arg2: (Rep[Int],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload5) = indexvector_zip[B,R](self,__arg1,__arg2)(implicitly[Typ[B]],implicitly[Typ[R]],__pos,overload5)
    def zip[B:Typ,R:Typ](__arg1: Rep[DenseVectorView[B]])(__arg2: (Rep[Int],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload6) = indexvector_zip[B,R](self,__arg1,__arg2)(implicitly[Typ[B]],implicitly[Typ[R]],__pos,overload6)
    def +(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload205) = indexvector_pl(self,__arg1)(__pos,overload205)
    def -(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload189) = indexvector_sub(self,__arg1)(__pos,overload189)
    def *(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload209) = indexvector_mul(self,__arg1)(__pos,overload209)
    def *(__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: Overload210) = indexvector_mul(self,__arg1)(__pos,overload210)
    def /(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload180) = indexvector_div(self,__arg1)(__pos,overload180)
    def abs(implicit __pos: SourceContext,__imp1: Overload18) = indexvector_abs(self)(__pos)
    def exp(implicit __pos: SourceContext,__imp1: Overload16) = indexvector_exp(self)(__pos)
    def log(implicit __pos: SourceContext,__imp1: Overload16) = indexvector_log(self)(__pos)
    def sum(implicit __pos: SourceContext,__imp1: Overload6) = indexvector_sum(self)(__pos)
    def prod(implicit __pos: SourceContext,__imp1: Overload4) = indexvector_prod(self)(__pos)
    def mean(implicit __pos: SourceContext,conv: (Rep[Int]) => Rep[Double],__imp2: Overload6) = indexvector_mean(self)(__pos,conv)
    def min(implicit __pos: SourceContext,__imp1: Overload12) = indexvector_min(self)(__pos)
    def max(implicit __pos: SourceContext,__imp1: Overload12) = indexvector_max(self)(__pos)
    def minIndex(implicit __pos: SourceContext,__imp1: Overload4) = indexvector_minindex(self)(__pos)
    def maxIndex(implicit __pos: SourceContext,__imp1: Overload4) = indexvector_maxindex(self)(__pos)
    def map[R:Typ](__arg1: (Rep[Int]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload5) = indexvector_map[R](self,__arg1)(implicitly[Typ[R]],__pos)
    def reduce(__arg1: (Rep[Int],Rep[Int]) => Rep[Int])(implicit __pos: SourceContext,__imp1: Overload5) = indexvector_reduce(self,__arg1)(__pos)
    def foreach(__arg1: (Rep[Int]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload6) = indexvector_foreach(self,__arg1)(__pos)
    def forall(__arg1: (Rep[Int]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload2) = indexvector_forall(self,__arg1)(__pos)
    def find(__arg1: (Rep[Int]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload2) = indexvector_find(self,__arg1)(__pos)
    def count(__arg1: (Rep[Int]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload4) = indexvector_count(self,__arg1)(__pos)
    def partition(pred: (Rep[Int]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload2) = indexvector_partition(self,pred)(__pos)
    def flatMap[R:Typ](__arg1: (Rep[Int]) => Rep[DenseVector[R]])(implicit __pos: SourceContext,__imp1: Overload2) = indexvector_flatmap[R](self,__arg1)(implicitly[Typ[R]],__pos)
    def scanLeft[R:Typ](zero: Rep[R])(__arg2: (Rep[R],Rep[Int]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload2) = indexvector_scanleft[R](self,zero,__arg2)(implicitly[Typ[R]],__pos)
    def scanRight[R:Typ](zero: Rep[R])(__arg2: (Rep[Int],Rep[R]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload2) = indexvector_scanright[R](self,zero,__arg2)(implicitly[Typ[R]],__pos)
    def prefixSum(implicit __pos: SourceContext,__imp1: Overload2) = indexvector_prefixsum(self)(__pos)
    def toArray(implicit __pos: SourceContext,__imp1: Overload3) = indexvector_toarray(self)(__pos)
    def apply[T:Typ](__arg1: (Rep[Int]) => Rep[T])(implicit __pos: SourceContext,__imp1: Overload37) = indexvector_apply[T](self,__arg1)(implicitly[Typ[T]],__pos,overload37)
  }

  implicit def repToIndexVectorIntOpsCls(x: Rep[Int])(implicit __pos: SourceContext) = new IndexVectorIntOpsCls(x)(__pos)
  implicit def varToIndexVectorIntOpsCls(x: Var[Int])(implicit __pos: SourceContext) = new IndexVectorIntOpsCls(readVar(x))(__pos)

  class IndexVectorIntOpsCls(val self: Rep[Int])(implicit __pos: SourceContext) {
    def ::(start: Rep[Int]) = indexvector_clncln(self,start)
  }

  implicit def liftToIndexVectorTuple2IndexVectorIndexVectorOpsCls(x: Tuple2[Rep[IndexVector],Rep[IndexVector]])(implicit __pos: SourceContext) = new IndexVectorTuple2IndexVectorIndexVectorOpsCls(x)(__pos)

  class IndexVectorTuple2IndexVectorIndexVectorOpsCls(val self: Tuple2[Rep[IndexVector],Rep[IndexVector]])(implicit __pos: SourceContext) {
    def apply[T:Typ](__arg1: (Rep[Int],Rep[Int]) => Rep[T])(implicit __pos: SourceContext,__imp1: Overload38) = indexvector_apply[T](self,__arg1)(implicitly[Typ[T]],__pos,overload38)
  }

  implicit def liftToIndexVectorTuple2IndexVectorIndexWildcardOpsCls(x: Tuple2[Rep[IndexVector],IndexWildcard])(implicit __pos: SourceContext) = new IndexVectorTuple2IndexVectorIndexWildcardOpsCls(x)(__pos)

  class IndexVectorTuple2IndexVectorIndexWildcardOpsCls(val self: Tuple2[Rep[IndexVector],IndexWildcard])(implicit __pos: SourceContext) {
    def apply[T:Typ](__arg1: (Rep[Int]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload39) = indexvector_apply[T](self,__arg1)(implicitly[Typ[T]],__pos,overload39)
  }

  implicit def liftToIndexVectorTuple2IndexWildcardIndexVectorOpsCls(x: Tuple2[IndexWildcard,Rep[IndexVector]])(implicit __pos: SourceContext) = new IndexVectorTuple2IndexWildcardIndexVectorOpsCls(x)(__pos)

  class IndexVectorTuple2IndexWildcardIndexVectorOpsCls(val self: Tuple2[IndexWildcard,Rep[IndexVector]])(implicit __pos: SourceContext) {
    def apply[T:Typ](__arg1: (Rep[Int]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload40) = indexvector_apply[T](self,__arg1)(implicitly[Typ[T]],__pos,overload40)
  }



  def indexvector_object_apply(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload27): Rep[IndexVector]
  def indexvector_object_apply(__arg0: Rep[DenseVector[Int]],__arg1: Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload29): Rep[IndexVector]
  def indexvector_indexvector_fromarray(__arg0: Rep[ForgeArray[Int]],__arg1: Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def indexvector_length(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_isrow(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Boolean]
  def indexvector_apply(self: Rep[IndexVector],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload35): Rep[Int]
  def indexvector_apply(self: Rep[IndexVector],__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload36): Rep[IndexVector]
  def indexvector_slice(self: Rep[IndexVector],start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext): Rep[IndexVector]
  def indexvector_t(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[IndexVector]
  def indexvector_clone(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[IndexVector]
  def indexvector_todense(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
  def indexvector___equal(self: Rep[IndexVector],__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload21): Rep[Boolean]
  def indexvector___equal(self: Rep[IndexVector],__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload22): Rep[Boolean]
  def indexvector_filter(self: Rep[IndexVector],__arg1: (Rep[Int]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def indexvector_toboolean(self: Rep[IndexVector])(implicit __pos: SourceContext,conv: (Rep[Int]) => Rep[Boolean]): Rep[DenseVector[Boolean]]
  def indexvector_todouble(self: Rep[IndexVector])(implicit __pos: SourceContext,conv: (Rep[Int]) => Rep[Double]): Rep[DenseVector[Double]]
  def indexvector_tofloat(self: Rep[IndexVector])(implicit __pos: SourceContext,conv: (Rep[Int]) => Rep[Float]): Rep[DenseVector[Float]]
  def indexvector_toint(self: Rep[IndexVector])(implicit __pos: SourceContext,conv: (Rep[Int]) => Rep[Int]): Rep[DenseVector[Int]]
  def indexvector_indices(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[IndexVector]
  def indexvector_isempty(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Boolean]
  def indexvector_first(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_last(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_drop(self: Rep[IndexVector],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[IndexVector]
  def indexvector_take(self: Rep[IndexVector],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[IndexVector]
  def indexvector_contains(self: Rep[IndexVector],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Boolean]
  def indexvector_histogram(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[ForgeHashMap[Int,Int]]
  def indexvector_distinct(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
  def indexvector_mutable(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
  def indexvector_replicate(self: Rep[IndexVector],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[Int]]
  def indexvector_makestring(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[String]
  def indexvector_tostring(self: Rep[IndexVector]): Rep[String]
  def indexvector_pprint(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Unit]
  def indexvector_makestrwithdelim(self: Rep[IndexVector],delim: Rep[String])(implicit __pos: SourceContext): Rep[String]
  def indexvector_pl(self: Rep[IndexVector],__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload201): Rep[DenseVector[Int]]
  def indexvector_sub(self: Rep[IndexVector],__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload185): Rep[DenseVector[Int]]
  def indexvector_mul(self: Rep[IndexVector],__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload205): Rep[DenseVector[Int]]
  def indexvector_mulclnmul(self: Rep[IndexVector],__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload25): Rep[Int]
  def indexvector_mulmul(self: Rep[IndexVector],__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload6): Rep[DenseMatrix[Int]]
  def indexvector_div(self: Rep[IndexVector],__arg1: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload176): Rep[DenseVector[Int]]
  def indexvector_pl(self: Rep[IndexVector],__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload202): Rep[DenseVector[Int]]
  def indexvector_sub(self: Rep[IndexVector],__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload186): Rep[DenseVector[Int]]
  def indexvector_mul(self: Rep[IndexVector],__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload206): Rep[DenseVector[Int]]
  def indexvector_mulclnmul(self: Rep[IndexVector],__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload26): Rep[Int]
  def indexvector_mulmul(self: Rep[IndexVector],__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload7): Rep[DenseMatrix[Int]]
  def indexvector_div(self: Rep[IndexVector],__arg1: Rep[DenseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload177): Rep[DenseVector[Int]]
  def indexvector_pl(self: Rep[IndexVector],__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload203): Rep[DenseVector[Int]]
  def indexvector_sub(self: Rep[IndexVector],__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload187): Rep[DenseVector[Int]]
  def indexvector_mul(self: Rep[IndexVector],__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload207): Rep[SparseVector[Int]]
  def indexvector_mulclnmul(self: Rep[IndexVector],__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload27): Rep[Int]
  def indexvector_mulmul(self: Rep[IndexVector],__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload8): Rep[DenseMatrix[Int]]
  def indexvector_div(self: Rep[IndexVector],__arg1: Rep[SparseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload178): Rep[DenseVector[Int]]
  def indexvector_pl(self: Rep[IndexVector],__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload204): Rep[DenseVector[Int]]
  def indexvector_sub(self: Rep[IndexVector],__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload188): Rep[DenseVector[Int]]
  def indexvector_mul(self: Rep[IndexVector],__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload208): Rep[SparseVector[Int]]
  def indexvector_mulclnmul(self: Rep[IndexVector],__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload28): Rep[Int]
  def indexvector_mulmul(self: Rep[IndexVector],__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload9): Rep[DenseMatrix[Int]]
  def indexvector_div(self: Rep[IndexVector],__arg1: Rep[SparseVectorView[Int]])(implicit __pos: SourceContext,__imp1: Overload179): Rep[DenseVector[Int]]
  def indexvector_zip[B:Typ,R:Typ](self: Rep[IndexVector],__arg1: Rep[DenseVector[B]],__arg2: (Rep[Int],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload5): Rep[DenseVector[R]]
  def indexvector_zip[B:Typ,R:Typ](self: Rep[IndexVector],__arg1: Rep[DenseVectorView[B]],__arg2: (Rep[Int],Rep[B]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload6): Rep[DenseVector[R]]
  def indexvector_pl(self: Rep[IndexVector],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload205): Rep[DenseVector[Int]]
  def indexvector_sub(self: Rep[IndexVector],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload189): Rep[DenseVector[Int]]
  def indexvector_mul(self: Rep[IndexVector],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload209): Rep[DenseVector[Int]]
  def indexvector_mul(self: Rep[IndexVector],__arg1: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext,__imp1: Overload210): Rep[DenseVector[Int]]
  def indexvector_div(self: Rep[IndexVector],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload180): Rep[DenseVector[Int]]
  def indexvector_abs(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
  def indexvector_exp(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
  def indexvector_log(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
  def indexvector_sum(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_prod(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_mean(self: Rep[IndexVector])(implicit __pos: SourceContext,conv: (Rep[Int]) => Rep[Double]): Rep[Double]
  def indexvector_min(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_max(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_minindex(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_maxindex(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_map[R:Typ](self: Rep[IndexVector],__arg1: (Rep[Int]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def indexvector_reduce(self: Rep[IndexVector],__arg1: (Rep[Int],Rep[Int]) => Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_foreach(self: Rep[IndexVector],__arg1: (Rep[Int]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def indexvector_forall(self: Rep[IndexVector],__arg1: (Rep[Int]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Boolean]
  def indexvector_find(self: Rep[IndexVector],__arg1: (Rep[Int]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def indexvector_count(self: Rep[IndexVector],__arg1: (Rep[Int]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_partition(self: Rep[IndexVector],pred: (Rep[Int]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Tup2[IndexVector,IndexVector]]
  def indexvector_flatmap[R:Typ](self: Rep[IndexVector],__arg1: (Rep[Int]) => Rep[DenseVector[R]])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def indexvector_scanleft[R:Typ](self: Rep[IndexVector],zero: Rep[R],__arg2: (Rep[R],Rep[Int]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def indexvector_scanright[R:Typ](self: Rep[IndexVector],zero: Rep[R],__arg2: (Rep[Int],Rep[R]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
  def indexvector_prefixsum(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
  def indexvector_toarray(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def indexvector_clncln(end: Rep[Int],start: Rep[Int]): Rep[IndexVector]
  def indexvector_apply[T:Typ](__arg0: Rep[IndexVector],__arg1: (Rep[Int]) => Rep[T])(implicit __pos: SourceContext,__imp1: Overload37): Rep[DenseVector[T]]
  def indexvector_apply[T:Typ](__arg0: Tuple2[Rep[IndexVector],Rep[IndexVector]],__arg1: (Rep[Int],Rep[Int]) => Rep[T])(implicit __pos: SourceContext,__imp1: Overload38): Rep[DenseMatrix[T]]
  def indexvector_apply[T:Typ](__arg0: Tuple2[Rep[IndexVector],IndexWildcard],__arg1: (Rep[Int]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload39): Rep[DenseMatrix[T]]
  def indexvector_apply[T:Typ](__arg0: Tuple2[IndexWildcard,Rep[IndexVector]],__arg1: (Rep[Int]) => Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload40): Rep[DenseMatrix[T]]
}
trait IndexVectorCompilerOps extends IndexVectorOps {
  this: OptiML => 

  def indexvector_copyarray(__arg0: Rep[DenseVector[Int]])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def indexvector_start(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_end(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Int]
  def indexvector_raw_data(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def indexvector_is_range(self: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Boolean]
  def min_index_of[T:Typ](self: Rep[IndexVector],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[Int]
  def max_index_of[T:Typ](self: Rep[IndexVector],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T]): Rep[Int]
  def indexvector_illegalalloc(self: Rep[IndexVector],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Nothing]
  def indexvector_illegalupdate(self: Rep[IndexVector],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[Nothing]
  def zeroT()(implicit __pos: SourceContext): Rep[Int]
  def indexvector_densevector_filter_map[R:Typ](self: Rep[IndexVector],__arg1: (Rep[Int]) => Rep[Boolean],__arg2: (Rep[Int]) => Rep[R])(implicit __pos: SourceContext): Rep[DenseVector[R]]
}

