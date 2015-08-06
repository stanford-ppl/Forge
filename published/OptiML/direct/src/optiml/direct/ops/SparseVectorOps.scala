package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait SparseVectorOpsBase extends Base {
  this: OptiML => 

  implicit def dist(__arg0: Rep[SparseVector[Double]],__arg1: Rep[SparseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload5): Rep[Double] = { dist(__arg0,__arg1,ABS) }

}

trait SparseVectorOps extends SparseVectorOpsBase {
  this: OptiML => 

  object SparseVector {
    def apply[T:Typ](__arg0: Rep[Int],__arg1: Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload23) = sparsevector_object_apply[T](__arg0,__arg1)(implicitly[Typ[T]],__pos)
    def fromElements[T:Typ](length: Rep[Int],isRow: Rep[Boolean],nzIndices: Rep[IndexVector],nzElements: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1) = sparsevector_object_fromelements[T](length,isRow,nzIndices,nzElements)(implicitly[Typ[T]],__pos)
    def fromSortedElements[T:Typ](length: Rep[Int],isRow: Rep[Boolean],nzIndices: Rep[IndexVector],nzElements: Rep[DenseVector[T]])(implicit __pos: SourceContext) = sparsevector_object_fromsortedelements[T](length,isRow,nzIndices,nzElements)(implicitly[Typ[T]],__pos)
    def fromFunc[T:Typ](__arg0: Rep[Int],__arg1: Rep[Boolean],__arg2: Rep[IndexVector],__arg3: (Rep[Int]) => Rep[T])(implicit __pos: SourceContext) = sparsevector_object_fromfunc[T](__arg0,__arg1,__arg2,__arg3)(implicitly[Typ[T]],__pos)
    def zeros(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload2): Rep[SparseVector[Double]] = { SparseVector[Double](__arg0, unit(true)).unsafeImmutable }
    def zerosf(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload2): Rep[SparseVector[Float]] = { SparseVector[Float](__arg0, unit(true)).unsafeImmutable }
    def rand(length: Rep[Int],sparsity: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload2) = sparsevector_object_rand(length,sparsity)(__pos)
    def randf(length: Rep[Int],sparsity: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload2) = sparsevector_object_randf(length,sparsity)(__pos)
  }

  def dist(__arg0: Rep[SparseVector[Double]],__arg1: Rep[SparseVector[Double]],__arg2: DistanceMetric)(implicit __pos: SourceContext,__imp1: Overload3) = sparsevector_dist(__arg0,__arg1,__arg2)(__pos)
  def __equal[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload16) = sparsevector___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload16)
  def __equal[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload17) = sparsevector___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload17)
  def __equal[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload18) = sparsevector___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload18)

  implicit def repToSparseVectorSparseVectorOpsCls[T:Typ](x: Rep[SparseVector[T]])(implicit __pos: SourceContext) = new SparseVectorSparseVectorOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToSparseVectorSparseVectorOpsCls[T:Typ](x: Var[SparseVector[T]])(implicit __pos: SourceContext) = new SparseVectorSparseVectorOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class SparseVectorSparseVectorOpsCls[T:Typ](val self: Rep[SparseVector[T]])(implicit __pos: SourceContext) {
    def length(implicit __pos: SourceContext,__imp1: Overload4) = sparsevector_length[T](self)(implicitly[Typ[T]],__pos)
    def isRow(implicit __pos: SourceContext,__imp1: Overload3) = sparsevector_isrow[T](self)(implicitly[Typ[T]],__pos)
    def nnz(implicit __pos: SourceContext,__imp1: Overload2) = sparsevector_nnz[T](self)(implicitly[Typ[T]],__pos)
    def nz(implicit __pos: SourceContext,__imp1: Overload2) = sparsevector_nz[T](self)(implicitly[Typ[T]],__pos)
    def indices(implicit __pos: SourceContext,__imp1: Overload5) = sparsevector_indices[T](self)(implicitly[Typ[T]],__pos)
    def apply(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload26) = sparsevector_apply[T](self,__arg1)(implicitly[Typ[T]],__pos,overload26)
    def apply(__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload27) = sparsevector_apply[T](self,__arg1)(implicitly[Typ[T]],__pos,overload27)
    def isEmpty(implicit __pos: SourceContext,__imp1: Overload2) = sparsevector_isempty[T](self)(implicitly[Typ[T]],__pos)
    def first(implicit __pos: SourceContext,__imp1: Overload2) = sparsevector_first[T](self)(implicitly[Typ[T]],__pos)
    def firstnz(implicit __pos: SourceContext) = sparsevector_firstnz[T](self)(implicitly[Typ[T]],__pos)
    def last(implicit __pos: SourceContext,__imp1: Overload2) = sparsevector_last[T](self)(implicitly[Typ[T]],__pos)
    def lastnz(implicit __pos: SourceContext) = sparsevector_lastnz[T](self)(implicitly[Typ[T]],__pos)
    def drop(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = sparsevector_drop[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def take(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = sparsevector_take[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def slice(start: Rep[Int],end: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload5) = { self(start::end) }
    def contains(__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload9) = sparsevector_contains[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def distinct(implicit __pos: SourceContext,__imp1: Overload2) = sparsevector_distinct[T](self)(implicitly[Typ[T]],__pos)
    def t(implicit __pos: SourceContext,__imp1: Overload3) = sparsevector_t[T](self)(implicitly[Typ[T]],__pos)
    def mt()(implicit __pos: SourceContext,__imp1: Overload1) = sparsevector_mt[T](self)(implicitly[Typ[T]],__pos)
    def Clone(implicit __pos: SourceContext,__imp1: Overload5) = sparsevector_clone[T](self)(implicitly[Typ[T]],__pos)
    def mutable()(implicit __pos: SourceContext,__imp1: Overload11) = sparsevector_mutable[T](self)(implicitly[Typ[T]],__pos)
    def toDense(implicit __pos: SourceContext,__imp1: Overload4) = sparsevector_todense[T](self)(implicitly[Typ[T]],__pos)
    def makeString(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload4) = sparsevector_makestring[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def toString(implicit __imp0: Overload8) = sparsevector_tostring[T](self)
    def pprint()(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload5) = sparsevector_pprint[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def update(pos: Rep[Int],e: Rep[T])(implicit __pos: SourceContext,__imp1: Overload7) = sparsevector_update[T](self,pos,e)(implicitly[Typ[T]],__pos,overload7)
    def update(indices: Rep[IndexVector],e: Rep[T])(implicit __pos: SourceContext,__imp1: Overload8) = sparsevector_update[T](self,indices,e)(implicitly[Typ[T]],__pos,overload8)
    def update(indices: Rep[IndexVector],v: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload9) = sparsevector_update[T](self,indices,v)(implicitly[Typ[T]],__pos,overload9)
    def <<(__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload5) = sparsevector_ltlt[T](self,__arg1)(implicitly[Typ[T]],__pos,overload5)
    def <<(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload6) = sparsevector_ltlt[T](self,__arg1)(implicitly[Typ[T]],__pos,overload6)
    def <<=(__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload3) = sparsevector_ltlteq[T](self,__arg1)(implicitly[Typ[T]],__pos,overload3)
    def <<=(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload4) = sparsevector_ltlteq[T](self,__arg1)(implicitly[Typ[T]],__pos,overload4)
    def insert(__arg1: Rep[Int],__arg2: Rep[T])(implicit __pos: SourceContext,__imp1: Overload1) = sparsevector_insert[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)
    def insertAll(pos: Rep[Int],xs: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1) = sparsevector_insertall[T](self,pos,xs)(implicitly[Typ[T]],__pos)
    def remove(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = sparsevector_remove[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def removeAll(pos: Rep[Int],len: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = sparsevector_removeall[T](self,pos,len)(implicitly[Typ[T]],__pos)
    def copyFrom(pos: Rep[Int],xs: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload1) = sparsevector_copyfrom[T](self,pos,xs)(implicitly[Typ[T]],__pos)
    def trim()(implicit __pos: SourceContext,__imp1: Overload3) = sparsevector_trim[T](self)(implicitly[Typ[T]],__pos)
    def clear()(implicit __pos: SourceContext,__imp1: Overload1) = sparsevector_clear[T](self)(implicitly[Typ[T]],__pos)
    def +(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload193) = sparsevector_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload193)
    def +(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload194) = sparsevector_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload194)
    def +(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload195) = sparsevector_pl[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload195)
    def -(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload177) = sparsevector_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload177)
    def -(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload178) = sparsevector_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload178)
    def -(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload179) = sparsevector_sub[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload179)
    def *(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload195) = sparsevector_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload195)
    def *:*(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload20) = sparsevector_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload20)
    def **(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5) = sparsevector_mulmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def /(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload168) = sparsevector_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload168)
    def /(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload169) = sparsevector_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload169)
    def /(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload170) = sparsevector_div[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload170)
    def abs(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload14) = sparsevector_abs[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def sum(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload4) = sparsevector_sum[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def mean(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload4) = sparsevector_mean[T](self)(implicitly[Typ[T]],__pos,conv)
    def min(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload10) = sparsevector_min[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def max(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload10) = sparsevector_max[T](self)(implicitly[Typ[T]],__pos,__imp0,__imp1)
    def mapnz[R:Typ](__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext,__imp1: Overload1) = sparsevector_mapnz[T,R](self,__arg1)(implicitly[Typ[T]],implicitly[Typ[R]],__pos)
    def reducenz(__arg1: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T]) = sparsevector_reducenz[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0)
    def filternz(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext) = sparsevector_filternz[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def foreachnz(__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload1) = sparsevector_foreachnz[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def findnz(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext) = sparsevector_findnz[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def countnz(__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload1) = sparsevector_countnz[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def *(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload196) = sparsevector_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload196)
    def *:*(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload21) = sparsevector_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload21)
    def *(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload197) = sparsevector_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload197)
    def *:*(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload22) = sparsevector_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload22)
    def *(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload198) = sparsevector_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload198)
    def *(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload199) = sparsevector_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload199)
  }



  def sparsevector_dist(__arg0: Rep[SparseVector[Double]],__arg1: Rep[SparseVector[Double]],__arg2: DistanceMetric)(implicit __pos: SourceContext): Rep[Double]
  def sparsevector_object_apply[T:Typ](__arg0: Rep[Int],__arg1: Rep[Boolean])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevector_object_fromelements[T:Typ](length: Rep[Int],isRow: Rep[Boolean],nzIndices: Rep[IndexVector],nzElements: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevector_object_fromsortedelements[T:Typ](length: Rep[Int],isRow: Rep[Boolean],nzIndices: Rep[IndexVector],nzElements: Rep[DenseVector[T]])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevector_object_fromfunc[T:Typ](__arg0: Rep[Int],__arg1: Rep[Boolean],__arg2: Rep[IndexVector],__arg3: (Rep[Int]) => Rep[T])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevector_object_rand(length: Rep[Int],sparsity: Rep[Double])(implicit __pos: SourceContext): Rep[SparseVector[Double]]
  def sparsevector_object_randf(length: Rep[Int],sparsity: Rep[Double])(implicit __pos: SourceContext): Rep[SparseVector[Float]]
  def sparsevector_length[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsevector_isrow[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Boolean]
  def sparsevector_nnz[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsevector_nz[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def sparsevector_indices[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def sparsevector_apply[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload26): Rep[T]
  def sparsevector_apply[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload27): Rep[SparseVector[T]]
  def sparsevector_isempty[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Boolean]
  def sparsevector_first[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[T]
  def sparsevector_firstnz[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[T]
  def sparsevector_last[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[T]
  def sparsevector_lastnz[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[T]
  def sparsevector_drop[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevector_take[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevector_contains[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext): Rep[Boolean]
  def sparsevector_distinct[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def sparsevector_t[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevector_mt[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_clone[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevector_mutable[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevector_todense[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[DenseVector[T]]
  def sparsevector_makestring[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[String]
  def sparsevector_tostring[T:Typ](self: Rep[SparseVector[T]]): Rep[String]
  def sparsevector_pprint[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[Unit]
  def sparsevector_update[T:Typ](self: Rep[SparseVector[T]],pos: Rep[Int],e: Rep[T])(implicit __pos: SourceContext,__imp1: Overload7): Rep[Unit]
  def sparsevector_update[T:Typ](self: Rep[SparseVector[T]],indices: Rep[IndexVector],e: Rep[T])(implicit __pos: SourceContext,__imp1: Overload8): Rep[Unit]
  def sparsevector_update[T:Typ](self: Rep[SparseVector[T]],indices: Rep[IndexVector],v: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload9): Rep[Unit]
  def sparsevector_ltlt[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload5): Rep[SparseVector[T]]
  def sparsevector_ltlt[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload6): Rep[SparseVector[T]]
  def sparsevector_ltlteq[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload3): Rep[Unit]
  def sparsevector_ltlteq[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload4): Rep[Unit]
  def sparsevector_insert[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_insertall[T:Typ](self: Rep[SparseVector[T]],pos: Rep[Int],xs: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_remove[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_removeall[T:Typ](self: Rep[SparseVector[T]],pos: Rep[Int],len: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_copyfrom[T:Typ](self: Rep[SparseVector[T]],pos: Rep[Int],xs: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_trim[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_clear[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_pl[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload193): Rep[SparseVector[T]]
  def sparsevector_pl[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload194): Rep[DenseVector[T]]
  def sparsevector_pl[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload195): Rep[DenseVector[T]]
  def sparsevector_sub[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload177): Rep[SparseVector[T]]
  def sparsevector_sub[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload178): Rep[DenseVector[T]]
  def sparsevector_sub[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload179): Rep[DenseVector[T]]
  def sparsevector_mul[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload195): Rep[SparseVector[T]]
  def sparsevector_mulclnmul[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload20): Rep[T]
  def sparsevector_mulmul[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]]
  def sparsevector_div[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload168): Rep[SparseVector[T]]
  def sparsevector_div[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload169): Rep[DenseVector[T]]
  def sparsevector_div[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload170): Rep[SparseVector[T]]
  def sparsevector_abs[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[SparseVector[T]]
  def sparsevector_sum[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def sparsevector_mean[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double]): Rep[Double]
  def sparsevector_min[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def sparsevector_max[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T]): Rep[T]
  def sparsevector___equal[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp1: Overload16): Rep[Boolean]
  def sparsevector___equal[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload17): Rep[Boolean]
  def sparsevector___equal[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload18): Rep[Boolean]
  def sparsevector_mapnz[T:Typ,R:Typ](self: Rep[SparseVector[T]],__arg1: (Rep[T]) => Rep[R])(implicit __pos: SourceContext): Rep[SparseVector[R]]
  def sparsevector_reducenz[T:Typ](self: Rep[SparseVector[T]],__arg1: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[T]
  def sparsevector_filternz[T:Typ](self: Rep[SparseVector[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevector_foreachnz[T:Typ](self: Rep[SparseVector[T]],__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_findnz[T:Typ](self: Rep[SparseVector[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[IndexVector]
  def sparsevector_countnz[T:Typ](self: Rep[SparseVector[T]],__arg1: (Rep[T]) => Rep[Boolean])(implicit __pos: SourceContext): Rep[Int]
  def sparsevector_mul[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload196): Rep[SparseVector[T]]
  def sparsevector_mulclnmul[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload21): Rep[T]
  def sparsevector_mul[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload197): Rep[SparseVector[T]]
  def sparsevector_mulclnmul[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload22): Rep[T]
  def sparsevector_mul[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload198): Rep[SparseVector[T]]
  def sparsevector_mul[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload199): Rep[DenseVector[T]]
}
trait SparseVectorCompilerOps extends SparseVectorOps {
  this: OptiML => 

  def sparsevector_alloc_raw[T:Typ](__arg0: Rep[Int],__arg1: Rep[Boolean],__arg2: Rep[ForgeArray[T]],__arg3: Rep[ForgeArray[Int]],__arg4: Rep[Int])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def bsearch(a: Rep[ForgeArray[Int]],_start: Rep[Int],_end: Rep[Int],pos: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def defaultValue[T:Typ]()(implicit __pos: SourceContext): Rep[T]
  def zipUnion[A:Typ,B:Typ,R:Typ](nnzInit: Rep[Int],aIdxInit: Rep[Int],annz: Rep[Int],aIndices: Rep[ForgeArray[Int]],aData: Rep[ForgeArray[A]],bIdxInit: Rep[Int],bnnz: Rep[Int],bIndices: Rep[ForgeArray[Int]],bData: Rep[ForgeArray[B]],outIndices: Rep[ForgeArray[Int]],outData: Rep[ForgeArray[R]],f: (Rep[A],Rep[B]) => Rep[R])(implicit __pos: SourceContext): Rep[Int]
  def zipIntersect[A:Typ,B:Typ,R:Typ](nnzInit: Rep[Int],aIdxInit: Rep[Int],annz: Rep[Int],aIndices: Rep[ForgeArray[Int]],aData: Rep[ForgeArray[A]],bIdxInit: Rep[Int],bnnz: Rep[Int],bIndices: Rep[ForgeArray[Int]],bData: Rep[ForgeArray[B]],outIndices: Rep[ForgeArray[Int]],outData: Rep[ForgeArray[R]],f: (Rep[A],Rep[B]) => Rep[R])(implicit __pos: SourceContext): Rep[Int]
  def sparsevector_find_offset[T:Typ](self: Rep[SparseVector[T]],pos: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def sparsevector_raw_data[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[ForgeArray[T]]
  def sparsevector_raw_indices[T:Typ](self: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def sparsevector_set_length[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_set_isrow[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[Boolean])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_set_raw_data[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[ForgeArray[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_set_raw_indices[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[ForgeArray[Int]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_set_nnz[T:Typ](self: Rep[SparseVector[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_insert_at_off[T:Typ](self: Rep[SparseVector[T]],off: Rep[Int],pos: Rep[Int],e: Rep[T])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_insertspace[T:Typ](self: Rep[SparseVector[T]],pos: Rep[Int],len: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_ensureextra[T:Typ](self: Rep[SparseVector[T]],extra: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsevector_realloc[T:Typ](self: Rep[SparseVector[T]],minLen: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def zipVectorUnion[T:Typ,B:Typ,R:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVector[B]],__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext): Rep[SparseVector[R]]
  def zipVectorIntersect[T:Typ,B:Typ,R:Typ](self: Rep[SparseVector[T]],__arg1: Rep[SparseVector[B]],__arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __pos: SourceContext): Rep[SparseVector[R]]
}

