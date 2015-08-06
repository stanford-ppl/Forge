package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait SparseVectorViewOpsBase extends Base {
  this: OptiML => 

  implicit def viewToSparse[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[SparseVector[T]] = sparsevectorview_viewtosparse[T](self)(implicitly[Typ[T]],__pos)
  implicit def chainViewToSparseOps[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): SparseVectorSparseVectorOpsCls[T] = sparsevectorview_chainviewtosparseops[T](self)(implicitly[Typ[T]],__pos)

  def sparsevectorview_viewtosparse[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevectorview_chainviewtosparseops[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): SparseVectorSparseVectorOpsCls[T]
}

trait SparseVectorViewOps extends SparseVectorViewOpsBase {
  this: OptiML => 

  object SparseVectorView {
    def apply[T:Typ](__arg0: Rep[SparseMatrix[T]],__arg1: Rep[Long],__arg2: Rep[Int],__arg3: Rep[Int],__arg4: Rep[Boolean])(implicit __pos: SourceContext,__imp1: Overload19) = sparsevectorview_object_apply[T](__arg0,__arg1,__arg2,__arg3,__arg4)(implicitly[Typ[T]],__pos)
  }

  def __equal[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload6) = sparsevectorview___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload6)
  def __equal[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload7) = sparsevectorview___equal[T](self,__arg1)(implicitly[Typ[T]],__pos,overload7)

  implicit def repToSparseVectorViewSparseVectorViewOpsCls[T:Typ](x: Rep[SparseVectorView[T]])(implicit __pos: SourceContext) = new SparseVectorViewSparseVectorViewOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToSparseVectorViewSparseVectorViewOpsCls[T:Typ](x: Var[SparseVectorView[T]])(implicit __pos: SourceContext) = new SparseVectorViewSparseVectorViewOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class SparseVectorViewSparseVectorViewOpsCls[T:Typ](val self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext) {
    def length(implicit __pos: SourceContext,__imp1: Overload3) = sparsevectorview_length[T](self)(implicitly[Typ[T]],__pos)
    def isRow(implicit __pos: SourceContext,__imp1: Overload2) = sparsevectorview_isrow[T](self)(implicitly[Typ[T]],__pos)
    def apply(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload21) = sparsevectorview_apply[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def nnz(implicit __pos: SourceContext,__imp1: Overload1) = sparsevectorview_nnz[T](self)(implicitly[Typ[T]],__pos)
    def nz(implicit __pos: SourceContext,__imp1: Overload1) = sparsevectorview_nz[T](self)(implicitly[Typ[T]],__pos)
    def indices(implicit __pos: SourceContext,__imp1: Overload4) = sparsevectorview_indices[T](self)(implicitly[Typ[T]],__pos)
    def Clone(implicit __pos: SourceContext,__imp1: ROverload4) = { self.toSparse }
    def toDense(implicit __pos: SourceContext,__imp1: ROverload3) = ??? //TR { self.toSparse.toDense }
    def toSparse(implicit __pos: SourceContext,__imp1: Overload2) = sparsevectorview_tosparse[T](self)(implicitly[Typ[T]],__pos)
    def toString(implicit __imp0: Overload6) = sparsevectorview_tostring[T](self)
    def *(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload189) = sparsevectorview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload189)
    def *:*(__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload18) = sparsevectorview_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload18)
    def *(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload190) = sparsevectorview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload190)
    def *:*(__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload19) = sparsevectorview_mulclnmul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload19)
    def *(__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload191) = sparsevectorview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload191)
    def *(__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload192) = sparsevectorview_mul[T](self,__arg1)(implicitly[Typ[T]],__pos,__imp0,overload192)
  }



  def sparsevectorview_object_apply[T:Typ](__arg0: Rep[SparseMatrix[T]],__arg1: Rep[Long],__arg2: Rep[Int],__arg3: Rep[Int],__arg4: Rep[Boolean])(implicit __pos: SourceContext): Rep[SparseVectorView[T]]
  def sparsevectorview_length[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsevectorview_isrow[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[Boolean]
  def sparsevectorview_apply[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[T]
  def sparsevectorview_nnz[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsevectorview_nz[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[DenseVectorView[T]]
  def sparsevectorview_indices[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[IndexVector]
  def sparsevectorview_tosparse[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[SparseVector[T]]
  def sparsevectorview___equal[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[SparseVectorView[T]])(implicit __pos: SourceContext,__imp1: Overload6): Rep[Boolean]
  def sparsevectorview___equal[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload7): Rep[Boolean]
  def sparsevectorview_tostring[T:Typ](self: Rep[SparseVectorView[T]]): Rep[String]
  def sparsevectorview_mul[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload189): Rep[SparseVector[T]]
  def sparsevectorview_mulclnmul[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload18): Rep[T]
  def sparsevectorview_mul[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload190): Rep[SparseVector[T]]
  def sparsevectorview_mulclnmul[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload19): Rep[T]
  def sparsevectorview_mul[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[T])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload191): Rep[SparseVector[T]]
  def sparsevectorview_mul[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload192): Rep[DenseVector[T]]
}
trait SparseVectorViewCompilerOps extends SparseVectorViewOps {
  this: OptiML => 

  def sparsevectorview_source[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def sparsevectorview_start[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[Long]
  def sparsevectorview_stride[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsevectorview_calc_offsets_all[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[Tup6[Int,Int,Int,Int,Int,Int]]
  def sparsevectorview_calc_offsets[T:Typ](self: Rep[SparseVectorView[T]])(implicit __pos: SourceContext): Rep[Tup2[Int,Int]]
  def sparsevectorview_includeoffset[T:Typ](self: Rep[SparseVectorView[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Boolean]
}

