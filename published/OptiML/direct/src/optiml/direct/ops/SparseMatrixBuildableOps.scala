package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait SparseMatrixBuildableOps extends Base {
  this: OptiML => 

  implicit def repToSparseMatrixBuildableSparseMatrixBuildableOpsCls[T:Typ](x: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext) = new SparseMatrixBuildableSparseMatrixBuildableOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToSparseMatrixBuildableSparseMatrixBuildableOpsCls[T:Typ](x: Var[SparseMatrixBuildable[T]])(implicit __pos: SourceContext) = new SparseMatrixBuildableSparseMatrixBuildableOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class SparseMatrixBuildableSparseMatrixBuildableOpsCls[T:Typ](val self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext) {
    def numRows(implicit __pos: SourceContext,__imp1: Overload6) = sparsematrixbuildable_numrows[T](self)(implicitly[Typ[T]],__pos)
    def numCols(implicit __pos: SourceContext,__imp1: Overload6) = sparsematrixbuildable_numcols[T](self)(implicitly[Typ[T]],__pos)
    def nnz(implicit __pos: SourceContext,__imp1: Overload4) = sparsematrixbuildable_nnz[T](self)(implicitly[Typ[T]],__pos)
    def size(implicit __pos: SourceContext,__imp1: Overload15) = sparsematrixbuildable_size[T](self)(implicitly[Typ[T]],__pos)
    def apply(i: Rep[Int],j: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload41) = sparsematrixbuildable_apply[T](self,i,j)(implicitly[Typ[T]],__pos)
    def pprint()(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload8) = sparsematrixbuildable_pprint[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def makeString(implicit __pos: SourceContext,__imp0: Stringable[T],__imp2: Overload7) = sparsematrixbuildable_makestring[T](self)(implicitly[Typ[T]],__pos,__imp0)
    def toString(implicit __imp0: Overload12) = sparsematrixbuildable_tostring[T](self)
    def mutable()(implicit __pos: SourceContext,__imp1: Overload15) = sparsematrixbuildable_mutable[T](self)(implicitly[Typ[T]],__pos)
    def update(__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[T])(implicit __pos: SourceContext,__imp1: Overload10) = sparsematrixbuildable_update[T](self,__arg1,__arg2,__arg3)(implicitly[Typ[T]],__pos)
    def append(i: Rep[Int],j: Rep[Int],y: Rep[T],alwaysWrite: Rep[Boolean] = unit(true))(implicit __pos: SourceContext) = sparsematrixbuildable_append[T](self,i,j,y,alwaysWrite)(implicitly[Typ[T]],__pos)
    def <<=(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload5) = sparsematrixbuildable_ltlteq[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def <<|=(__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload3) = sparsematrixbuildable_ltltoreq[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def insertRow(pos: Rep[Int],y: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrixbuildable_insertrow[T](self,pos,y)(implicitly[Typ[T]],__pos)
    def insertCol(pos: Rep[Int],y: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrixbuildable_insertcol[T](self,pos,y)(implicitly[Typ[T]],__pos)
    def removeRow(pos: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrixbuildable_removerow[T](self,pos)(implicitly[Typ[T]],__pos)
    def removeCol(pos: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrixbuildable_removecol[T](self,pos)(implicitly[Typ[T]],__pos)
    def removeRows(pos: Rep[Int],num: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrixbuildable_removerows[T](self,pos,num)(implicitly[Typ[T]],__pos)
    def removeCols(pos: Rep[Int],num: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = sparsematrixbuildable_removecols[T](self,pos,num)(implicitly[Typ[T]],__pos)
    def finish(implicit __pos: SourceContext) = sparsematrixbuildable_finish[T](self)(implicitly[Typ[T]],__pos)
  }



  def sparsematrixbuildable_numrows[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsematrixbuildable_numcols[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsematrixbuildable_nnz[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsematrixbuildable_size[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[Int]
  def sparsematrixbuildable_apply[T:Typ](self: Rep[SparseMatrixBuildable[T]],i: Rep[Int],j: Rep[Int])(implicit __pos: SourceContext): Rep[T]
  def sparsematrixbuildable_pprint[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[Unit]
  def sparsematrixbuildable_makestring[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext,__imp0: Stringable[T]): Rep[String]
  def sparsematrixbuildable_tostring[T:Typ](self: Rep[SparseMatrixBuildable[T]]): Rep[String]
  def sparsematrixbuildable_mutable[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[SparseMatrixBuildable[T]]
  def sparsematrixbuildable_update[T:Typ](self: Rep[SparseMatrixBuildable[T]],__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[T])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrixbuildable_append[T:Typ](self: Rep[SparseMatrixBuildable[T]],i: Rep[Int],j: Rep[Int],y: Rep[T],alwaysWrite: Rep[Boolean] = unit(true))(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrixbuildable_ltlteq[T:Typ](self: Rep[SparseMatrixBuildable[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrixbuildable_ltltoreq[T:Typ](self: Rep[SparseMatrixBuildable[T]],__arg1: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrixbuildable_insertrow[T:Typ](self: Rep[SparseMatrixBuildable[T]],pos: Rep[Int],y: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrixbuildable_insertcol[T:Typ](self: Rep[SparseMatrixBuildable[T]],pos: Rep[Int],y: Rep[SparseVector[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrixbuildable_removerow[T:Typ](self: Rep[SparseMatrixBuildable[T]],pos: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrixbuildable_removecol[T:Typ](self: Rep[SparseMatrixBuildable[T]],pos: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrixbuildable_removerows[T:Typ](self: Rep[SparseMatrixBuildable[T]],pos: Rep[Int],num: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrixbuildable_removecols[T:Typ](self: Rep[SparseMatrixBuildable[T]],pos: Rep[Int],num: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrixbuildable_finish[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
}
trait SparseMatrixBuildableCompilerOps extends SparseMatrixBuildableOps {
  this: OptiML => 

  def sparsematrix_coo_find_offset[T:Typ](self: Rep[SparseMatrixBuildable[T]],row: Rep[Int],col: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def sparsematrix_coo_data[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[ForgeArray[T]]
  def sparsematrix_coo_rowindices[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def sparsematrix_coo_colindices[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def sparsematrix_coo_set_numrows[T:Typ](self: Rep[SparseMatrixBuildable[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_coo_set_numcols[T:Typ](self: Rep[SparseMatrixBuildable[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_coo_set_data[T:Typ](self: Rep[SparseMatrixBuildable[T]],__arg1: Rep[ForgeArray[T]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_coo_set_rowindices[T:Typ](self: Rep[SparseMatrixBuildable[T]],__arg1: Rep[ForgeArray[Int]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_coo_set_colindices[T:Typ](self: Rep[SparseMatrixBuildable[T]],__arg1: Rep[ForgeArray[Int]])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_coo_set_nnz[T:Typ](self: Rep[SparseMatrixBuildable[T]],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_coo_ensureextra[T:Typ](self: Rep[SparseMatrixBuildable[T]],extra: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def sparsematrix_coo_realloc[T:Typ](self: Rep[SparseMatrixBuildable[T]],minLen: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def coo_to_csr[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def coo_ordered[T:Typ](self: Rep[SparseMatrixBuildable[T]],nnz: Rep[Int],rowIndices: Rep[ForgeArray[Int]],colIndices: Rep[ForgeArray[Int]])(implicit __pos: SourceContext): Rep[Boolean]
  def coo_to_csr_ordered[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def coo_to_csr_unordered[T:Typ](self: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
  def coo_to_csr_finalize[T:Typ](self: Rep[SparseMatrixBuildable[T]],outData: Rep[ForgeArray[T]],outColIndices: Rep[ForgeArray[Int]],outRowPtr: Rep[ForgeArray[Int]])(implicit __pos: SourceContext): Rep[SparseMatrix[T]]
}

