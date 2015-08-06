package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait ComputeStreamOps extends Base {
  this: OptiML => 

  object ComputeStream {
    def apply[T:Typ](numRows: Rep[Int],numCols: Rep[Int])(func: (Rep[Int],Rep[Int]) => Rep[T])(implicit __pos: SourceContext,__imp1: Overload25) = computestream_object_apply[T](numRows,numCols,func)(implicitly[Typ[T]],__pos)
  }

  implicit def repToComputeStreamComputeStreamOpsCls[T:Typ](x: Rep[ComputeStream[T]])(implicit __pos: SourceContext) = new ComputeStreamComputeStreamOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToComputeStreamComputeStreamOpsCls[T:Typ](x: Var[ComputeStream[T]])(implicit __pos: SourceContext) = new ComputeStreamComputeStreamOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class ComputeStreamComputeStreamOpsCls[T:Typ](val self: Rep[ComputeStream[T]])(implicit __pos: SourceContext) {
    def numRows(implicit __pos: SourceContext,__imp1: Overload5) = computestream_numrows[T](self)(implicitly[Typ[T]],__pos)
    def numCols(implicit __pos: SourceContext,__imp1: Overload5) = computestream_numcols[T](self)(implicitly[Typ[T]],__pos)
    def apply(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload34) = computestream_apply[T](self,__arg1,__arg2)(implicitly[Typ[T]],__pos)
    def foreach(__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload5) = computestream_foreach[T](self,__arg1)(implicitly[Typ[T]],__pos)
    def foreachRow(__arg1: (Rep[DenseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload4) = computestream_foreachrow[T](self,__arg1)(implicitly[Typ[T]],__pos)
  }



  def computestream_object_apply[T:Typ](numRows: Rep[Int],numCols: Rep[Int],func: (Rep[Int],Rep[Int]) => Rep[T])(implicit __pos: SourceContext): Rep[ComputeStream[T]]
  def computestream_numrows[T:Typ](self: Rep[ComputeStream[T]])(implicit __pos: SourceContext): Rep[Int]
  def computestream_numcols[T:Typ](self: Rep[ComputeStream[T]])(implicit __pos: SourceContext): Rep[Int]
  def computestream_apply[T:Typ](self: Rep[ComputeStream[T]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[T]
  def computestream_foreach[T:Typ](self: Rep[ComputeStream[T]],__arg1: (Rep[T]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def computestream_foreachrow[T:Typ](self: Rep[ComputeStream[T]],__arg1: (Rep[DenseVectorView[T]]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
}
trait ComputeStreamCompilerOps extends ComputeStreamOps {
  this: OptiML => 

  def stream_func[T:Typ](self: Rep[ComputeStream[T]])(implicit __pos: SourceContext): Rep[Function1[Tup2[Int,Int],T]]
}

