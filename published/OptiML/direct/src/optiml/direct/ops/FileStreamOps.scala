package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait FileStreamOps extends Base {
  this: OptiML => 

  object FileStream {
    def apply(__arg0: Rep[String])(implicit __pos: SourceContext,__imp1: Overload5) = filestream_object_apply(__arg0)(__pos)
  }

  def hashMatrixDeserializer(hash: Rep[HashStream[DenseMatrix[Double]]],k: Rep[String])(implicit __pos: SourceContext) = filestream_hashmatrixdeserializer(hash,k)(__pos)
  def getChunkByteSize(implicit __pos: SourceContext) = filestream_getchunkbytesize()(__pos)

  implicit def repToFileStreamFileStreamOpsCls(x: Rep[FileStream])(implicit __pos: SourceContext) = new FileStreamFileStreamOpsCls(x)(__pos)
  implicit def varToFileStreamFileStreamOpsCls(x: Var[FileStream])(implicit __pos: SourceContext) = new FileStreamFileStreamOpsCls(readVar(x))(__pos)

  class FileStreamFileStreamOpsCls(val self: Rep[FileStream])(implicit __pos: SourceContext) {
    def path(implicit __pos: SourceContext) = filestream_path(self)(__pos)
    def foreach(__arg1: (Rep[String]) => Rep[Unit])(implicit __pos: SourceContext,__imp1: Overload1) = filestream_foreach(self,__arg1)(__pos)
    def map(outFile: Rep[String],preserveOrder: Rep[Boolean] = unit(false),chunkSize: Rep[Long] = filestream_getchunkbytesize())(func: (Rep[String]) => Rep[String])(implicit __pos: SourceContext,__imp1: Overload1) = filestream_map(self,outFile,preserveOrder,chunkSize,func)(__pos)
    def mapRows[R:Typ](outFile: Rep[String],inDelim: Rep[String] = unit("\\s+"),outDelim: Rep[String] = unit("    "))(func: (Rep[DenseVector[String]]) => Rep[DenseVector[R]])(implicit __pos: SourceContext,__imp0: Stringable[R],__imp2: Overload1) = filestream_maprows[R](self,outFile,inDelim,outDelim,func)(implicitly[Typ[R]],__pos,__imp0)
    def groupRowsBy(outTable: Rep[String],delim: Rep[String] = unit("\\s+"),appendToHash: Rep[Boolean] = unit(false))(keyFunc: (Rep[DenseVector[String]]) => Rep[String],valFunc: (Rep[DenseVector[String]]) => Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: Overload1) = filestream_grouprowsby(self,outTable,delim,appendToHash,keyFunc,valFunc)(__pos)
    def reduce[T:Typ](zero: Rep[T])(func: (Rep[String]) => Rep[T])(rfunc: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext,__imp1: Overload1) = filestream_reduce[T](self,zero,func,rfunc)(implicitly[Typ[T]],__pos)
  }



  def filestream_object_apply(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[FileStream]
  def filestream_hashmatrixdeserializer(hash: Rep[HashStream[DenseMatrix[Double]]],k: Rep[String])(implicit __pos: SourceContext): Rep[DenseMatrix[Double]]
  def filestream_getchunkbytesize()(implicit __pos: SourceContext): Rep[Long]
  def filestream_path(self: Rep[FileStream])(implicit __pos: SourceContext): Rep[String]
  def filestream_foreach(self: Rep[FileStream],__arg1: (Rep[String]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def filestream_map(self: Rep[FileStream],outFile: Rep[String],preserveOrder: Rep[Boolean] = unit(false),chunkSize: Rep[Long] = filestream_getchunkbytesize(),func: (Rep[String]) => Rep[String])(implicit __pos: SourceContext): Rep[FileStream]
  def filestream_maprows[R:Typ](self: Rep[FileStream],outFile: Rep[String],inDelim: Rep[String] = unit("\\s+"),outDelim: Rep[String] = unit("    "),func: (Rep[DenseVector[String]]) => Rep[DenseVector[R]])(implicit __pos: SourceContext,__imp0: Stringable[R]): Rep[FileStream]
  def filestream_grouprowsby(self: Rep[FileStream],outTable: Rep[String],delim: Rep[String] = unit("\\s+"),appendToHash: Rep[Boolean] = unit(false),keyFunc: (Rep[DenseVector[String]]) => Rep[String],valFunc: (Rep[DenseVector[String]]) => Rep[DenseVector[Double]])(implicit __pos: SourceContext): Rep[HashStream[DenseMatrix[Double]]]
  def filestream_reduce[T:Typ](self: Rep[FileStream],zero: Rep[T],func: (Rep[String]) => Rep[T],rfunc: (Rep[T],Rep[T]) => Rep[T])(implicit __pos: SourceContext): Rep[T]
}
trait FileStreamCompilerOps extends FileStreamOps {
  this: OptiML => 

  def hashMatrixLogicalKey(k: Rep[String],index: Rep[Int])(implicit __pos: SourceContext): Rep[String]
  def processFileChunks[R:Typ](self: Rep[FileStream],readFunc: (Rep[String]) => Rep[R],processFunc: (Rep[ForgeArray[R]]) => Rep[Unit],chunkSize: Rep[Long] = filestream_getchunkbytesize())(implicit __pos: SourceContext): Rep[Unit]
}

