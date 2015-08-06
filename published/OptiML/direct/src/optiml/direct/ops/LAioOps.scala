package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait LAioOps extends Base {
  this: OptiML => 

  def readVector(path: Rep[String])(implicit __pos: SourceContext,__imp1: Overload1) = laio_readvector(path)(__pos,overload1)
  def readMatrix(path: Rep[String])(implicit __pos: SourceContext,__imp1: Overload1) = laio_readmatrix(path)(__pos,overload1)
  def readMatrix(path: Rep[String],delim: Rep[String])(implicit __pos: SourceContext,__imp1: Overload2) = laio_readmatrix(path,delim)(__pos,overload2)
  def readVector[Elem:Typ](path: Rep[String],schemaBldr: (Rep[String]) => Rep[Elem])(implicit __pos: SourceContext,__imp1: Overload2) = laio_readvector[Elem](path,schemaBldr)(implicitly[Typ[Elem]],__pos,overload2)
  def readVectorAndParse[Elem:Typ](path: Rep[String],schemaBldr: (Rep[DenseVector[String]]) => Rep[Elem],delim: Rep[String] = unit("\\s+"))(implicit __pos: SourceContext) = laio_readvectorandparse[Elem](path,schemaBldr,delim)(implicitly[Typ[Elem]],__pos)
  def readMatrix[Elem:Typ](path: Rep[String],schemaBldr: (Rep[String]) => Rep[Elem],delim: Rep[String] = unit("\\s+"))(implicit __pos: SourceContext,__imp1: Overload3) = laio_readmatrix[Elem](path,schemaBldr,delim)(implicitly[Typ[Elem]],__pos,overload3)
  def readMatrixAndParse[Elem:Typ](path: Rep[String],schemaBldr: (Rep[DenseVector[String]]) => Rep[DenseVector[Elem]],delim: Rep[String] = unit("\\s+"))(implicit __pos: SourceContext) = laio_readmatrixandparse[Elem](path,schemaBldr,delim)(implicitly[Typ[Elem]],__pos)
  def writeVector[Elem:Stringable:Typ](v: Rep[DenseVector[Elem]],path: Rep[String])(implicit __pos: SourceContext) = laio_writevector[Elem](v,path)(implicitly[Stringable[Elem]],implicitly[Typ[Elem]],__pos)
  def writeMatrix[Elem:Stringable:Typ](m: Rep[DenseMatrix[Elem]],path: Rep[String],delim: Rep[String] = unit("    "))(implicit __pos: SourceContext) = laio_writematrix[Elem](m,path,delim)(implicitly[Stringable[Elem]],implicitly[Typ[Elem]],__pos)
  def deleteFile(__arg0: Rep[String])(implicit __pos: SourceContext) = laio_deletefile(__arg0)(__pos)


  def laio_readvector(path: Rep[String])(implicit __pos: SourceContext,__imp1: Overload1): Rep[DenseVector[Double]]
  def laio_readmatrix(path: Rep[String])(implicit __pos: SourceContext,__imp1: Overload1): Rep[DenseMatrix[Double]]
  def laio_readmatrix(path: Rep[String],delim: Rep[String])(implicit __pos: SourceContext,__imp1: Overload2): Rep[DenseMatrix[Double]]
  def laio_readvector[Elem:Typ](path: Rep[String],schemaBldr: (Rep[String]) => Rep[Elem])(implicit __pos: SourceContext,__imp1: Overload2): Rep[DenseVector[Elem]]
  def laio_readvectorandparse[Elem:Typ](path: Rep[String],schemaBldr: (Rep[DenseVector[String]]) => Rep[Elem],delim: Rep[String] = unit("\\s+"))(implicit __pos: SourceContext): Rep[DenseVector[Elem]]
  def laio_readmatrix[Elem:Typ](path: Rep[String],schemaBldr: (Rep[String]) => Rep[Elem],delim: Rep[String] = unit("\\s+"))(implicit __pos: SourceContext,__imp1: Overload3): Rep[DenseMatrix[Elem]]
  def laio_readmatrixandparse[Elem:Typ](path: Rep[String],schemaBldr: (Rep[DenseVector[String]]) => Rep[DenseVector[Elem]],delim: Rep[String] = unit("\\s+"))(implicit __pos: SourceContext): Rep[DenseMatrix[Elem]]
  def laio_writevector[Elem:Stringable:Typ](v: Rep[DenseVector[Elem]],path: Rep[String])(implicit __pos: SourceContext): Rep[Unit]
  def laio_writematrix[Elem:Stringable:Typ](m: Rep[DenseMatrix[Elem]],path: Rep[String],delim: Rep[String] = unit("    "))(implicit __pos: SourceContext): Rep[Unit]
  def laio_deletefile(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Unit]
}
trait LAioCompilerOps extends LAioOps {
  this: OptiML => 

  def optila_todouble(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Double]
  def readFirstLine(path: Rep[String])(implicit __pos: SourceContext): Rep[String]
}

