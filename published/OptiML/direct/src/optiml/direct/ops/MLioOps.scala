package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait MLioOps extends Base {
  this: OptiML => 

  def readARFF[Row:Typ](path: Rep[String],schemaBldr: (Rep[DenseVector[String]]) => Rep[Row])(implicit __pos: SourceContext) = mlio_readarff[Row](path,schemaBldr)(implicitly[Typ[Row]],__pos)

  implicit def repToMLiojavaioDataInputStreamOpsCls(x: Rep[java.io.DataInputStream])(implicit __pos: SourceContext) = new MLiojavaioDataInputStreamOpsCls(x)(__pos)
  implicit def varToMLiojavaioDataInputStreamOpsCls(x: Var[java.io.DataInputStream])(implicit __pos: SourceContext) = new MLiojavaioDataInputStreamOpsCls(readVar(x))(__pos)

  class MLiojavaioDataInputStreamOpsCls(val self: Rep[java.io.DataInputStream])(implicit __pos: SourceContext) {
    def available()(implicit __pos: SourceContext) = mlio_available(self)(__pos)
    def fclose()(implicit __pos: SourceContext) = mlio_fclose(self)(__pos)
    def readShort()(implicit __pos: SourceContext) = mlio_readshort(self)(__pos)
    def readInt()(implicit __pos: SourceContext) = mlio_readint(self)(__pos)
    def readLong()(implicit __pos: SourceContext) = mlio_readlong(self)(__pos)
    def readDouble()(implicit __pos: SourceContext) = mlio_readdouble(self)(__pos)
    def readBoolean()(implicit __pos: SourceContext) = mlio_readboolean(self)(__pos)
  }



  def mlio_available(__arg0: Rep[java.io.DataInputStream])(implicit __pos: SourceContext): Rep[Int]
  def mlio_fclose(__arg0: Rep[java.io.DataInputStream])(implicit __pos: SourceContext): Rep[Unit]
  def mlio_readshort(__arg0: Rep[java.io.DataInputStream])(implicit __pos: SourceContext): Rep[Short]
  def mlio_readint(__arg0: Rep[java.io.DataInputStream])(implicit __pos: SourceContext): Rep[Int]
  def mlio_readlong(__arg0: Rep[java.io.DataInputStream])(implicit __pos: SourceContext): Rep[Long]
  def mlio_readdouble(__arg0: Rep[java.io.DataInputStream])(implicit __pos: SourceContext): Rep[Double]
  def mlio_readboolean(__arg0: Rep[java.io.DataInputStream])(implicit __pos: SourceContext): Rep[Boolean]
  def mlio_readarff[Row:Typ](path: Rep[String],schemaBldr: (Rep[DenseVector[String]]) => Rep[Row])(implicit __pos: SourceContext): Rep[DenseVector[Row]]
}
trait MLioCompilerOps extends MLioOps {
  this: OptiML => 

  def datainputstream_new(path: Rep[String])(implicit __pos: SourceContext): Rep[java.io.DataInputStream]
}

