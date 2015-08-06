package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait SByteBufferOps extends Base {
  this: OptiML => 

  def ByteBuffer(__arg0: Rep[Int])(implicit __pos: SourceContext) = sbytebuffer_bytebuffer(__arg0)(__pos)
  def ByteBufferWrap(__arg0: Rep[ForgeArray[Byte]])(implicit __pos: SourceContext) = sbytebuffer_bytebufferwrap(__arg0)(__pos)

  implicit def repToSByteBufferjavanioByteBufferOpsCls(x: Rep[java.nio.ByteBuffer])(implicit __pos: SourceContext) = new SByteBufferjavanioByteBufferOpsCls(x)(__pos)
  implicit def varToSByteBufferjavanioByteBufferOpsCls(x: Var[java.nio.ByteBuffer])(implicit __pos: SourceContext) = new SByteBufferjavanioByteBufferOpsCls(readVar(x))(__pos)

  class SByteBufferjavanioByteBufferOpsCls(val self: Rep[java.nio.ByteBuffer])(implicit __pos: SourceContext) {
    def rewind()(implicit __pos: SourceContext) = sbytebuffer_rewind(self)(__pos)
    def array(implicit __pos: SourceContext) = sbytebuffer_array(self)(__pos)
    def getInt(implicit __pos: SourceContext) = sbytebuffer_getint(self)(__pos)
    def getDouble(implicit __pos: SourceContext) = sbytebuffer_getdouble(self)(__pos)
    def putInt(__arg1: Rep[Int])(implicit __pos: SourceContext) = sbytebuffer_putint(self,__arg1)(__pos)
    def putDouble(__arg1: Rep[Double])(implicit __pos: SourceContext) = sbytebuffer_putdouble(self,__arg1)(__pos)
    def get(__arg1: Rep[ForgeArray[Int]],__arg2: Rep[Int],__arg3: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = sbytebuffer_get(self,__arg1,__arg2,__arg3)(__pos,overload1)
    def get(__arg1: Rep[ForgeArray[Double]],__arg2: Rep[Int],__arg3: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = sbytebuffer_get(self,__arg1,__arg2,__arg3)(__pos,overload2)
    def put(__arg1: Rep[ForgeArray[Int]],__arg2: Rep[Int],__arg3: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = sbytebuffer_put(self,__arg1,__arg2,__arg3)(__pos,overload1)
    def put(__arg1: Rep[ForgeArray[Double]],__arg2: Rep[Int],__arg3: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = sbytebuffer_put(self,__arg1,__arg2,__arg3)(__pos,overload2)
  }



  def sbytebuffer_bytebuffer(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[java.nio.ByteBuffer]
  def sbytebuffer_bytebufferwrap(__arg0: Rep[ForgeArray[Byte]])(implicit __pos: SourceContext): Rep[java.nio.ByteBuffer]
  def sbytebuffer_rewind(__arg0: Rep[java.nio.ByteBuffer])(implicit __pos: SourceContext): Rep[Unit]
  def sbytebuffer_array(__arg0: Rep[java.nio.ByteBuffer])(implicit __pos: SourceContext): Rep[ForgeArray[Byte]]
  def sbytebuffer_getint(__arg0: Rep[java.nio.ByteBuffer])(implicit __pos: SourceContext): Rep[Int]
  def sbytebuffer_getdouble(__arg0: Rep[java.nio.ByteBuffer])(implicit __pos: SourceContext): Rep[Double]
  def sbytebuffer_putint(__arg0: Rep[java.nio.ByteBuffer],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[java.nio.ByteBuffer]
  def sbytebuffer_putdouble(__arg0: Rep[java.nio.ByteBuffer],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[java.nio.ByteBuffer]
  def sbytebuffer_get(__arg0: Rep[java.nio.ByteBuffer],__arg1: Rep[ForgeArray[Int]],__arg2: Rep[Int],__arg3: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1): Rep[java.nio.IntBuffer]
  def sbytebuffer_get(__arg0: Rep[java.nio.ByteBuffer],__arg1: Rep[ForgeArray[Double]],__arg2: Rep[Int],__arg3: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2): Rep[java.nio.DoubleBuffer]
  def sbytebuffer_put(__arg0: Rep[java.nio.ByteBuffer],__arg1: Rep[ForgeArray[Int]],__arg2: Rep[Int],__arg3: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1): Rep[java.nio.IntBuffer]
  def sbytebuffer_put(__arg0: Rep[java.nio.ByteBuffer],__arg1: Rep[ForgeArray[Double]],__arg2: Rep[Int],__arg3: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2): Rep[java.nio.DoubleBuffer]
}
