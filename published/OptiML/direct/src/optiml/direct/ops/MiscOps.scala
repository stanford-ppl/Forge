package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait MiscOps extends Base {
  this: OptiML => 

  def exit(__arg0: Rep[Int])(implicit __pos: SourceContext) = misc_exit(__arg0)(__pos)
  def print(__arg0: Rep[Any])(implicit __pos: SourceContext) = misc_print(__arg0)(__pos)
  def fatal(__arg0: Rep[String])(implicit __pos: SourceContext) = misc_fatal(__arg0)(__pos)
  def println(__arg0: Rep[Any])(implicit __pos: SourceContext,__imp1: Overload1) = misc_println(__arg0)(__pos,overload1)
  def println()(implicit __pos: SourceContext,__imp1: Overload2) = misc_println()(__pos,overload2)
  def __whileDo(__arg0:  => Rep[Boolean],__arg1:  => Rep[Unit])(implicit __pos: SourceContext) = misc___whiledo(__arg0,__arg1)(__pos)
  def __ifThenElse[T:Typ](__arg0:  => Rep[Boolean],__arg1:  => Rep[T],__arg2:  => Rep[T])(implicit __pos: SourceContext) = misc___ifthenelse[T](__arg0,__arg1,__arg2)(implicitly[Typ[T]],__pos)
  def getMaxHeapSize(implicit __pos: SourceContext) = misc_getmaxheapsize()(__pos)

  implicit def repToMiscTOpsCls[T:Typ](x: Rep[T])(implicit __pos: SourceContext) = new MiscTOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToMiscTOpsCls[T:Typ](x: Var[T])(implicit __pos: SourceContext) = new MiscTOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class MiscTOpsCls[T:Typ](val self: Rep[T])(implicit __pos: SourceContext) {
    def unsafeImmutable(implicit __pos: SourceContext) = misc_unsafeimmutable[T](self)(implicitly[Typ[T]],__pos)
    def unsafeMutable()(implicit __pos: SourceContext) = misc_unsafemutable[T](self)(implicitly[Typ[T]],__pos)
  }



  def misc_exit(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def misc_print(__arg0: Rep[Any])(implicit __pos: SourceContext): Rep[Unit]
  def misc_fatal(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Nothing]
  def misc_println(__arg0: Rep[Any])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Unit]
  def misc_println()(implicit __pos: SourceContext,__imp1: Overload2): Rep[Unit]
  def misc___whiledo(__arg0:  => Rep[Boolean],__arg1:  => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def misc___ifthenelse[T:Typ](__arg0:  => Rep[Boolean],__arg1:  => Rep[T],__arg2:  => Rep[T])(implicit __pos: SourceContext): Rep[T]
  def misc_unsafeimmutable[T:Typ](__arg0: Rep[T])(implicit __pos: SourceContext): Rep[T]
  def misc_unsafemutable[T:Typ](__arg0: Rep[T])(implicit __pos: SourceContext): Rep[T]
  def misc_getmaxheapsize()(implicit __pos: SourceContext): Rep[Long]
}
