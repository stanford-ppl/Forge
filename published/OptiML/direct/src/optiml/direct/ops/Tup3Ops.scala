package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait Tup3Ops extends Base {
  this: OptiML => 

  def unpack[A:Typ,B:Typ,C:Typ](t: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext,__imp1: Overload7) = tup3_unpack[A,B,C](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],__pos)
  def pack[A:Typ,B:Typ,C:Typ](t: Tuple3[Rep[A],Rep[B],Rep[C]])(implicit __pos: SourceContext,__imp1: Overload7) = tup3_pack[A,B,C](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],__pos)

  implicit def repToTup3Tup3OpsCls[A:Typ,B:Typ,C:Typ](x: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = new Tup3Tup3OpsCls(x)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],__pos)
  implicit def varToTup3Tup3OpsCls[A:Typ,B:Typ,C:Typ](x: Var[Tup3[A,B,C]])(implicit __pos: SourceContext) = new Tup3Tup3OpsCls(readVar(x))(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],__pos)

  class Tup3Tup3OpsCls[A:Typ,B:Typ,C:Typ](val self: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) {
    def _1(implicit __pos: SourceContext,__imp1: Overload7) = tup3__1[A,B,C](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],__pos)
    def _2(implicit __pos: SourceContext,__imp1: Overload7) = tup3__2[A,B,C](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],__pos)
    def _3(implicit __pos: SourceContext,__imp1: Overload7) = tup3__3[A,B,C](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],__pos)
    def toString(implicit __imp0: Overload15) = tup3_tostring[A,B,C](self)
  }



  def tup3__1[A:Typ,B:Typ,C:Typ](__arg0: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext): Rep[A]
  def tup3__2[A:Typ,B:Typ,C:Typ](__arg0: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext): Rep[B]
  def tup3__3[A:Typ,B:Typ,C:Typ](__arg0: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext): Rep[C]
  def tup3_unpack[A:Typ,B:Typ,C:Typ](t: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext): Tuple3[Rep[A],Rep[B],Rep[C]]
  def tup3_pack[A:Typ,B:Typ,C:Typ](t: Tuple3[Rep[A],Rep[B],Rep[C]])(implicit __pos: SourceContext): Rep[Tup3[A,B,C]]
  def tup3_tostring[A:Typ,B:Typ,C:Typ](t: Rep[Tup3[A,B,C]]): Rep[String]
}
trait Tup3CompilerOps extends Tup3Ops {
  this: OptiML => 

  def internal_pack3[A:Typ,B:Typ,C:Typ](__arg0: Rep[A],__arg1: Rep[B],__arg2: Rep[C])(implicit __pos: SourceContext): Rep[Tup3[A,B,C]]
}

