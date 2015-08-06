package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait Tup5Ops extends Base {
  this: OptiML => 

  def unpack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](t: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext,__imp1: Overload6) = tup5_unpack[A,B,C,D,E](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],__pos)
  def pack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](t: Tuple5[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E]])(implicit __pos: SourceContext,__imp1: Overload6) = tup5_pack[A,B,C,D,E](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],__pos)

  implicit def repToTup5Tup5OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](x: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = new Tup5Tup5OpsCls(x)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],__pos)
  implicit def varToTup5Tup5OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](x: Var[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = new Tup5Tup5OpsCls(readVar(x))(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],__pos)

  class Tup5Tup5OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](val self: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) {
    def _1(implicit __pos: SourceContext,__imp1: Overload6) = tup5__1[A,B,C,D,E](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],__pos)
    def _2(implicit __pos: SourceContext,__imp1: Overload6) = tup5__2[A,B,C,D,E](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],__pos)
    def _3(implicit __pos: SourceContext,__imp1: Overload6) = tup5__3[A,B,C,D,E](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],__pos)
    def _4(implicit __pos: SourceContext,__imp1: Overload6) = tup5__4[A,B,C,D,E](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],__pos)
    def _5(implicit __pos: SourceContext,__imp1: Overload5) = tup5__5[A,B,C,D,E](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],__pos)
    def toString(implicit __imp0: Overload14) = tup5_tostring[A,B,C,D,E](self)
  }



  def tup5__1[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](__arg0: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext): Rep[A]
  def tup5__2[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](__arg0: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext): Rep[B]
  def tup5__3[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](__arg0: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext): Rep[C]
  def tup5__4[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](__arg0: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext): Rep[D]
  def tup5__5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](__arg0: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext): Rep[E]
  def tup5_unpack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](t: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext): Tuple5[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E]]
  def tup5_pack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](t: Tuple5[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E]])(implicit __pos: SourceContext): Rep[Tup5[A,B,C,D,E]]
  def tup5_tostring[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](t: Rep[Tup5[A,B,C,D,E]]): Rep[String]
}
trait Tup5CompilerOps extends Tup5Ops {
  this: OptiML => 

  def internal_pack5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](__arg0: Rep[A],__arg1: Rep[B],__arg2: Rep[C],__arg3: Rep[D],__arg4: Rep[E])(implicit __pos: SourceContext): Rep[Tup5[A,B,C,D,E]]
}

