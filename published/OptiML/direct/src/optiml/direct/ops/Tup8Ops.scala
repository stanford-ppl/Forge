package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait Tup8Ops extends Base {
  this: OptiML => 

  def unpack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](t: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext,__imp1: Overload2) = tup8_unpack[A,B,C,D,E,F,G,H](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)
  def pack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](t: Tuple8[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G],Rep[H]])(implicit __pos: SourceContext,__imp1: Overload2) = tup8_pack[A,B,C,D,E,F,G,H](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)

  implicit def repToTup8Tup8OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](x: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = new Tup8Tup8OpsCls(x)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)
  implicit def varToTup8Tup8OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](x: Var[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = new Tup8Tup8OpsCls(readVar(x))(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)

  class Tup8Tup8OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](val self: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) {
    def _1(implicit __pos: SourceContext,__imp1: Overload2) = tup8__1[A,B,C,D,E,F,G,H](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)
    def _2(implicit __pos: SourceContext,__imp1: Overload2) = tup8__2[A,B,C,D,E,F,G,H](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)
    def _3(implicit __pos: SourceContext,__imp1: Overload2) = tup8__3[A,B,C,D,E,F,G,H](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)
    def _4(implicit __pos: SourceContext,__imp1: Overload2) = tup8__4[A,B,C,D,E,F,G,H](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)
    def _5(implicit __pos: SourceContext,__imp1: Overload2) = tup8__5[A,B,C,D,E,F,G,H](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)
    def _6(implicit __pos: SourceContext,__imp1: Overload2) = tup8__6[A,B,C,D,E,F,G,H](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)
    def _7(implicit __pos: SourceContext,__imp1: Overload2) = tup8__7[A,B,C,D,E,F,G,H](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)
    def _8(implicit __pos: SourceContext,__imp1: Overload1) = tup8__8[A,B,C,D,E,F,G,H](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],__pos)
    def toString(implicit __imp0: Overload5) = tup8_tostring[A,B,C,D,E,F,G,H](self)
  }



  def tup8__1[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](__arg0: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext): Rep[A]
  def tup8__2[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](__arg0: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext): Rep[B]
  def tup8__3[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](__arg0: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext): Rep[C]
  def tup8__4[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](__arg0: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext): Rep[D]
  def tup8__5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](__arg0: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext): Rep[E]
  def tup8__6[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](__arg0: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext): Rep[F]
  def tup8__7[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](__arg0: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext): Rep[G]
  def tup8__8[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](__arg0: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext): Rep[H]
  def tup8_unpack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](t: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext): Tuple8[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G],Rep[H]]
  def tup8_pack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](t: Tuple8[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G],Rep[H]])(implicit __pos: SourceContext): Rep[Tup8[A,B,C,D,E,F,G,H]]
  def tup8_tostring[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](t: Rep[Tup8[A,B,C,D,E,F,G,H]]): Rep[String]
}
trait Tup8CompilerOps extends Tup8Ops {
  this: OptiML => 

  def internal_pack8[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ](__arg0: Rep[A],__arg1: Rep[B],__arg2: Rep[C],__arg3: Rep[D],__arg4: Rep[E],__arg5: Rep[F],__arg6: Rep[G],__arg7: Rep[H])(implicit __pos: SourceContext): Rep[Tup8[A,B,C,D,E,F,G,H]]
}

