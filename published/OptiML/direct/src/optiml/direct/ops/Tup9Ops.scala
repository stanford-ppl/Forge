package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait Tup9Ops extends Base {
  this: OptiML => 

  def unpack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](t: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext,__imp1: Overload4) = tup9_unpack[A,B,C,D,E,F,G,H,I](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)
  def pack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](t: Tuple9[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G],Rep[H],Rep[I]])(implicit __pos: SourceContext,__imp1: Overload4) = tup9_pack[A,B,C,D,E,F,G,H,I](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)

  implicit def repToTup9Tup9OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](x: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = new Tup9Tup9OpsCls(x)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)
  implicit def varToTup9Tup9OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](x: Var[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = new Tup9Tup9OpsCls(readVar(x))(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)

  class Tup9Tup9OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](val self: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) {
    def _1(implicit __pos: SourceContext,__imp1: Overload4) = tup9__1[A,B,C,D,E,F,G,H,I](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)
    def _2(implicit __pos: SourceContext,__imp1: Overload4) = tup9__2[A,B,C,D,E,F,G,H,I](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)
    def _3(implicit __pos: SourceContext,__imp1: Overload4) = tup9__3[A,B,C,D,E,F,G,H,I](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)
    def _4(implicit __pos: SourceContext,__imp1: Overload4) = tup9__4[A,B,C,D,E,F,G,H,I](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)
    def _5(implicit __pos: SourceContext,__imp1: Overload4) = tup9__5[A,B,C,D,E,F,G,H,I](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)
    def _6(implicit __pos: SourceContext,__imp1: Overload4) = tup9__6[A,B,C,D,E,F,G,H,I](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)
    def _7(implicit __pos: SourceContext,__imp1: Overload3) = tup9__7[A,B,C,D,E,F,G,H,I](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)
    def _8(implicit __pos: SourceContext,__imp1: Overload2) = tup9__8[A,B,C,D,E,F,G,H,I](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)
    def _9(implicit __pos: SourceContext) = tup9__9[A,B,C,D,E,F,G,H,I](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],implicitly[Typ[H]],implicitly[Typ[I]],__pos)
    def toString(implicit __imp0: Overload10) = tup9_tostring[A,B,C,D,E,F,G,H,I](self)
  }



  def tup9__1[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](__arg0: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext): Rep[A]
  def tup9__2[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](__arg0: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext): Rep[B]
  def tup9__3[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](__arg0: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext): Rep[C]
  def tup9__4[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](__arg0: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext): Rep[D]
  def tup9__5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](__arg0: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext): Rep[E]
  def tup9__6[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](__arg0: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext): Rep[F]
  def tup9__7[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](__arg0: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext): Rep[G]
  def tup9__8[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](__arg0: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext): Rep[H]
  def tup9__9[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](__arg0: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext): Rep[I]
  def tup9_unpack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](t: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext): Tuple9[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G],Rep[H],Rep[I]]
  def tup9_pack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](t: Tuple9[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G],Rep[H],Rep[I]])(implicit __pos: SourceContext): Rep[Tup9[A,B,C,D,E,F,G,H,I]]
  def tup9_tostring[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](t: Rep[Tup9[A,B,C,D,E,F,G,H,I]]): Rep[String]
}
trait Tup9CompilerOps extends Tup9Ops {
  this: OptiML => 

  def internal_pack9[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ](__arg0: Rep[A],__arg1: Rep[B],__arg2: Rep[C],__arg3: Rep[D],__arg4: Rep[E],__arg5: Rep[F],__arg6: Rep[G],__arg7: Rep[H],__arg8: Rep[I])(implicit __pos: SourceContext): Rep[Tup9[A,B,C,D,E,F,G,H,I]]
}

