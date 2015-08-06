package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait Tup7Ops extends Base {
  this: OptiML => 

  def unpack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](t: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext,__imp1: Overload1) = tup7_unpack[A,B,C,D,E,F,G](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],__pos)
  def pack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](t: Tuple7[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G]])(implicit __pos: SourceContext,__imp1: Overload1) = tup7_pack[A,B,C,D,E,F,G](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],__pos)

  implicit def repToTup7Tup7OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](x: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = new Tup7Tup7OpsCls(x)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],__pos)
  implicit def varToTup7Tup7OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](x: Var[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = new Tup7Tup7OpsCls(readVar(x))(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],__pos)

  class Tup7Tup7OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](val self: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) {
    def _1(implicit __pos: SourceContext,__imp1: Overload1) = tup7__1[A,B,C,D,E,F,G](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],__pos)
    def _2(implicit __pos: SourceContext,__imp1: Overload1) = tup7__2[A,B,C,D,E,F,G](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],__pos)
    def _3(implicit __pos: SourceContext,__imp1: Overload1) = tup7__3[A,B,C,D,E,F,G](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],__pos)
    def _4(implicit __pos: SourceContext,__imp1: Overload1) = tup7__4[A,B,C,D,E,F,G](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],__pos)
    def _5(implicit __pos: SourceContext,__imp1: Overload1) = tup7__5[A,B,C,D,E,F,G](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],__pos)
    def _6(implicit __pos: SourceContext,__imp1: Overload1) = tup7__6[A,B,C,D,E,F,G](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],__pos)
    def _7(implicit __pos: SourceContext,__imp1: Overload1) = tup7__7[A,B,C,D,E,F,G](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],implicitly[Typ[G]],__pos)
    def toString(implicit __imp0: Overload3) = tup7_tostring[A,B,C,D,E,F,G](self)
  }



  def tup7__1[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](__arg0: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext): Rep[A]
  def tup7__2[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](__arg0: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext): Rep[B]
  def tup7__3[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](__arg0: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext): Rep[C]
  def tup7__4[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](__arg0: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext): Rep[D]
  def tup7__5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](__arg0: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext): Rep[E]
  def tup7__6[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](__arg0: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext): Rep[F]
  def tup7__7[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](__arg0: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext): Rep[G]
  def tup7_unpack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](t: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext): Tuple7[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G]]
  def tup7_pack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](t: Tuple7[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G]])(implicit __pos: SourceContext): Rep[Tup7[A,B,C,D,E,F,G]]
  def tup7_tostring[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](t: Rep[Tup7[A,B,C,D,E,F,G]]): Rep[String]
}
trait Tup7CompilerOps extends Tup7Ops {
  this: OptiML => 

  def internal_pack7[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ](__arg0: Rep[A],__arg1: Rep[B],__arg2: Rep[C],__arg3: Rep[D],__arg4: Rep[E],__arg5: Rep[F],__arg6: Rep[G])(implicit __pos: SourceContext): Rep[Tup7[A,B,C,D,E,F,G]]
}

