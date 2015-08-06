package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait Tup6Ops extends Base {
  this: OptiML => 

  def unpack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](t: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext,__imp1: Overload3) = tup6_unpack[A,B,C,D,E,F](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],__pos)
  def pack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](t: Tuple6[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F]])(implicit __pos: SourceContext,__imp1: Overload3) = tup6_pack[A,B,C,D,E,F](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],__pos)

  implicit def repToTup6Tup6OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](x: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = new Tup6Tup6OpsCls(x)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],__pos)
  implicit def varToTup6Tup6OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](x: Var[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = new Tup6Tup6OpsCls(readVar(x))(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],__pos)

  class Tup6Tup6OpsCls[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](val self: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) {
    def _1(implicit __pos: SourceContext,__imp1: Overload3) = tup6__1[A,B,C,D,E,F](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],__pos)
    def _2(implicit __pos: SourceContext,__imp1: Overload3) = tup6__2[A,B,C,D,E,F](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],__pos)
    def _3(implicit __pos: SourceContext,__imp1: Overload3) = tup6__3[A,B,C,D,E,F](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],__pos)
    def _4(implicit __pos: SourceContext,__imp1: Overload3) = tup6__4[A,B,C,D,E,F](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],__pos)
    def _5(implicit __pos: SourceContext,__imp1: Overload3) = tup6__5[A,B,C,D,E,F](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],__pos)
    def _6(implicit __pos: SourceContext,__imp1: Overload3) = tup6__6[A,B,C,D,E,F](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],implicitly[Typ[E]],implicitly[Typ[F]],__pos)
    def toString(implicit __imp0: Overload7) = tup6_tostring[A,B,C,D,E,F](self)
  }



  def tup6__1[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](__arg0: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext): Rep[A]
  def tup6__2[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](__arg0: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext): Rep[B]
  def tup6__3[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](__arg0: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext): Rep[C]
  def tup6__4[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](__arg0: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext): Rep[D]
  def tup6__5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](__arg0: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext): Rep[E]
  def tup6__6[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](__arg0: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext): Rep[F]
  def tup6_unpack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](t: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext): Tuple6[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F]]
  def tup6_pack[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](t: Tuple6[Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F]])(implicit __pos: SourceContext): Rep[Tup6[A,B,C,D,E,F]]
  def tup6_tostring[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](t: Rep[Tup6[A,B,C,D,E,F]]): Rep[String]
}
trait Tup6CompilerOps extends Tup6Ops {
  this: OptiML => 

  def internal_pack6[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ](__arg0: Rep[A],__arg1: Rep[B],__arg2: Rep[C],__arg3: Rep[D],__arg4: Rep[E],__arg5: Rep[F])(implicit __pos: SourceContext): Rep[Tup6[A,B,C,D,E,F]]
}

