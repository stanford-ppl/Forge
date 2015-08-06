package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait Tup4Ops extends Base {
  this: OptiML => 

  def unpack[A:Typ,B:Typ,C:Typ,D:Typ](t: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext,__imp1: Overload5) = tup4_unpack[A,B,C,D](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],__pos)
  def pack[A:Typ,B:Typ,C:Typ,D:Typ](t: Tuple4[Rep[A],Rep[B],Rep[C],Rep[D]])(implicit __pos: SourceContext,__imp1: Overload5) = tup4_pack[A,B,C,D](t)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],__pos)

  implicit def repToTup4Tup4OpsCls[A:Typ,B:Typ,C:Typ,D:Typ](x: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = new Tup4Tup4OpsCls(x)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],__pos)
  implicit def varToTup4Tup4OpsCls[A:Typ,B:Typ,C:Typ,D:Typ](x: Var[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = new Tup4Tup4OpsCls(readVar(x))(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],__pos)

  class Tup4Tup4OpsCls[A:Typ,B:Typ,C:Typ,D:Typ](val self: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) {
    def _1(implicit __pos: SourceContext,__imp1: Overload5) = tup4__1[A,B,C,D](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],__pos)
    def _2(implicit __pos: SourceContext,__imp1: Overload5) = tup4__2[A,B,C,D](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],__pos)
    def _3(implicit __pos: SourceContext,__imp1: Overload5) = tup4__3[A,B,C,D](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],__pos)
    def _4(implicit __pos: SourceContext,__imp1: Overload5) = tup4__4[A,B,C,D](self)(implicitly[Typ[A]],implicitly[Typ[B]],implicitly[Typ[C]],implicitly[Typ[D]],__pos)
    def toString(implicit __imp0: Overload13) = tup4_tostring[A,B,C,D](self)
  }



  def tup4__1[A:Typ,B:Typ,C:Typ,D:Typ](__arg0: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext): Rep[A]
  def tup4__2[A:Typ,B:Typ,C:Typ,D:Typ](__arg0: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext): Rep[B]
  def tup4__3[A:Typ,B:Typ,C:Typ,D:Typ](__arg0: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext): Rep[C]
  def tup4__4[A:Typ,B:Typ,C:Typ,D:Typ](__arg0: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext): Rep[D]
  def tup4_unpack[A:Typ,B:Typ,C:Typ,D:Typ](t: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext): Tuple4[Rep[A],Rep[B],Rep[C],Rep[D]]
  def tup4_pack[A:Typ,B:Typ,C:Typ,D:Typ](t: Tuple4[Rep[A],Rep[B],Rep[C],Rep[D]])(implicit __pos: SourceContext): Rep[Tup4[A,B,C,D]]
  def tup4_tostring[A:Typ,B:Typ,C:Typ,D:Typ](t: Rep[Tup4[A,B,C,D]]): Rep[String]
}
trait Tup4CompilerOps extends Tup4Ops {
  this: OptiML => 

  def internal_pack4[A:Typ,B:Typ,C:Typ,D:Typ](__arg0: Rep[A],__arg1: Rep[B],__arg2: Rep[C],__arg3: Rep[D])(implicit __pos: SourceContext): Rep[Tup4[A,B,C,D]]
}

