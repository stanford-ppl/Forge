package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait Tup2Ops extends Base {
  this: OptiML => 

  def unpack[A:Typ,B:Typ](t: Rep[Tup2[A,B]])(implicit __pos: SourceContext,__imp1: Overload8) = tup2_unpack[A,B](t)(implicitly[Typ[A]],implicitly[Typ[B]],__pos)
  def pack[A:Typ,B:Typ](t: Tuple2[Rep[A],Rep[B]])(implicit __pos: SourceContext,__imp1: Overload8) = tup2_pack[A,B](t)(implicitly[Typ[A]],implicitly[Typ[B]],__pos,overload8)
  def pack[A:Typ,B:Typ](__arg0: Tuple2[Var[A],Rep[B]])(implicit __pos: SourceContext,__imp1: ROverload9): Rep[Tup2[A,B]] = { tup2_pack((__arg0._1,__arg0._2)) }
  def pack[A:Typ,B:Typ](__arg0: Tuple2[Rep[A],Var[B]])(implicit __pos: SourceContext,__imp1: ROverload10): Rep[Tup2[A,B]] = { tup2_pack((__arg0._1,__arg0._2)) }
  def pack[A:Typ,B:Typ](__arg0: Tuple2[Var[A],Var[B]])(implicit __pos: SourceContext,__imp1: ROverload11): Rep[Tup2[A,B]] = { tup2_pack((__arg0._1,__arg0._2)) }

  implicit def repToTup2Tup2OpsCls[A:Typ,B:Typ](x: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = new Tup2Tup2OpsCls(x)(implicitly[Typ[A]],implicitly[Typ[B]],__pos)
  implicit def varToTup2Tup2OpsCls[A:Typ,B:Typ](x: Var[Tup2[A,B]])(implicit __pos: SourceContext) = new Tup2Tup2OpsCls(readVar(x))(implicitly[Typ[A]],implicitly[Typ[B]],__pos)

  class Tup2Tup2OpsCls[A:Typ,B:Typ](val self: Rep[Tup2[A,B]])(implicit __pos: SourceContext) {
    def _1(implicit __pos: SourceContext,__imp1: Overload8) = tup2__1[A,B](self)(implicitly[Typ[A]],implicitly[Typ[B]],__pos)
    def _2(implicit __pos: SourceContext,__imp1: Overload8) = tup2__2[A,B](self)(implicitly[Typ[A]],implicitly[Typ[B]],__pos)
    def toString(implicit __imp0: Overload16) = tup2_tostring[A,B](self)
  }



  def tup2__1[A:Typ,B:Typ](__arg0: Rep[Tup2[A,B]])(implicit __pos: SourceContext): Rep[A]
  def tup2__2[A:Typ,B:Typ](__arg0: Rep[Tup2[A,B]])(implicit __pos: SourceContext): Rep[B]
  def tup2_unpack[A:Typ,B:Typ](t: Rep[Tup2[A,B]])(implicit __pos: SourceContext): Tuple2[Rep[A],Rep[B]]
  def tup2_pack[A:Typ,B:Typ](t: Tuple2[Rep[A],Rep[B]])(implicit __pos: SourceContext,__imp1: Overload8): Rep[Tup2[A,B]]
  def tup2_tostring[A:Typ,B:Typ](t: Rep[Tup2[A,B]]): Rep[String]
}
trait Tup2CompilerOps extends Tup2Ops {
  this: OptiML => 

  def internal_pack2[A:Typ,B:Typ](__arg0: Rep[A],__arg1: Rep[B])(implicit __pos: SourceContext): Rep[Tup2[A,B]]
}

