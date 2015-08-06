package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait OrderingOps extends Base {
  this: OptiML => 

  def __equal[A:Typ,B:Typ](__arg0: Rep[A],__arg1: Rep[B])(implicit __pos: SourceContext,__imp1: Overload8) = forge_equals[A,B](__arg0,__arg1)(implicitly[Typ[A]],implicitly[Typ[B]],__pos)
  def __equal[A:Typ,B:Typ](__arg0: Var[A],__arg1: Rep[B])(implicit __pos: SourceContext,__imp1: ROverload9): Rep[Boolean] = { forge_equals(readVar(__arg0), __arg1) }
  def __equal[A:Typ,B:Typ](__arg0: Rep[A],__arg1: Var[B])(implicit __pos: SourceContext,__imp1: ROverload10): Rep[Boolean] = { forge_equals(__arg0, readVar(__arg1)) }
  def __equal[A:Typ,B:Typ](__arg0: Var[A],__arg1: Var[B])(implicit __pos: SourceContext,__imp1: ROverload11): Rep[Boolean] = { forge_equals(readVar(__arg0), readVar(__arg1)) }
  def __equal[A:Typ,B:Typ](__arg0: Rep[A],__arg1: B)(implicit __pos: SourceContext,__imp1: ROverload12): Rep[Boolean] = { forge_equals(__arg0, unit(__arg1)) }
  def __equal[A:Typ,B:Typ](__arg0: Var[A],__arg1: B)(implicit __pos: SourceContext,__imp1: ROverload13): Rep[Boolean] = { forge_equals(readVar(__arg0), unit(__arg1)) }
  def __equal[A:Typ,B:Typ](__arg0: A,__arg1: Rep[B])(implicit __pos: SourceContext,__imp1: ROverload14): Rep[Boolean] = { forge_equals(unit(__arg0), __arg1) }
  def __equal[A:Typ,B:Typ](__arg0: A,__arg1: Var[B])(implicit __pos: SourceContext,__imp1: ROverload15): Rep[Boolean] = { forge_equals(unit(__arg0), readVar(__arg1)) }

  implicit def repToOrderingAOpsCls[A:Typ](x: Rep[A])(implicit __pos: SourceContext) = new OrderingAOpsCls(x)(implicitly[Typ[A]],__pos)
  implicit def liftToOrderingAOpsCls[A:Typ](x: A)(implicit __pos: SourceContext) = new OrderingAOpsCls(unit(x))(implicitly[Typ[A]],__pos)
  implicit def varToOrderingAOpsCls[A:Typ](x: Var[A])(implicit __pos: SourceContext) = new OrderingAOpsCls(readVar(x))(implicitly[Typ[A]],__pos)

  class OrderingAOpsCls[A:Typ](val self: Rep[A])(implicit __pos: SourceContext) {
    def !=[B:Typ](__arg1: Rep[B])(implicit __pos: SourceContext,__imp1: Overload1) = forge_notequals[A,B](self,__arg1)(implicitly[Typ[A]],implicitly[Typ[B]],__pos)
    def !=[B:Typ](__arg1: Var[B])(implicit __pos: SourceContext,__imp1: ROverload3) = { forge_notequals(self, readVar(__arg1)) }
    def !=[B:Typ](__arg1: B)(implicit __pos: SourceContext,__imp1: ROverload5) = { forge_notequals(self, unit(__arg1)) }
    def min(__arg1: Rep[A])(implicit __cb0: Ordering[A],__pos: SourceContext,__imp1: Overload8) = ordering_min[A](self,__arg1)(implicitly[Ordering[A]],implicitly[Typ[A]],__pos)
    def max(__arg1: Rep[A])(implicit __cb0: Ordering[A],__pos: SourceContext,__imp1: Overload8) = ordering_max[A](self,__arg1)(implicitly[Ordering[A]],implicitly[Typ[A]],__pos)
    def <(__arg1: Rep[A])(implicit __cb0: Ordering[A],__pos: SourceContext) = ordering_lt[A](self,__arg1)(implicitly[Ordering[A]],implicitly[Typ[A]],__pos)
    def <=(__arg1: Rep[A])(implicit __cb0: Ordering[A],__pos: SourceContext) = ordering_lteq[A](self,__arg1)(implicitly[Ordering[A]],implicitly[Typ[A]],__pos)
    def >(__arg1: Rep[A])(implicit __cb0: Ordering[A],__pos: SourceContext) = ordering_gt[A](self,__arg1)(implicitly[Ordering[A]],implicitly[Typ[A]],__pos)
    def >=(__arg1: Rep[A])(implicit __cb0: Ordering[A],__pos: SourceContext) = ordering_gteq[A](self,__arg1)(implicitly[Ordering[A]],implicitly[Typ[A]],__pos)
  }



  def forge_equals[A:Typ,B:Typ](__arg0: Rep[A],__arg1: Rep[B])(implicit __pos: SourceContext): Rep[Boolean]
  def forge_notequals[A:Typ,B:Typ](__arg0: Rep[A],__arg1: Rep[B])(implicit __pos: SourceContext): Rep[Boolean]
  def ordering_min[A:Ordering:Typ](__arg0: Rep[A],__arg1: Rep[A])(implicit __pos: SourceContext): Rep[A]
  def ordering_max[A:Ordering:Typ](__arg0: Rep[A],__arg1: Rep[A])(implicit __pos: SourceContext): Rep[A]
  def ordering_lt[A:Ordering:Typ](__arg0: Rep[A],__arg1: Rep[A])(implicit __pos: SourceContext): Rep[Boolean]
  def ordering_lteq[A:Ordering:Typ](__arg0: Rep[A],__arg1: Rep[A])(implicit __pos: SourceContext): Rep[Boolean]
  def ordering_gt[A:Ordering:Typ](__arg0: Rep[A],__arg1: Rep[A])(implicit __pos: SourceContext): Rep[Boolean]
  def ordering_gteq[A:Ordering:Typ](__arg0: Rep[A],__arg1: Rep[A])(implicit __pos: SourceContext): Rep[Boolean]
}
