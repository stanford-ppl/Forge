package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait CastOps extends Base {
  this: OptiML => 

  implicit def repToCastAOpsCls[A:Typ](x: Rep[A])(implicit __pos: SourceContext) = new CastAOpsCls(x)(implicitly[Typ[A]],__pos)
  implicit def varToCastAOpsCls[A:Typ](x: Var[A])(implicit __pos: SourceContext) = new CastAOpsCls(readVar(x))(implicitly[Typ[A]],__pos)

  class CastAOpsCls[A:Typ](val self: Rep[A])(implicit __pos: SourceContext) {
    def AsInstanceOf[B:Typ](implicit __pos: SourceContext) = cast_asinstanceof[A,B](self)(implicitly[Typ[A]],implicitly[Typ[B]],__pos)
    def IsInstanceOf[B:Typ](implicit __pos: SourceContext) = cast_isinstanceof[A,B](self)(implicitly[Typ[A]],implicitly[Typ[B]],__pos)
  }



  def cast_asinstanceof[A:Typ,B:Typ](__arg0: Rep[A])(implicit __pos: SourceContext): Rep[B]
  def cast_isinstanceof[A:Typ,B:Typ](__arg0: Rep[A])(implicit __pos: SourceContext): Rep[Boolean]
}
