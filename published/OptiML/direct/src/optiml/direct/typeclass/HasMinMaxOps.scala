package optiml.direct.typeclass

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

trait HasMinMaxOps extends Base with scala.math.Numeric.ExtraImplicits {
  this: OptiML => 

  /**
   * Type class
   */
  trait HasMinMax[T] {
    def min(implicit __pos: SourceContext): Rep[T]
    def max(implicit __pos: SourceContext): Rep[T]
  }

  def HMMtype[A,B](x: HasMinMax[A]) = x.asInstanceOf[HasMinMax[B]]

  /**
   * Type class instances
   */

  implicit def canHasMinMaxLong: HasMinMax[Long] = new HasMinMax[Long] {
    def min(implicit __pos: SourceContext) = {
      unit(scala.Long.MinValue)
    }
    def max(implicit __pos: SourceContext) = {
      unit(scala.Long.MaxValue)
    }
  }

  implicit def canHasMinMaxFloat: HasMinMax[Float] = new HasMinMax[Float] {
    def min(implicit __pos: SourceContext) = {
      unit(scala.Float.MinValue)
    }
    def max(implicit __pos: SourceContext) = {
      unit(scala.Float.MaxValue)
    }
  }

  implicit def canHasMinMaxDouble: HasMinMax[Double] = new HasMinMax[Double] {
    def min(implicit __pos: SourceContext) = {
      unit(scala.Double.MinValue)
    }
    def max(implicit __pos: SourceContext) = {
      unit(scala.Double.MaxValue)
    }
  }

  implicit def canHasMinMaxInt: HasMinMax[Int] = new HasMinMax[Int] {
    def min(implicit __pos: SourceContext) = {
      unit(scala.Int.MinValue)
    }
    def max(implicit __pos: SourceContext) = {
      unit(scala.Int.MaxValue)
    }
  }


  /**
   * Forwarders - these allow infix notation to be used when the type class is available
   */
  implicit class HasMinMax2HasMinMaxOps[T](self: Rep[T])(implicit __cb0: Typ[T],__tc: HasMinMax[T]){
    def min()(implicit __pos: SourceContext) = hasminmax_min[T]()
    def max()(implicit __pos: SourceContext) = hasminmax_max[T]()
  }

  def hasminmax_min[T]()(implicit __cb0: Typ[T],__pos: SourceContext,__tc: HasMinMax[T]): Rep[T] = __tc.min
  def hasminmax_max[T]()(implicit __cb0: Typ[T],__pos: SourceContext,__tc: HasMinMax[T]): Rep[T] = __tc.max
}
