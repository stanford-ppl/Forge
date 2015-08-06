package optiml.direct.typeclass

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

trait ShapeOps extends Base with scala.math.Numeric.ExtraImplicits {
  this: OptiML => 

  /**
   * Type class
   */
  trait Shape[S] {
    def contains(__arg0: Rep[S],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext): Rep[Boolean]
    def apply(__arg0: Rep[S],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Tup2[Int,Int]]
    def size(__arg0: Rep[S])(implicit __pos: SourceContext): Rep[Int]
  }

  def shtype[A,B](x: Shape[A]) = x.asInstanceOf[Shape[B]]

  /**
   * Type class instances
   */

  implicit def canShapeUTri: Shape[UTriangle] = new Shape[UTriangle] {
    def contains(__arg0: Rep[UTriangle],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext) = {
      __arg0.contains(__arg1,__arg2)
    }
    def apply(__arg0: Rep[UTriangle],__arg1: Rep[Int])(implicit __pos: SourceContext) = {
      __arg0.apply(__arg1)
    }
    def size(__arg0: Rep[UTriangle])(implicit __pos: SourceContext) = {
      __arg0.size
    }
  }


  /**
   * Forwarders - these allow infix notation to be used when the type class is available
   */
  implicit class Shape2ShapeOps[S](self: Rep[S])(implicit __cb0: Typ[S],__tc: Shape[S]){
    def contains(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext) = shape_contains[S](self,__arg1,__arg2)
    def apply(__arg1: Rep[Int])(implicit __pos: SourceContext) = shape_apply[S](self,__arg1)
    def size()(implicit __pos: SourceContext) = shape_size[S](self)
  }

  def shape_contains[S](__arg0: Rep[S],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __cb0: Typ[S],__pos: SourceContext,__tc: Shape[S]): Rep[Boolean] = __tc.contains(__arg0,__arg1,__arg2)
  def shape_apply[S](__arg0: Rep[S],__arg1: Rep[Int])(implicit __cb0: Typ[S],__pos: SourceContext,__tc: Shape[S]): Rep[Tup2[Int,Int]] = __tc.apply(__arg0,__arg1)
  def shape_size[S](__arg0: Rep[S])(implicit __cb0: Typ[S],__pos: SourceContext,__tc: Shape[S]): Rep[Int] = __tc.size(__arg0)
}
