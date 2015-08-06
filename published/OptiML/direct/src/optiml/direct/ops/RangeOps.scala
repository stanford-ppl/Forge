package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait RangeOps extends Base {
  this: OptiML => 


}
trait RangeCompilerOps extends RangeOps {
  this: OptiML => 

  def range_start(__arg0: Rep[Range])(implicit __pos: SourceContext): Rep[Int]
  def range_end(__arg0: Rep[Range])(implicit __pos: SourceContext): Rep[Int]
  def infix_until(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Range]
  def infix_foreach(__arg0: Rep[Range],__arg1: (Rep[Int]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
  def range_foreach(start: Rep[Int],end: Rep[Int],func: (Rep[Int]) => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit]
}

