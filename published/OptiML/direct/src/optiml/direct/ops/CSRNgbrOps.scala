package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait CSRNgbrOps extends Base {
  this: OptiML => 

  object CSRNgbr {
    def apply(edgeId: Rep[Int],nodeId: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload11) = csrngbr_object_apply(edgeId,nodeId)(__pos)
  }

  implicit def repToCSRNgbrCSRNgbrOpsCls(x: Rep[CSRNgbr])(implicit __pos: SourceContext) = new CSRNgbrCSRNgbrOpsCls(x)(__pos)
  implicit def varToCSRNgbrCSRNgbrOpsCls(x: Var[CSRNgbr])(implicit __pos: SourceContext) = new CSRNgbrCSRNgbrOpsCls(readVar(x))(__pos)

  class CSRNgbrCSRNgbrOpsCls(val self: Rep[CSRNgbr])(implicit __pos: SourceContext) {
    def edge(implicit __pos: SourceContext) = csrngbr_edge(self)(__pos)
    def node(implicit __pos: SourceContext) = csrngbr_node(self)(__pos)
  }



  def csrngbr_object_apply(edgeId: Rep[Int],nodeId: Rep[Int])(implicit __pos: SourceContext): Rep[CSRNgbr]
  def csrngbr_edge(self: Rep[CSRNgbr])(implicit __pos: SourceContext): Rep[Int]
  def csrngbr_node(self: Rep[CSRNgbr])(implicit __pos: SourceContext): Rep[Int]
}
