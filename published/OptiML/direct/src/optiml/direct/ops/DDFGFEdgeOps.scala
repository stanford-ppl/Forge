package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DDFGFEdgeOps extends Base {
  this: OptiML => 

  object DDFGFEdge {
    def apply(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[Boolean],__arg4: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload14) = ddfgfedge_object_apply(__arg0,__arg1,__arg2,__arg3,__arg4)(__pos)
  }

  implicit def repToDDFGFEdgeDDFGFEdgeOpsCls(x: Rep[DDFGFEdge])(implicit __pos: SourceContext) = new DDFGFEdgeDDFGFEdgeOpsCls(x)(__pos)
  implicit def varToDDFGFEdgeDDFGFEdgeOpsCls(x: Var[DDFGFEdge])(implicit __pos: SourceContext) = new DDFGFEdgeDDFGFEdgeOpsCls(readVar(x))(__pos)

  class DDFGFEdgeDDFGFEdgeOpsCls(val self: Rep[DDFGFEdge])(implicit __pos: SourceContext) {
    def variableId(implicit __pos: SourceContext,__imp1: Overload1) = ddfgfedge_variableid(self)(__pos)
    def factorId(implicit __pos: SourceContext,__imp1: Overload1) = ddfgfedge_factorid(self)(__pos)
    def position(implicit __pos: SourceContext) = ddfgfedge_position(self)(__pos)
    def isPositive(implicit __pos: SourceContext) = ddfgfedge_ispositive(self)(__pos)
    def equalPredicate(implicit __pos: SourceContext) = ddfgfedge_equalpredicate(self)(__pos)
  }



  def ddfgfedge_object_apply(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[Boolean],__arg4: Rep[Int])(implicit __pos: SourceContext): Rep[DDFGFEdge]
  def ddfgfedge_variableid(self: Rep[DDFGFEdge])(implicit __pos: SourceContext): Rep[Int]
  def ddfgfedge_factorid(self: Rep[DDFGFEdge])(implicit __pos: SourceContext): Rep[Int]
  def ddfgfedge_position(self: Rep[DDFGFEdge])(implicit __pos: SourceContext): Rep[Int]
  def ddfgfedge_ispositive(self: Rep[DDFGFEdge])(implicit __pos: SourceContext): Rep[Boolean]
  def ddfgfedge_equalpredicate(self: Rep[DDFGFEdge])(implicit __pos: SourceContext): Rep[Int]
}
