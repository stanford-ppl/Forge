package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DDFGFVariableOps extends Base {
  this: OptiML => 

  object DDFGFVariable {
    def apply(__arg0: Rep[Int],__arg1: Rep[Boolean],__arg2: Rep[Double],__arg3: Rep[Int],__arg4: Rep[Int],__arg5: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload15) = ddfgfvariable_object_apply(__arg0,__arg1,__arg2,__arg3,__arg4,__arg5)(__pos)
  }

  implicit def repToDDFGFVariableDDFGFVariableOpsCls(x: Rep[DDFGFVariable])(implicit __pos: SourceContext) = new DDFGFVariableDDFGFVariableOpsCls(x)(__pos)
  implicit def varToDDFGFVariableDDFGFVariableOpsCls(x: Var[DDFGFVariable])(implicit __pos: SourceContext) = new DDFGFVariableDDFGFVariableOpsCls(readVar(x))(__pos)

  class DDFGFVariableDDFGFVariableOpsCls(val self: Rep[DDFGFVariable])(implicit __pos: SourceContext) {
    def variableId(implicit __pos: SourceContext,__imp1: Overload2) = ddfgfvariable_variableid(self)(__pos)
    def isEvidence(implicit __pos: SourceContext) = ddfgfvariable_isevidence(self)(__pos)
    def initialValue(implicit __pos: SourceContext,__imp1: Overload2) = ddfgfvariable_initialvalue(self)(__pos)
    def dataType(implicit __pos: SourceContext) = ddfgfvariable_datatype(self)(__pos)
    def edgeCount(implicit __pos: SourceContext,__imp1: Overload1) = ddfgfvariable_edgecount(self)(__pos)
    def cardinality(implicit __pos: SourceContext) = ddfgfvariable_cardinality(self)(__pos)
  }



  def ddfgfvariable_object_apply(__arg0: Rep[Int],__arg1: Rep[Boolean],__arg2: Rep[Double],__arg3: Rep[Int],__arg4: Rep[Int],__arg5: Rep[Int])(implicit __pos: SourceContext): Rep[DDFGFVariable]
  def ddfgfvariable_variableid(self: Rep[DDFGFVariable])(implicit __pos: SourceContext): Rep[Int]
  def ddfgfvariable_isevidence(self: Rep[DDFGFVariable])(implicit __pos: SourceContext): Rep[Boolean]
  def ddfgfvariable_initialvalue(self: Rep[DDFGFVariable])(implicit __pos: SourceContext): Rep[Double]
  def ddfgfvariable_datatype(self: Rep[DDFGFVariable])(implicit __pos: SourceContext): Rep[Int]
  def ddfgfvariable_edgecount(self: Rep[DDFGFVariable])(implicit __pos: SourceContext): Rep[Int]
  def ddfgfvariable_cardinality(self: Rep[DDFGFVariable])(implicit __pos: SourceContext): Rep[Int]
}
