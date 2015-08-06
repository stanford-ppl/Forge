package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DDFGFWeightOps extends Base {
  this: OptiML => 

  object DDFGFWeight {
    def apply(__arg0: Rep[Int],__arg1: Rep[Boolean],__arg2: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload1) = ddfgfweight_object_apply(__arg0,__arg1,__arg2)(__pos)
  }

  implicit def repToDDFGFWeightDDFGFWeightOpsCls(x: Rep[DDFGFWeight])(implicit __pos: SourceContext) = new DDFGFWeightDDFGFWeightOpsCls(x)(__pos)
  implicit def varToDDFGFWeightDDFGFWeightOpsCls(x: Var[DDFGFWeight])(implicit __pos: SourceContext) = new DDFGFWeightDDFGFWeightOpsCls(readVar(x))(__pos)

  class DDFGFWeightDDFGFWeightOpsCls(val self: Rep[DDFGFWeight])(implicit __pos: SourceContext) {
    def weightId(implicit __pos: SourceContext,__imp1: Overload1) = ddfgfweight_weightid(self)(__pos)
    def isFixed(implicit __pos: SourceContext) = ddfgfweight_isfixed(self)(__pos)
    def initialValue(implicit __pos: SourceContext,__imp1: Overload1) = ddfgfweight_initialvalue(self)(__pos)
  }



  def ddfgfweight_object_apply(__arg0: Rep[Int],__arg1: Rep[Boolean],__arg2: Rep[Double])(implicit __pos: SourceContext): Rep[DDFGFWeight]
  def ddfgfweight_weightid(self: Rep[DDFGFWeight])(implicit __pos: SourceContext): Rep[Int]
  def ddfgfweight_isfixed(self: Rep[DDFGFWeight])(implicit __pos: SourceContext): Rep[Boolean]
  def ddfgfweight_initialvalue(self: Rep[DDFGFWeight])(implicit __pos: SourceContext): Rep[Double]
}
