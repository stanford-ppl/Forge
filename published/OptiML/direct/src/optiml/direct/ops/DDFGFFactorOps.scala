package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DDFGFFactorOps extends Base {
  this: OptiML => 

  object DDFGFFactor {
    def apply(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload32) = ddfgffactor_object_apply(__arg0,__arg1,__arg2,__arg3)(__pos)
  }

  implicit def repToDDFGFFactorDDFGFFactorOpsCls(x: Rep[DDFGFFactor])(implicit __pos: SourceContext) = new DDFGFFactorDDFGFFactorOpsCls(x)(__pos)
  implicit def varToDDFGFFactorDDFGFFactorOpsCls(x: Var[DDFGFFactor])(implicit __pos: SourceContext) = new DDFGFFactorDDFGFFactorOpsCls(readVar(x))(__pos)

  class DDFGFFactorDDFGFFactorOpsCls(val self: Rep[DDFGFFactor])(implicit __pos: SourceContext) {
    def factorId(implicit __pos: SourceContext,__imp1: Overload2) = ddfgffactor_factorid(self)(__pos)
    def weightId(implicit __pos: SourceContext,__imp1: Overload2) = ddfgffactor_weightid(self)(__pos)
    def factorFunction(implicit __pos: SourceContext,__imp1: Overload2) = ddfgffactor_factorfunction(self)(__pos)
    def edgeCount(implicit __pos: SourceContext,__imp1: Overload2) = ddfgffactor_edgecount(self)(__pos)
  }



  def ddfgffactor_object_apply(__arg0: Rep[Int],__arg1: Rep[Int],__arg2: Rep[Int],__arg3: Rep[Int])(implicit __pos: SourceContext): Rep[DDFGFFactor]
  def ddfgffactor_factorid(self: Rep[DDFGFFactor])(implicit __pos: SourceContext): Rep[Int]
  def ddfgffactor_weightid(self: Rep[DDFGFFactor])(implicit __pos: SourceContext): Rep[Int]
  def ddfgffactor_factorfunction(self: Rep[DDFGFFactor])(implicit __pos: SourceContext): Rep[Int]
  def ddfgffactor_edgecount(self: Rep[DDFGFFactor])(implicit __pos: SourceContext): Rep[Int]
}
