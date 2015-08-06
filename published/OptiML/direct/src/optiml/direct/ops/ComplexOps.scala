package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait ComplexOps extends Base {
  this: OptiML => 

  object Complex {
    def apply(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload33) = complex_object_apply(__arg0,__arg1)(__pos)
  }

  def __equal(self: Rep[Complex],__arg1: Rep[Complex])(implicit __pos: SourceContext,__imp1: Overload23) = complex___equal(self,__arg1)(__pos)

  implicit def repToComplexComplexOpsCls(x: Rep[Complex])(implicit __pos: SourceContext) = new ComplexComplexOpsCls(x)(__pos)
  implicit def varToComplexComplexOpsCls(x: Var[Complex])(implicit __pos: SourceContext) = new ComplexComplexOpsCls(readVar(x))(__pos)

  class ComplexComplexOpsCls(val self: Rep[Complex])(implicit __pos: SourceContext) {
    def real(implicit __pos: SourceContext) = complex_real(self)(__pos)
    def imag(implicit __pos: SourceContext) = complex_imag(self)(__pos)
    def conj(implicit __pos: SourceContext) = complex_conj(self)(__pos)
    def +(__arg1: Rep[Complex])(implicit __pos: SourceContext,__imp1: Overload209) = complex_pl(self,__arg1)(__pos)
    def -(__arg1: Rep[Complex])(implicit __pos: SourceContext,__imp1: Overload193) = complex_sub(self,__arg1)(__pos)
    def *(__arg1: Rep[Complex])(implicit __pos: SourceContext,__imp1: Overload214) = complex_mul(self,__arg1)(__pos)
    def /(__arg1: Rep[Complex])(implicit __pos: SourceContext,__imp1: Overload184) = complex_div(self,__arg1)(__pos)
    def abs(implicit __pos: SourceContext,__imp1: Overload22) = complex_abs(self)(__pos)
    def exp(implicit __pos: SourceContext,__imp1: Overload20) = complex_exp(self)(__pos)
    def log(implicit __pos: SourceContext,__imp1: Overload20) = complex_log(self)(__pos)
  }



  def complex_object_apply(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Complex]
  def complex_real(self: Rep[Complex])(implicit __pos: SourceContext): Rep[Double]
  def complex_imag(self: Rep[Complex])(implicit __pos: SourceContext): Rep[Double]
  def complex_conj(self: Rep[Complex])(implicit __pos: SourceContext): Rep[Complex]
  def complex_pl(self: Rep[Complex],__arg1: Rep[Complex])(implicit __pos: SourceContext): Rep[Complex]
  def complex_sub(self: Rep[Complex],__arg1: Rep[Complex])(implicit __pos: SourceContext): Rep[Complex]
  def complex_mul(self: Rep[Complex],__arg1: Rep[Complex])(implicit __pos: SourceContext): Rep[Complex]
  def complex_div(self: Rep[Complex],__arg1: Rep[Complex])(implicit __pos: SourceContext): Rep[Complex]
  def complex_abs(self: Rep[Complex])(implicit __pos: SourceContext): Rep[Complex]
  def complex_exp(self: Rep[Complex])(implicit __pos: SourceContext): Rep[Complex]
  def complex_log(self: Rep[Complex])(implicit __pos: SourceContext): Rep[Complex]
  def complex___equal(self: Rep[Complex],__arg1: Rep[Complex])(implicit __pos: SourceContext): Rep[Boolean]
}
