package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait MathOps extends Base {
  this: OptiML => 

  object Math {
    def bitcount(__arg0: Rep[Long])(implicit __pos: SourceContext) = math_object_bitcount(__arg0)(__pos)
    def abs(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_abs(__arg0)(__pos)
    def exp(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_exp(__arg0)(__pos)
    def log(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_log(__arg0)(__pos)
    def log10(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_log10(__arg0)(__pos)
    def sqrt(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_sqrt(__arg0)(__pos)
    def ceil(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_ceil(__arg0)(__pos)
    def floor(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_floor(__arg0)(__pos)
    def round(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_round(__arg0)(__pos)
    def sin(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_sin(__arg0)(__pos)
    def sinh(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_sinh(__arg0)(__pos)
    def asin(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_asin(__arg0)(__pos)
    def cos(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_cos(__arg0)(__pos)
    def cosh(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_cosh(__arg0)(__pos)
    def acos(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_acos(__arg0)(__pos)
    def tan(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_tan(__arg0)(__pos)
    def tanh(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_tanh(__arg0)(__pos)
    def atan(__arg0: Rep[Double])(implicit __pos: SourceContext) = math_object_atan(__arg0)(__pos)
    def atan2(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = math_object_atan2(__arg0,__arg1)(__pos)
    def pow(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = math_object_pow(__arg0,__arg1)(__pos)
    def max(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = math_object_max(__arg0,__arg1)(__pos)
    def min(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = math_object_min(__arg0,__arg1)(__pos)
  }

  def INF(implicit __pos: SourceContext) = math_inf()(__pos)
  def nINF(implicit __pos: SourceContext) = math_ninf()(__pos)
  def Pi(implicit __pos: SourceContext): Rep[Double] = { unit(java.lang.Math.PI) }
  def E(implicit __pos: SourceContext): Rep[Double] = { unit(java.lang.Math.E) }


  def math_inf()(implicit __pos: SourceContext): Rep[Double]
  def math_ninf()(implicit __pos: SourceContext): Rep[Double]
  def math_object_bitcount(__arg0: Rep[Long])(implicit __pos: SourceContext): Rep[Int]
  def math_object_abs(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_exp(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_log(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_log10(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_sqrt(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_ceil(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_floor(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_round(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Long]
  def math_object_sin(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_sinh(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_asin(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_cos(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_cosh(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_acos(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_tan(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_tanh(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_atan(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_atan2(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_pow(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_max(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def math_object_min(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
}
