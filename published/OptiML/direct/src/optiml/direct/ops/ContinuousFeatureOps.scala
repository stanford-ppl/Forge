package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait ContinuousFeatureOps extends Base {
  this: OptiML => 

  object ContinuousFeature {
    def apply(default: Rep[Double] = unit(0.0209),min: Rep[Double] = math_ninf(),max: Rep[Double] = math_inf())(implicit __pos: SourceContext,__imp1: Overload18) = continuousfeature_object_apply(default,min,max)(__pos)
  }

  implicit def repToContinuousFeatureContinuousFeatureOpsCls(x: Rep[ContinuousFeature])(implicit __pos: SourceContext) = new ContinuousFeatureContinuousFeatureOpsCls(x)(__pos)
  implicit def varToContinuousFeatureContinuousFeatureOpsCls(x: Var[ContinuousFeature])(implicit __pos: SourceContext) = new ContinuousFeatureContinuousFeatureOpsCls(readVar(x))(__pos)

  class ContinuousFeatureContinuousFeatureOpsCls(val self: Rep[ContinuousFeature])(implicit __pos: SourceContext) {
    def default(implicit __pos: SourceContext,__imp1: Overload1) = continuousfeature_default(self)(__pos)
    def min(implicit __pos: SourceContext,__imp1: Overload7) = continuousfeature_min(self)(__pos)
    def max(implicit __pos: SourceContext,__imp1: Overload7) = continuousfeature_max(self)(__pos)
    def apply(__arg1: Rep[String])(implicit __pos: SourceContext,__imp1: Overload19) = continuousfeature_apply(self,__arg1)(__pos)
  }



  def continuousfeature_object_apply(default: Rep[Double] = unit(0.0209),min: Rep[Double] = math_ninf(),max: Rep[Double] = math_inf())(implicit __pos: SourceContext): Rep[ContinuousFeature]
  def continuousfeature_default(self: Rep[ContinuousFeature])(implicit __pos: SourceContext): Rep[Double]
  def continuousfeature_min(self: Rep[ContinuousFeature])(implicit __pos: SourceContext): Rep[Double]
  def continuousfeature_max(self: Rep[ContinuousFeature])(implicit __pos: SourceContext): Rep[Double]
  def continuousfeature_apply(self: Rep[ContinuousFeature],__arg1: Rep[String])(implicit __pos: SourceContext): Rep[Double]
}
