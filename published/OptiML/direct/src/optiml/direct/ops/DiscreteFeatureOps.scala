package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DiscreteFeatureOps extends Base {
  this: OptiML => 

  object DiscreteFeature {
    def apply(__arg0: Rep[String]*)(implicit __pos: SourceContext,__imp1: Overload2) = discretefeature_object_apply(__arg0)(__pos)
  }

  implicit def repToDiscreteFeatureDiscreteFeatureOpsCls(x: Rep[DiscreteFeature])(implicit __pos: SourceContext) = new DiscreteFeatureDiscreteFeatureOpsCls(x)(__pos)
  implicit def varToDiscreteFeatureDiscreteFeatureOpsCls(x: Var[DiscreteFeature])(implicit __pos: SourceContext) = new DiscreteFeatureDiscreteFeatureOpsCls(readVar(x))(__pos)

  class DiscreteFeatureDiscreteFeatureOpsCls(val self: Rep[DiscreteFeature])(implicit __pos: SourceContext) {
    def apply(__arg1: Rep[String])(implicit __pos: SourceContext,__imp1: Overload1) = discretefeature_apply(self,__arg1)(__pos)
    def indicator(__arg1: Rep[String])(implicit __pos: SourceContext) = discretefeature_indicator(self,__arg1)(__pos)
    def size(implicit __pos: SourceContext,__imp1: Overload1) = discretefeature_size(self)(__pos)
  }



  def discretefeature_object_apply(__arg0: Seq[Rep[String]])(implicit __pos: SourceContext): Rep[DiscreteFeature]
  def discretefeature_apply(self: Rep[DiscreteFeature],__arg1: Rep[String])(implicit __pos: SourceContext): Rep[Double]
  def discretefeature_indicator(self: Rep[DiscreteFeature],__arg1: Rep[String])(implicit __pos: SourceContext): Rep[DenseVector[Double]]
  def discretefeature_size(self: Rep[DiscreteFeature])(implicit __pos: SourceContext): Rep[Int]
}
trait DiscreteFeatureCompilerOps extends DiscreteFeatureOps {
  this: OptiML => 

  def discrete_feature_alloc(__arg0: Rep[ForgeHashMap[String,Int]])(implicit __pos: SourceContext): Rep[DiscreteFeature]
  def getFeatures(self: Rep[DiscreteFeature])(implicit __pos: SourceContext): Rep[ForgeHashMap[String,Int]]
}

