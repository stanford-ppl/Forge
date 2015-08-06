package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait BinaryFeatureOps extends Base {
  this: OptiML => 

  object BinaryFeature {
    def apply(default: Rep[Boolean] = unit(false))(implicit __pos: SourceContext,__imp1: Overload37) = binaryfeature_object_apply(default)(__pos)
  }

  implicit def repToBinaryFeatureBinaryFeatureOpsCls(x: Rep[BinaryFeature])(implicit __pos: SourceContext) = new BinaryFeatureBinaryFeatureOpsCls(x)(__pos)
  implicit def varToBinaryFeatureBinaryFeatureOpsCls(x: Var[BinaryFeature])(implicit __pos: SourceContext) = new BinaryFeatureBinaryFeatureOpsCls(readVar(x))(__pos)

  class BinaryFeatureBinaryFeatureOpsCls(val self: Rep[BinaryFeature])(implicit __pos: SourceContext) {
    def default(implicit __pos: SourceContext,__imp1: Overload2) = binaryfeature_default(self)(__pos)
    def apply(__arg1: Rep[String])(implicit __pos: SourceContext,__imp1: Overload46) = binaryfeature_apply(self,__arg1)(__pos)
  }



  def binaryfeature_object_apply(default: Rep[Boolean] = unit(false))(implicit __pos: SourceContext): Rep[BinaryFeature]
  def binaryfeature_default(self: Rep[BinaryFeature])(implicit __pos: SourceContext): Rep[Boolean]
  def binaryfeature_apply(self: Rep[BinaryFeature],__arg1: Rep[String])(implicit __pos: SourceContext): Rep[Double]
}
