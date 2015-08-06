package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DenseTrainingSetOps extends Base {
  this: OptiML => 

  object DenseTrainingSet {
    def apply[D:Typ,L:Typ](__arg0: Rep[DenseMatrix[D]],__arg1: Rep[DenseVector[L]])(implicit __pos: SourceContext,__imp1: Overload31) = densetrainingset_object_apply[D,L](__arg0,__arg1)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
  }

  implicit def repToDenseTrainingSetDenseTrainingSetOpsCls[D:Typ,L:Typ](x: Rep[DenseTrainingSet[D,L]])(implicit __pos: SourceContext) = new DenseTrainingSetDenseTrainingSetOpsCls(x)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
  implicit def varToDenseTrainingSetDenseTrainingSetOpsCls[D:Typ,L:Typ](x: Var[DenseTrainingSet[D,L]])(implicit __pos: SourceContext) = new DenseTrainingSetDenseTrainingSetOpsCls(readVar(x))(implicitly[Typ[D]],implicitly[Typ[L]],__pos)

  class DenseTrainingSetDenseTrainingSetOpsCls[D:Typ,L:Typ](val self: Rep[DenseTrainingSet[D,L]])(implicit __pos: SourceContext) {
    def labels(implicit __pos: SourceContext,__imp1: Overload4) = densetrainingset_labels[D,L](self)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
    def data(implicit __pos: SourceContext,__imp1: Overload3) = densetrainingset_data[D,L](self)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
    def apply(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload42) = densetrainingset_apply[D,L](self,__arg1,__arg2)(implicitly[Typ[D]],implicitly[Typ[L]],__pos,overload42)
    def apply(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload43) = densetrainingset_apply[D,L](self,__arg1)(implicitly[Typ[D]],implicitly[Typ[L]],__pos,overload43)
    def numSamples(implicit __pos: SourceContext,__imp1: Overload4) = densetrainingset_numsamples[D,L](self)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
    def numFeatures(implicit __pos: SourceContext,__imp1: Overload4) = densetrainingset_numfeatures[D,L](self)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
  }



  def densetrainingset_object_apply[D:Typ,L:Typ](__arg0: Rep[DenseMatrix[D]],__arg1: Rep[DenseVector[L]])(implicit __pos: SourceContext): Rep[DenseTrainingSet[D,L]]
  def densetrainingset_labels[D:Typ,L:Typ](self: Rep[DenseTrainingSet[D,L]])(implicit __pos: SourceContext): Rep[DenseVector[L]]
  def densetrainingset_data[D:Typ,L:Typ](self: Rep[DenseTrainingSet[D,L]])(implicit __pos: SourceContext): Rep[DenseMatrix[D]]
  def densetrainingset_apply[D:Typ,L:Typ](self: Rep[DenseTrainingSet[D,L]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload42): Rep[D]
  def densetrainingset_apply[D:Typ,L:Typ](self: Rep[DenseTrainingSet[D,L]],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload43): Rep[DenseVectorView[D]]
  def densetrainingset_numsamples[D:Typ,L:Typ](self: Rep[DenseTrainingSet[D,L]])(implicit __pos: SourceContext): Rep[Int]
  def densetrainingset_numfeatures[D:Typ,L:Typ](self: Rep[DenseTrainingSet[D,L]])(implicit __pos: SourceContext): Rep[Int]
}
