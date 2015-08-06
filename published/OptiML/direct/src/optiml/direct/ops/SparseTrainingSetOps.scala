package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait SparseTrainingSetOps extends Base {
  this: OptiML => 

  object SparseTrainingSet {
    def apply[D:Typ,L:Typ](__arg0: Rep[SparseMatrix[D]],__arg1: Rep[DenseVector[L]])(implicit __pos: SourceContext,__imp1: Overload22) = sparsetrainingset_object_apply[D,L](__arg0,__arg1)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
  }

  implicit def repToSparseTrainingSetSparseTrainingSetOpsCls[D:Typ,L:Typ](x: Rep[SparseTrainingSet[D,L]])(implicit __pos: SourceContext) = new SparseTrainingSetSparseTrainingSetOpsCls(x)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
  implicit def varToSparseTrainingSetSparseTrainingSetOpsCls[D:Typ,L:Typ](x: Var[SparseTrainingSet[D,L]])(implicit __pos: SourceContext) = new SparseTrainingSetSparseTrainingSetOpsCls(readVar(x))(implicitly[Typ[D]],implicitly[Typ[L]],__pos)

  class SparseTrainingSetSparseTrainingSetOpsCls[D:Typ,L:Typ](val self: Rep[SparseTrainingSet[D,L]])(implicit __pos: SourceContext) {
    def labels(implicit __pos: SourceContext,__imp1: Overload3) = sparsetrainingset_labels[D,L](self)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
    def data(implicit __pos: SourceContext,__imp1: Overload2) = sparsetrainingset_data[D,L](self)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
    def apply(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload24) = sparsetrainingset_apply[D,L](self,__arg1,__arg2)(implicitly[Typ[D]],implicitly[Typ[L]],__pos,overload24)
    def apply(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload25) = sparsetrainingset_apply[D,L](self,__arg1)(implicitly[Typ[D]],implicitly[Typ[L]],__pos,overload25)
    def numSamples(implicit __pos: SourceContext,__imp1: Overload3) = sparsetrainingset_numsamples[D,L](self)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
    def numFeatures(implicit __pos: SourceContext,__imp1: Overload3) = sparsetrainingset_numfeatures[D,L](self)(implicitly[Typ[D]],implicitly[Typ[L]],__pos)
  }



  def sparsetrainingset_object_apply[D:Typ,L:Typ](__arg0: Rep[SparseMatrix[D]],__arg1: Rep[DenseVector[L]])(implicit __pos: SourceContext): Rep[SparseTrainingSet[D,L]]
  def sparsetrainingset_labels[D:Typ,L:Typ](self: Rep[SparseTrainingSet[D,L]])(implicit __pos: SourceContext): Rep[DenseVector[L]]
  def sparsetrainingset_data[D:Typ,L:Typ](self: Rep[SparseTrainingSet[D,L]])(implicit __pos: SourceContext): Rep[SparseMatrix[D]]
  def sparsetrainingset_apply[D:Typ,L:Typ](self: Rep[SparseTrainingSet[D,L]],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload24): Rep[D]
  def sparsetrainingset_apply[D:Typ,L:Typ](self: Rep[SparseTrainingSet[D,L]],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload25): Rep[SparseVectorView[D]]
  def sparsetrainingset_numsamples[D:Typ,L:Typ](self: Rep[SparseTrainingSet[D,L]])(implicit __pos: SourceContext): Rep[Int]
  def sparsetrainingset_numfeatures[D:Typ,L:Typ](self: Rep[SparseTrainingSet[D,L]])(implicit __pos: SourceContext): Rep[Int]
}
