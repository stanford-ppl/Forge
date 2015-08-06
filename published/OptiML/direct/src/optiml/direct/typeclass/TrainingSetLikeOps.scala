package optiml.direct.typeclass

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

trait TrainingSetLikeOps extends Base with scala.math.Numeric.ExtraImplicits {
  this: OptiML => 

  /**
   * Type class
   */
  trait TrainingSetLike[D,L,TS[D,L]] {
    def labels(__arg0: Rep[TS[D,L]])(implicit __pos: SourceContext): Rep[DenseVector[L]]
    def numSamples(__arg0: Rep[TS[D,L]])(implicit __pos: SourceContext): Rep[Int]
    def numFeatures(__arg0: Rep[TS[D,L]])(implicit __pos: SourceContext): Rep[Int]
    def getRows(__arg0: Rep[TS[D,L]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext): Rep[TS[D,L]]
    def getCols(__arg0: Rep[TS[D,L]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext): Rep[TS[D,L]]
    def dot(__arg0: Rep[TS[D,L]],__arg1: Rep[Int],__arg2: Rep[DenseVector[D]])(implicit __pos: SourceContext,__imp0: Arith[D]): Rep[D]
    def times(__arg0: Rep[TS[D,L]],__arg1: Rep[Int],__arg2: Rep[DenseVector[D]])(implicit __pos: SourceContext,__imp0: Arith[D]): Rep[DenseVector[D]]
    def timesScalar(__arg0: Rep[TS[D,L]],__arg1: Rep[Int],__arg2: Rep[D])(implicit __pos: SourceContext,__imp0: Arith[D]): Rep[DenseVector[D]]
  }


  /**
   * Type class instances
   */

  implicit def canTrainingSetLikeSparseTrainingSet[D:Typ,L:Typ]: TrainingSetLike[D,L,SparseTrainingSet] = new TrainingSetLike[D,L,SparseTrainingSet] {
    def labels(__arg0: Rep[SparseTrainingSet[D,L]])(implicit __pos: SourceContext) = {
      __arg0.labels
    }
    def numSamples(__arg0: Rep[SparseTrainingSet[D,L]])(implicit __pos: SourceContext) = {
      __arg0.numSamples
    }
    def numFeatures(__arg0: Rep[SparseTrainingSet[D,L]])(implicit __pos: SourceContext) = {
      __arg0.numFeatures
    }
    def getRows(__arg0: Rep[SparseTrainingSet[D,L]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext) = {
      SparseTrainingSet(__arg0.data.apply(__arg1), __arg0.labels.apply(__arg1))
    }
    def getCols(__arg0: Rep[SparseTrainingSet[D,L]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext) = {
      SparseTrainingSet(__arg0.data.getCols(__arg1), __arg0.labels)
    }
    def dot(__arg0: Rep[SparseTrainingSet[D,L]],__arg1: Rep[Int],__arg2: Rep[DenseVector[D]])(implicit __pos: SourceContext,__imp0: Arith[D]) = {
      __arg0(__arg1) *:* __arg2
    }
    def times(__arg0: Rep[SparseTrainingSet[D,L]],__arg1: Rep[Int],__arg2: Rep[DenseVector[D]])(implicit __pos: SourceContext,__imp0: Arith[D]) = {
      (__arg0(__arg1)*__arg2).toDense
    }
    def timesScalar(__arg0: Rep[SparseTrainingSet[D,L]],__arg1: Rep[Int],__arg2: Rep[D])(implicit __pos: SourceContext,__imp0: Arith[D]) = {
      (__arg0(__arg1)*__arg2).toDense
    }
  }

  implicit def canTrainingSetLikeDenseTrainingSet[D:Typ,L:Typ]: TrainingSetLike[D,L,DenseTrainingSet] = new TrainingSetLike[D,L,DenseTrainingSet] {
    def labels(__arg0: Rep[DenseTrainingSet[D,L]])(implicit __pos: SourceContext) = {
      __arg0.labels
    }
    def numSamples(__arg0: Rep[DenseTrainingSet[D,L]])(implicit __pos: SourceContext) = {
      __arg0.numSamples
    }
    def numFeatures(__arg0: Rep[DenseTrainingSet[D,L]])(implicit __pos: SourceContext) = {
      __arg0.numFeatures
    }
    def getRows(__arg0: Rep[DenseTrainingSet[D,L]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext) = {
      DenseTrainingSet(__arg0.data.apply(__arg1), __arg0.labels.apply(__arg1))
    }
    def getCols(__arg0: Rep[DenseTrainingSet[D,L]],__arg1: Rep[IndexVector])(implicit __pos: SourceContext) = {
      DenseTrainingSet(__arg0.data.getCols(__arg1), __arg0.labels)
    }
    def dot(__arg0: Rep[DenseTrainingSet[D,L]],__arg1: Rep[Int],__arg2: Rep[DenseVector[D]])(implicit __pos: SourceContext,__imp0: Arith[D]) = {
      __arg0(__arg1) *:* __arg2
    }
    def times(__arg0: Rep[DenseTrainingSet[D,L]],__arg1: Rep[Int],__arg2: Rep[DenseVector[D]])(implicit __pos: SourceContext,__imp0: Arith[D]) = {
      __arg0(__arg1) * __arg2
    }
    def timesScalar(__arg0: Rep[DenseTrainingSet[D,L]],__arg1: Rep[Int],__arg2: Rep[D])(implicit __pos: SourceContext,__imp0: Arith[D]) = {
      __arg0(__arg1) * __arg2
    }
  }


  /**
   * Forwarders - these allow infix notation to be used when the type class is available
   */
  class TrainingSetLike2TrainingSetLikeOps[D,L,TS[D,L]](self: Rep[TS[D,L]])(implicit __cb0: Typ[D],__cb1: Typ[L],__cb2: Typ[TS[D,L]],__tc: TrainingSetLike[D,L,TS]){
//    def labels()(implicit __pos: SourceContext) = trainingsetlike_labels[D,L,TS](self)
//    def numSamples()(implicit __pos: SourceContext) = trainingsetlike_numsamples[D,L,TS](self)
//    def numFeatures()(implicit __pos: SourceContext) = trainingsetlike_numfeatures[D,L,TS](self)
//    def getRows(__arg1: Rep[IndexVector])(implicit __pos: SourceContext) = trainingsetlike_getrows[D,L,TS](self,__arg1)
//    def getCols(__arg1: Rep[IndexVector])(implicit __pos: SourceContext) = trainingsetlike_getcols[D,L,TS](self,__arg1)
//    def dot(__arg1: Rep[Int],__arg2: Rep[DenseVector[D]])(implicit __pos: SourceContext,__imp0: Arith[D]) = trainingsetlike_dot[D,L,TS](self,__arg1,__arg2)
//    def times(__arg1: Rep[Int],__arg2: Rep[DenseVector[D]])(implicit __pos: SourceContext,__imp0: Arith[D]) = trainingsetlike_times[D,L,TS](self,__arg1,__arg2)
//    def timesScalar(__arg1: Rep[Int],__arg2: Rep[D])(implicit __pos: SourceContext,__imp0: Arith[D]) = trainingsetlike_timesscalar[D,L,TS](self,__arg1,__arg2)
  }

//TR weird compiler crashes???
//  def trainingsetlike_labels[D,L,TS[D,L]](__arg0: Rep[TS[D,L]])(implicit __cb0: Typ[D],__cb1: Typ[L],__pos: SourceContext,__tc: TrainingSetLike[D,L,TS],__cb_hk_0: Typ[TS[D,L]]): Rep[DenseVector[L]] = __tc.labels(__arg0)
//  def trainingsetlike_numsamples[D,L,TS[D,L]](__arg0: Rep[TS[D,L]])(implicit __cb0: Typ[D],__cb1: Typ[L],__pos: SourceContext,__tc: TrainingSetLike[D,L,TS],__cb_hk_0: Typ[TS[D,L]]): Rep[Int] = __tc.numSamples(__arg0)
//  def trainingsetlike_numfeatures[D,L,TS[D,L]](__arg0: Rep[TS[D,L]])(implicit __cb0: Typ[D],__cb1: Typ[L],__pos: SourceContext,__tc: TrainingSetLike[D,L,TS],__cb_hk_0: Typ[TS[D,L]]): Rep[Int] = __tc.numFeatures(__arg0)
//  def trainingsetlike_getrows[D,L,TS[D,L]](__arg0: Rep[TS[D,L]],__arg1: Rep[IndexVector])(implicit __cb0: Typ[D],__cb1: Typ[L],__pos: SourceContext,__tc: TrainingSetLike[D,L,TS],__cb_hk_0: Typ[TS[D,L]]): Rep[TS[D,L]] = __tc.getRows(__arg0,__arg1)
//  def trainingsetlike_getcols[D,L,TS[D,L]](__arg0: Rep[TS[D,L]],__arg1: Rep[IndexVector])(implicit __cb0: Typ[D],__cb1: Typ[L],__pos: SourceContext,__tc: TrainingSetLike[D,L,TS],__cb_hk_0: Typ[TS[D,L]]): Rep[TS[D,L]] = __tc.getCols(__arg0,__arg1)
//  def trainingsetlike_dot[D,L,TS[D,L]](__arg0: Rep[TS[D,L]],__arg1: Rep[Int],__arg2: Rep[DenseVector[D]])(implicit __cb0: Typ[D],__cb1: Typ[L],__pos: SourceContext,__imp0: Arith[D],__tc: TrainingSetLike[D,L,TS],__cb_hk_0: Typ[TS[D,L]]): Rep[D] = __tc.dot(__arg0,__arg1,__arg2)
//  def trainingsetlike_times[D,L,TS[D,L]](__arg0: Rep[TS[D,L]],__arg1: Rep[Int],__arg2: Rep[DenseVector[D]])(implicit __cb0: Typ[D],__cb1: Typ[L],__pos: SourceContext,__imp0: Arith[D],__tc: TrainingSetLike[D,L,TS],__cb_hk_0: Typ[TS[D,L]]): Rep[DenseVector[D]] = __tc.times(__arg0,__arg1,__arg2)
//  def trainingsetlike_timesscalar[D,L,TS[D,L]](__arg0: Rep[TS[D,L]],__arg1: Rep[Int],__arg2: Rep[D])(implicit __cb0: Typ[D],__cb1: Typ[L],__pos: SourceContext,__imp0: Arith[D],__tc: TrainingSetLike[D,L,TS],__cb_hk_0: Typ[TS[D,L]]): Rep[DenseVector[D]] = __tc.timesScalar(__arg0,__arg1,__arg2)
}
