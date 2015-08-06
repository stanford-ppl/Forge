package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait ClassifierOps extends Base {
  this: OptiML => 

  def rforest(trainingSet: Rep[DenseTrainingSet[Double,Boolean]],numTrees: Rep[Int] = unit(10),samplingRate: Rep[Double] = unit(0.66),maxDepth: Rep[Int] = unit(-1),maxNumFeatures: Rep[Int] = unit(-1),minSamplesSplit: Rep[Int] = unit(2),minSamplesLeaf: Rep[Int] = unit(1),verbose: Rep[Boolean] = unit(false))(implicit __pos: SourceContext) = classifier_rforest(trainingSet,numTrees,samplingRate,maxDepth,maxNumFeatures,minSamplesSplit,minSamplesLeaf,verbose)(__pos)
  def logreg[TS[D,L]](data: Rep[TS[Double,Boolean]],stochastic: Rep[Boolean] = unit(true),initLearningRate: Rep[Double] = unit(1.0),maxIter: Rep[Int] = unit(30),lambda: Rep[Double] = unit(0.0),verbose: Rep[Boolean] = unit(false),callback: (Rep[DenseVector[Double]],Rep[Int]) => Rep[Unit] = (m,i) => unit(()))(implicit __pos: SourceContext,_ts: TrainingSetLike[Double,Boolean,TS],__cb_hk_0: Typ[TS[Double,Boolean]]) = classifier_logreg[TS](data,stochastic,initLearningRate,maxIter,lambda,verbose,callback)(__pos,_ts,__cb_hk_0)
  def sparseLogreg(data: Rep[SparseTrainingSet[Double,Boolean]],stochastic: Rep[Boolean] = unit(true),initLearningRate: Rep[Double] = unit(1.0),maxIter: Rep[Int] = unit(30),lambda: Rep[Double] = unit(0.0),verbose: Rep[Boolean] = unit(false),callback: (Rep[SparseVector[Double]],Rep[Int]) => Rep[Unit] = (m,i) => unit(()))(implicit __pos: SourceContext) = classifier_sparselogreg(data,stochastic,initLearningRate,maxIter,lambda,verbose,callback)(__pos)


  def classifier_rforest(trainingSet: Rep[DenseTrainingSet[Double,Boolean]],numTrees: Rep[Int] = unit(10),samplingRate: Rep[Double] = unit(0.66),maxDepth: Rep[Int] = unit(-1),maxNumFeatures: Rep[Int] = unit(-1),minSamplesSplit: Rep[Int] = unit(2),minSamplesLeaf: Rep[Int] = unit(1),verbose: Rep[Boolean] = unit(false))(implicit __pos: SourceContext): Rep[RandomForest]
  def classifier_logreg[TS[D,L]](data: Rep[TS[Double,Boolean]],stochastic: Rep[Boolean] = unit(true),initLearningRate: Rep[Double] = unit(1.0),maxIter: Rep[Int] = unit(30),lambda: Rep[Double] = unit(0.0),verbose: Rep[Boolean] = unit(false),callback: (Rep[DenseVector[Double]],Rep[Int]) => Rep[Unit] = (m,i) => unit(()))(implicit __pos: SourceContext,_ts: TrainingSetLike[Double,Boolean,TS],__cb_hk_0: Typ[TS[Double,Boolean]]): Rep[DenseVector[Double]]
  def classifier_sparselogreg(data: Rep[SparseTrainingSet[Double,Boolean]],stochastic: Rep[Boolean] = unit(true),initLearningRate: Rep[Double] = unit(1.0),maxIter: Rep[Int] = unit(30),lambda: Rep[Double] = unit(0.0),verbose: Rep[Boolean] = unit(false),callback: (Rep[SparseVector[Double]],Rep[Int]) => Rep[Unit] = (m,i) => unit(()))(implicit __pos: SourceContext): Rep[SparseVector[Double]]
}
