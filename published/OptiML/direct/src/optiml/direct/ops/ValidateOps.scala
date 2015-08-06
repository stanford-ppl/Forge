package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait ValidateOps extends Base {
  this: OptiML => 

  def holdOut[T:Typ,L:Typ,TS[T,L]](dataSet: Rep[TS[T,L]],pct: Rep[Double])(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,L,TS],__cb_hk_0: Typ[TS[T,L]]) = validate_holdout[T,L,TS](dataSet,pct)(implicitly[Typ[T]],implicitly[Typ[L]],__pos,__imp0,__cb_hk_0)
  def holdOut2[T:Typ,L:Typ,TS[T,L]](dataSet: Rep[TS[T,L]],pctValidationSamples: Rep[Double],pctTestSamples: Rep[Double])(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,L,TS],__cb_hk_0: Typ[TS[T,L]]) = validate_holdout2[T,L,TS](dataSet,pctValidationSamples,pctTestSamples)(implicitly[Typ[T]],implicitly[Typ[L]],__pos,__imp0,__cb_hk_0)
  def confusionMatrix[T:Typ,TS[T,L]](testSet: Rep[TS[T,Boolean]],classify: (Rep[Int]) => Rep[Boolean],numSamples: Rep[Int] = unit(-1))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]) = validate_confusionmatrix[T,TS](testSet,classify,numSamples)(implicitly[Typ[T]],__pos,__imp0,__cb_hk_0)
  def confusionMatrixBatch[T:Typ,TS[T,L]](testSet: Rep[TS[T,Boolean]],classify: (Rep[IndexVector]) => Rep[DenseVector[Boolean]],numSamples: Rep[Int] = unit(-1))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]) = validate_confusionmatrixbatch[T,TS](testSet,classify,numSamples)(implicitly[Typ[T]],__pos,__imp0,__cb_hk_0)
  def crossValidateRaw[T:Typ,M:Typ,R:Typ,TS[T,L]](dataSet: Rep[TS[T,Boolean]],train: (Rep[TS[T,Boolean]]) => Rep[M],_numFolds: Rep[Int] = unit(10))(evalTestSet: (Rep[M],Rep[TS[T,Boolean]]) => Rep[R])(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]) = validate_crossvalidateraw[T,M,R,TS](dataSet,train,_numFolds,evalTestSet)(implicitly[Typ[T]],implicitly[Typ[M]],implicitly[Typ[R]],__pos,__imp0,__cb_hk_0)
  def crossValidate[T:Typ,M:Typ,TS[T,L]](dataSet: Rep[TS[T,Boolean]],train: (Rep[TS[T,Boolean]]) => Rep[M],classify: (Rep[M],Rep[TS[T,Boolean]],Rep[Int]) => Rep[Boolean],metric: (Rep[DenseMatrix[Int]]) => Rep[Double],numFolds: Rep[Int] = unit(10),verbose: Rep[Boolean] = unit(false))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]) = validate_crossvalidate[T,M,TS](dataSet,train,classify,metric,numFolds,verbose)(implicitly[Typ[T]],implicitly[Typ[M]],__pos,__imp0,__cb_hk_0)
  def crossValidateBatch[T:Typ,M:Typ,TS[T,L]](dataSet: Rep[TS[T,Boolean]],train: (Rep[TS[T,Boolean]]) => Rep[M],classify: (Rep[M],Rep[TS[T,Boolean]],Rep[IndexVector]) => Rep[DenseVector[Boolean]],metric: (Rep[DenseMatrix[Int]]) => Rep[Double],numFolds: Rep[Int] = unit(10),verbose: Rep[Boolean] = unit(false))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]) = validate_crossvalidatebatch[T,M,TS](dataSet,train,classify,metric,numFolds,verbose)(implicitly[Typ[T]],implicitly[Typ[M]],__pos,__imp0,__cb_hk_0)
  def crossValidateAUC[T:Typ,M:Typ,TS[T,L]](dataSet: Rep[TS[T,Boolean]],train: (Rep[TS[T,Boolean]]) => Rep[M],classify: (Rep[M],Rep[TS[T,Boolean]],Rep[Int]) => Rep[Double],numFolds: Rep[Int] = unit(10),numThresholds: Rep[Int] = unit(10))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]) = validate_crossvalidateauc[T,M,TS](dataSet,train,classify,numFolds,numThresholds)(implicitly[Typ[T]],implicitly[Typ[M]],__pos,__imp0,__cb_hk_0)
  def crossValidateAUCBatch[T:Typ,M:Typ,TS[T,L]](dataSet: Rep[TS[T,Boolean]],train: (Rep[TS[T,Boolean]]) => Rep[M],classify: (Rep[M],Rep[TS[T,Boolean]],Rep[IndexVector]) => Rep[DenseVector[Double]],numFolds: Rep[Int] = unit(10),numThresholds: Rep[Int] = unit(10))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]) = validate_crossvalidateaucbatch[T,M,TS](dataSet,train,classify,numFolds,numThresholds)(implicitly[Typ[T]],implicitly[Typ[M]],__pos,__imp0,__cb_hk_0)
  def ROC(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext) = validate_roc(__arg0)(__pos)
  def ROCCurve[T:Typ,TS[T,L]](testSet: Rep[TS[T,Boolean]],classify: (Rep[Double]) => (Rep[Int]) => Rep[Boolean],numThresholds: Rep[Int] = unit(10))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]) = validate_roccurve[T,TS](testSet,classify,numThresholds)(implicitly[Typ[T]],__pos,__imp0,__cb_hk_0)
  def ROCCurveBatch[T:Typ,TS[T,L]](testSet: Rep[TS[T,Boolean]],classify: (Rep[Double]) => (Rep[IndexVector]) => Rep[DenseVector[Boolean]],numThresholds: Rep[Int] = unit(10))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]) = validate_roccurvebatch[T,TS](testSet,classify,numThresholds)(implicitly[Typ[T]],__pos,__imp0,__cb_hk_0)
  def AUC(unsortedROCs: Rep[DenseVector[Tup2[Double,Double]]])(implicit __pos: SourceContext) = validate_auc(unsortedROCs)(__pos)
  def accuracy(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext) = validate_accuracy(__arg0)(__pos)
  def precision(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext) = validate_precision(__arg0)(__pos)
  def specificity(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext) = validate_specificity(__arg0)(__pos)
  def sensitivity(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext) = validate_sensitivity(__arg0)(__pos)
  def recall(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext): Rep[Double] = { sensitivity(__arg0) }
  def fallout(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext) = validate_fallout(__arg0)(__pos)
  def fscore(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext) = validate_fscore(__arg0)(__pos)


  def validate_holdout[T:Typ,L:Typ,TS[T,L]](dataSet: Rep[TS[T,L]],pct: Rep[Double])(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,L,TS],__cb_hk_0: Typ[TS[T,L]]): Rep[Tup2[TS[T,L],TS[T,L]]]
  def validate_holdout2[T:Typ,L:Typ,TS[T,L]](dataSet: Rep[TS[T,L]],pctValidationSamples: Rep[Double],pctTestSamples: Rep[Double])(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,L,TS],__cb_hk_0: Typ[TS[T,L]]): Rep[Tup3[TS[T,L],TS[T,L],TS[T,L]]]
  def validate_confusionmatrix[T:Typ,TS[T,L]](testSet: Rep[TS[T,Boolean]],classify: (Rep[Int]) => Rep[Boolean],numSamples: Rep[Int] = unit(-1))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]): Rep[DenseMatrix[Int]]
  def validate_confusionmatrixbatch[T:Typ,TS[T,L]](testSet: Rep[TS[T,Boolean]],classify: (Rep[IndexVector]) => Rep[DenseVector[Boolean]],numSamples: Rep[Int] = unit(-1))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]): Rep[DenseMatrix[Int]]
  def validate_crossvalidateraw[T:Typ,M:Typ,R:Typ,TS[T,L]](dataSet: Rep[TS[T,Boolean]],train: (Rep[TS[T,Boolean]]) => Rep[M],_numFolds: Rep[Int] = unit(10),evalTestSet: (Rep[M],Rep[TS[T,Boolean]]) => Rep[R])(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]): Rep[DenseVector[R]]
  def validate_crossvalidate[T:Typ,M:Typ,TS[T,L]](dataSet: Rep[TS[T,Boolean]],train: (Rep[TS[T,Boolean]]) => Rep[M],classify: (Rep[M],Rep[TS[T,Boolean]],Rep[Int]) => Rep[Boolean],metric: (Rep[DenseMatrix[Int]]) => Rep[Double],numFolds: Rep[Int] = unit(10),verbose: Rep[Boolean] = unit(false))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]): Rep[Double]
  def validate_crossvalidatebatch[T:Typ,M:Typ,TS[T,L]](dataSet: Rep[TS[T,Boolean]],train: (Rep[TS[T,Boolean]]) => Rep[M],classify: (Rep[M],Rep[TS[T,Boolean]],Rep[IndexVector]) => Rep[DenseVector[Boolean]],metric: (Rep[DenseMatrix[Int]]) => Rep[Double],numFolds: Rep[Int] = unit(10),verbose: Rep[Boolean] = unit(false))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]): Rep[Double]
  def validate_crossvalidateauc[T:Typ,M:Typ,TS[T,L]](dataSet: Rep[TS[T,Boolean]],train: (Rep[TS[T,Boolean]]) => Rep[M],classify: (Rep[M],Rep[TS[T,Boolean]],Rep[Int]) => Rep[Double],numFolds: Rep[Int] = unit(10),numThresholds: Rep[Int] = unit(10))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]): Rep[Double]
  def validate_crossvalidateaucbatch[T:Typ,M:Typ,TS[T,L]](dataSet: Rep[TS[T,Boolean]],train: (Rep[TS[T,Boolean]]) => Rep[M],classify: (Rep[M],Rep[TS[T,Boolean]],Rep[IndexVector]) => Rep[DenseVector[Double]],numFolds: Rep[Int] = unit(10),numThresholds: Rep[Int] = unit(10))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]): Rep[Double]
  def validate_roc(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext): Rep[Tup2[Double,Double]]
  def validate_roccurve[T:Typ,TS[T,L]](testSet: Rep[TS[T,Boolean]],classify: (Rep[Double]) => (Rep[Int]) => Rep[Boolean],numThresholds: Rep[Int] = unit(10))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]): Rep[DenseVector[Tup2[Double,Double]]]
  def validate_roccurvebatch[T:Typ,TS[T,L]](testSet: Rep[TS[T,Boolean]],classify: (Rep[Double]) => (Rep[IndexVector]) => Rep[DenseVector[Boolean]],numThresholds: Rep[Int] = unit(10))(implicit __pos: SourceContext,__imp0: TrainingSetLike[T,Boolean,TS],__cb_hk_0: Typ[TS[T,Boolean]]): Rep[DenseVector[Tup2[Double,Double]]]
  def validate_auc(unsortedROCs: Rep[DenseVector[Tup2[Double,Double]]])(implicit __pos: SourceContext): Rep[Double]
  def validate_accuracy(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext): Rep[Double]
  def validate_precision(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext): Rep[Double]
  def validate_specificity(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext): Rep[Double]
  def validate_sensitivity(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext): Rep[Double]
  def validate_fallout(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext): Rep[Double]
  def validate_fscore(__arg0: Rep[DenseMatrix[Int]])(implicit __pos: SourceContext): Rep[Double]
}
trait ValidateCompilerOps extends ValidateOps {
  this: OptiML => 

  def confusionMatrixIndicator(trueLabel: Rep[Boolean],predictedLabel: Rep[Boolean])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
}

