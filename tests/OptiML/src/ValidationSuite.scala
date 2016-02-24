import optiml.compiler._
import optiml.shared._
import optiml.library._
import ppl.tests.scalatest._

object ConfusionMatrixRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with ConfusionMatrix
object ConfusionMatrixRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with ConfusionMatrix
trait ConfusionMatrix extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val testSet = DenseTrainingSet(DenseMatrix(DenseVector(0,1), DenseVector(2,3), DenseVector(4,5), DenseVector(6,7)), DenseVector(true,false,true,true))
    def classify(x: Rep[DenseVectorView[Int]]) = (x(0) == 0) || (x(0) == 2)
    val results = confusionMatrix(testSet, i => classify(testSet(i)))

    collect(results == DenseMatrix(DenseVector(1, 1), DenseVector(2, 0)))
    collect(accuracy(results) == 0.25)
    collect(precision(results) == 0.50)
    collect(specificity(results) == 0.0)
    collect(sensitivity(results) == 1.0 / 3.0)
    collect(fallout(results) == 1.0)

    mkReport
  }
}

object CrossValidationRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with CrossValidation
object CrossValidationRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with CrossValidation
trait CrossValidation extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val testSet = DenseTrainingSet(DenseMatrix(DenseVector(0,1), DenseVector(2,3), DenseVector(4,5), DenseVector(6,7)), DenseVector(true,false,true,true))
    def train(x: Rep[DenseTrainingSet[Int,Boolean]]) = DenseVector(3.5,3.5)
    def classify(x: Rep[DenseVector[Double]], set: Rep[DenseTrainingSet[Int,Boolean]], i: Rep[Int]) = (set(i,0).toDouble > x(0)) && (set(i,1).toDouble > x(1))
    val res = crossValidate(testSet, train, classify, accuracy)
    collect(res == 0.75)

    mkReport
  }
}

object HoldOutRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with HoldOut
object HoldOutRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with HoldOut
trait HoldOut extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val x = DenseMatrix.rand(100,10)
    val y = DenseVector.rand(100).map(e => e > 0.5)
    val trainingSet = DenseTrainingSet(x, y)

    val (t1,t2) = unpack(holdOut(trainingSet, 0.2))
    collect(t1.numSamples + t2.numSamples == trainingSet.numSamples)
    collect(t1.numFeatures == t2.numFeatures && t2.numFeatures == trainingSet.numFeatures)
    collect(t1.labels.length + t2.labels.length == trainingSet.labels.length)

    val dataRecombined = t1.data << t2.data
    val labelsRecombined = t1.labels << t2.labels
    val sortedRecombinedIndices = IndexVector(dataRecombined.rowIndices.sortBy(i => dataRecombined(i).sum))
    val sortedOriginalIndices = IndexVector(trainingSet.data.rowIndices.sortBy(i => trainingSet.data.apply(i).sum))

    collect(sum(abs(dataRecombined(sortedRecombinedIndices) - x(sortedOriginalIndices))) < 0.01)
    collect(labelsRecombined(sortedRecombinedIndices) == trainingSet.labels.apply(sortedOriginalIndices))

    val (v1,v2,v3) = unpack(holdOut2(trainingSet, 0.1, 0.1))
    collect(v1.numSamples + v2.numSamples + v3.numSamples == trainingSet.numSamples)
    collect(v1.numFeatures == v2.numFeatures && v2.numFeatures == v3.numFeatures && v3.numFeatures == trainingSet.numFeatures)
    collect(v1.labels.length + v2.labels.length + v3.labels.length == trainingSet.labels.length)

    val dataRecombined2 = v1.data << v2.data << v3.data
    val labelsRecombined2 = v1.labels << v2.labels << v3.labels
    val sortedRecombinedIndices2 = IndexVector(dataRecombined2.rowIndices.sortBy(i => dataRecombined2(i).sum))

    collect(sum(abs(dataRecombined2(sortedRecombinedIndices2) - x(sortedOriginalIndices))) < 0.01)
    collect(labelsRecombined2(sortedRecombinedIndices2) == trainingSet.labels.apply(sortedOriginalIndices))

    mkReport
  }
}


class ValidationSuiteInterpreter extends ForgeSuiteInterpreter {
  def testConfusionMatrixOps() { runTest(ConfusionMatrixRunnerI) }
  def testCrossValidationOps() { runTest(CrossValidationRunnerI) }
  def testHoldOutOps() { runTest(HoldOutRunnerI) }
}
class ValidationSuiteCompiler extends ForgeSuiteCompiler {
  cppWhiteList ++= Seq("sortindex_helper")
  def testConfusionMatrixOps() { runTest(ConfusionMatrixRunnerC) }
  def testCrossValidationOps() { runTest(CrossValidationRunnerC) }
  def testHoldOutOps() { runTest(HoldOutRunnerC) }
}
