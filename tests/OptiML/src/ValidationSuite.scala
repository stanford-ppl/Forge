import optiml.compiler._
import optiml.shared._
import optiml.library._
import ppl.tests.scalatest._

object ConfusionMatrixRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with ConfusionMatrix
object ConfusionMatrixRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with ConfusionMatrix
trait ConfusionMatrix extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val testSet = TrainingSet(DenseMatrix(DenseVector(0,1), DenseVector(2,3), DenseVector(4,5), DenseVector(6,7)), DenseVector(true,false,true,true))
    def classify(x: Rep[DenseVectorView[Int]]) = (x(0) == 0) || (x(0) == 2)
    val results = confusionMatrix(testSet, classify)

    collect(results == DenseMatrix(DenseVector(1, 1), DenseVector(2, 0)))
    collect(accuracy(results) == 0.25)
    collect(precision(results) == 0.50)
    collect(specificity(results) == 0.0)
    collect(sensitivity(results) == 1.0 / 3.0)
    collect(fallout(results) == 1.0)

    mkReport
  }
}

object CrossValidationRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with CrossValidation
object CrossValidationRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with CrossValidation
trait CrossValidation extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val testSet = TrainingSet(DenseMatrix(DenseVector(0,1), DenseVector(2,3), DenseVector(4,5), DenseVector(6,7)), DenseVector(true,false,true,true))
    def train(x: Rep[TrainingSet[Int,Boolean]]) = DenseVector(3.5,3.5)
    def classify(x: Rep[DenseVector[Double]], y: Rep[DenseVectorView[Int]]) = (y(0).toDouble > x(0)) && (y(1).toDouble > x(1))
    val res = crossValidate(testSet, train, classify, accuracy)
    collect(res == 0.75)

    mkReport
  }
}

class ValidationSuiteInterpreter extends ForgeSuiteInterpreter {
  def testConfusionMatrixOps() { runTest(ConfusionMatrixRunnerI) }
  def testCrossValidationOps() { runTest(CrossValidationRunnerI) }
}
class ValidationSuiteCompiler extends ForgeSuiteCompiler {
  def testConfusionMatrixOps() { runTest(ConfusionMatrixRunnerC) }
  def testCrossValidationOps() { runTest(CrossValidationRunnerC) }
}
