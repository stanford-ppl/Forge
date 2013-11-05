import optiml.compiler._
import optiml.shared._
import optiml.library._
import ppl.tests.scalatest._

object SimpleDenseVectorArithmeticRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with SimpleDenseVectorArithmetic
object SimpleDenseVectorArithmeticRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with SimpleDenseVectorArithmetic
trait SimpleDenseVectorArithmetic extends ForgeTestModule with DenseLinearAlgebraTestsCommon {
  def main() = {
    // TODO: these can't be factored out right now because they throw an NPE when the test is being initialized
    val rowA = DenseVector(11., 22., 33.)
    val rowB = DenseVector(-5.3, -17.2, -131.)
    val rowD = DenseVector(-1.1, -6.2)
    val colC = DenseVector(7., 3.2, 13.3).t

    // A*B piecewise
    val ansVec = rowA*rowB
    collect(check(ansVec, DenseVector(-58.3, -378.4, -4323.)))

    // dot product
    val ansDbl = rowA *:* colC
    collect(check(ansDbl, 586.3))

    // outer product
    val ansMat = colC ** rowA
    collect(check(ansMat, DenseMatrix((77., 154., 231.), (35.2, 70.4, 105.6), (146.3, 292.6, 438.9))))

    mkReport
  }
}

object SimpleDenseMatrixArithmeticRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with SimpleDenseMatrixArithmetic
object SimpleDenseMatrixArithmeticRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with SimpleDenseMatrixArithmetic
trait SimpleDenseMatrixArithmetic extends ForgeTestModule with DenseLinearAlgebraTestsCommon {
  def main() = {
    val rowA = DenseVector(11., 22., 33.)
    val rowB = DenseVector(-5.3, -17.2, -131.)
    val colC = DenseVector(7., 3.2, 13.3).t
    val m33 = DenseMatrix(rowA, rowB, colC.t)
    val m23 = DenseMatrix((3.5, 7.5, 9.0), (-5.6, 8.2, 17.3))
    val m32 = DenseMatrix((.07, .91), (17., -10.), (-99.,.023))

    // matrix square multiplication
    val ansMat = m33*m33
    collect(check(ansMat, DenseMatrix((235.4, -30.8, -2080.1), (-884.14, -239.96, 336.), (153.14, 141.52, -11.31))))

    // inverse
    // val ansMat2 = m33.inv
    // collect(check(ansMat2, DenseMatrix((-.0145, 0.0143, 0.1765), (0.0645, 0.0065, -0.0965), (-0.0079, -0.0091, 0.0055))))

    // matrix transpose
    val ansMat3 = m33.t
    collect(check(ansMat3, DenseMatrix((11., -5.3, 7.), (22., -17.2, 3.2), (33., -131., 13.3))))

    // matrix multiplication
    val ansMat4 = m33*m32
    collect(check(ansMat4, DenseMatrix((-2892.223, -209.2310), (12676.229, 164.1640), (-1261.81, -25.3241))))

    // chained matrix multiplication
    val ansMat5 = m23*m33*m32
    collect(check(ansMat5, DenseMatrix((73592.6225, 271.0046), (98312.252799, 2079.73147))))

    // summation
    val ans = m23.sum
    collect(check(ans, 39.9))

    mkReport
  }
}

object CombinedVecMatArithmeticRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with CombinedVecMatArithmetic
object CombinedVecMatArithmeticRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with CombinedVecMatArithmetic
trait CombinedVecMatArithmetic extends ForgeTestModule with DenseLinearAlgebraTestsCommon {
  def main() = {
    val rowA = DenseVector(11., 22., 33.)
    val rowB = DenseVector(-5.3, -17.2, -131.)
    val rowD = DenseVector(-1.1, -6.2)
    val colC = DenseVector(7., 3.2, 13.3).t
    val colE = DenseVector(.05, 9.97).t
    val m33 = DenseMatrix(rowA, rowB, colC.t)
    val m23 = DenseMatrix((3.5, 7.5, 9.0), (-5.6, 8.2, 17.3))
    val m32 = DenseMatrix((.07, .91), (17., -10.), (-99.,.023))
    val alpha = 4.235
    val beta = -99.759

    val ansVec = m23*colC
    collect(check(ansVec, DenseVector(168.2, 217.13)))

    val ansVec2 = rowB*m32
    collect(check(ansVec2, DenseVector(12676.229, 164.1640)))

    // val a1 = m23*alpha
    // val a2 = a1 * (m33.t.inv)
    // val a3 = a2 * m32
    // val ansVec3 = a3 * (rowD.t*colE*beta)
    // collect(check(ansVec3, DenseVector(194179.526, 593097.843)))

    mkReport
  }
}

trait DenseLinearAlgebraTestsCommon extends OptiMLApplication with GenOverloadHack {
  this: ForgeTestModule =>

  ////////////////
  // helpers

  def approx(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = {
    // be very generous w.r.t. precision, because the ground truth
    // answers have not all been entered with high precision
    abs(x - y) < .01
  }

  def check(x: Rep[DenseVector[Double]], y: Rep[DenseVector[Double]]): Rep[Boolean] = {
    if (x.length != y.length) {
      false
    }
    else {
      val res = x.zip(y) { (a,b) => approx(a,b) }
      ((res count { _ == false }) == 0)
    }
  }

  def check(x: Rep[DenseMatrix[Double]], y: Rep[DenseMatrix[Double]])(implicit o: ROverload11): Rep[Boolean] = {
    if ((x.numRows != y.numRows) || (x.numCols != y.numCols)) {
      false
    }
    else {
      val res = x.zip(y) { (a,b) => approx(a,b) }
      ((res count { _ == false }) == 0)
    }
  }

  def check(x: Rep[Double], y: Rep[Double])(implicit o: ROverload12): Rep[Boolean] = {
    approx(x,y)
  }
}


class DenseLinearAlgebraSuiteInterpreter extends ForgeSuiteInterpreter {
  def testSimpleDenseVector() { runTest(SimpleDenseVectorArithmeticRunnerI) }
  def testSimpleDenseMatrix() { runTest(SimpleDenseMatrixArithmeticRunnerI) }
  def testCombinedVecMat() { runTest(CombinedVecMatArithmeticRunnerI) }
}

class DenseLinearAlgebraSuiteCompiler extends ForgeSuiteCompiler {
  def testSimpleDenseVector() { runTest(SimpleDenseVectorArithmeticRunnerC) }
  def testSimpleDenseMatrix() { runTest(SimpleDenseMatrixArithmeticRunnerC) }
  def testCombinedVecMat() { runTest(CombinedVecMatArithmeticRunnerC) }
}

