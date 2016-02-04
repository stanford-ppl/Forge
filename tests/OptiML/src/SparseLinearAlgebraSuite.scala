import optiml.compiler._
import optiml.shared._
import optiml.library._
import ppl.tests.scalatest._

object DenseSparseMatrixArithmeticRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with DenseSparseMatrixArithmetic
object DenseSparseMatrixArithmeticRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with DenseSparseMatrixArithmetic
trait DenseSparseMatrixArithmetic extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val nz = DenseVector(0.18608, 0.65816, 0.22775, 0.36317, 0.48526, 0.77628, 0.77896)
    val rowIndices = IndexVector(DenseVector(0, 1, 3, 4, 0, 2, 3))
    val colIndices = IndexVector(DenseVector(0, 0, 0, 0, 1, 1, 1))

    val m = SparseMatrix.fromElements(5, 2, nz, rowIndices, colIndices)

    val d = DenseMatrix(DenseVector(0.77197, 0.78586, 0.24811, 0.33899, 0.24673),
                        DenseVector(0.57986, 0.55802, 0.89939, 0.96024, 0.14403))

    // Sparse * Dense
    // 5 x 2 * 2 x 5 = 5 x 5
    val t1 = m*d
    val ans = DenseMatrix(DenseVector(0.424942, 0.417016, 0.482603, 0.529042, 0.115804),
                          DenseVector(0.508079, 0.517218, 0.163296, 0.223108, 0.162384),
                          DenseVector(0.449986, 0.433174, 0.698166, 0.745404, 0.111809),
                          DenseVector(0.627365, 0.613656, 0.757092, 0.825190, 0.168389),
                          DenseVector(0.280360, 0.285403, 0.090107, 0.123112, 0.089604))

    collect(t1.numRows == 5)
    collect(t1.numCols == 5)
    collect(sum(abs(t1-ans)) < 0.1)


    // Dense * Sparse
    // 2 x 5 * 5 x 2 = 2 x 2
    val t2 = densematrix_sparse_matmult(d,m) //d*m
    val ans2 = DenseMatrix(DenseVector(0.82768, 0.83127),
                           DenseVector(0.74614, 1.72744))

    collect(t2.numRows == 2)
    collect(t2.numCols == 2)
    collect(sum(abs(t2-ans2)) < 0.1)

    mkReport
  }
}

class SparseLinearAlgebraSuiteInterpreter extends ForgeSuiteInterpreter {
  def testDenseSparseMatrixArithmetic() { runTest(DenseSparseMatrixArithmeticRunnerI) }
}

class SparseLinearAlgebraSuiteCompiler extends ForgeSuiteCompiler {
  cppWhiteList ++= Seq("sortindex_helper")
  def testDenseSparseMatrixArithmetic() { runTest(DenseSparseMatrixArithmeticRunnerC) }
}
