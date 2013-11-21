/* Testing OptiML i/o functionality
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 12/24/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import optiml.compiler._
import optiml.library._
import optiml.shared._
import ppl.tests.scalatest._

object SimpleWriteReadRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with SimpleWriteRead
object SimpleWriteReadRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with SimpleWriteRead
trait SimpleWriteRead extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val testMat = "test.mat"
    val testVec = "test.vec"

    val x = DenseMatrix.rand(10,10)
    writeMatrix(x, testMat)
    val y = readMatrix(testMat)
    collect(sum(abs(x-y)) < .01)
    deleteFile(testMat)

    val a = DenseVector.rand(10)
    writeVector(a, testVec)
    val b = readVector(testVec)
    collect(sum(abs(a-b)) < .01)
    deleteFile(testVec)

    mkReport
  }
}

class IOSuiteInterpreter extends ForgeSuiteInterpreter {
  def testSimpleWriteRead() { runTest(SimpleWriteReadRunnerI) }
}
class IOSuiteCompiler extends ForgeSuiteCompiler {
  def testSimpleWriteRead() { runTest(SimpleWriteReadRunnerC) }
}

