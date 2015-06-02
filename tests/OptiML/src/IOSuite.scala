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

trait IOSuitePaths {
  val testMat = "test.mat"
  val testVec = "test.vec"
}

/* Reading and writing the same filename in the same app is not supported, as the order of operations is not guaranteed */

object SimpleWriteRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with SimpleWrite
object SimpleWriteRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with SimpleWrite
trait SimpleWrite extends ForgeTestModule with OptiMLApplication with IOSuitePaths {
  def main() = {
    val x = (0::10, 0::10) { (i,j) => i+j*0.1 }
    writeMatrix(x, testMat)

    val a = (0::10) { i => i*3.6 }
    writeVector(a, testVec)

    collect(true)
    mkReport
  }
}

object SimpleReadRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with SimpleRead
object SimpleReadRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with SimpleRead
trait SimpleRead extends ForgeTestModule with OptiMLApplication with IOSuitePaths {
  def main() = {
    val x = (0::10, 0::10) { (i,j) => i+j*0.1 }
    val y = readMatrix(testMat)
    collect(sum(abs(x-y)) < .01)

    val a = (0::10) { i => i*3.6 }
    val b = readVector(testVec)
    collect(sum(abs(a-b)) < .01)

    mkReport
  }
}

object SimpleDeleteRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with SimpleDelete
object SimpleDeleteRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with SimpleDelete
trait SimpleDelete extends ForgeTestModule with OptiMLApplication with IOSuitePaths {
  def main() = {
    deleteFile(testMat)
    deleteFile(testVec)

    collect(true)
    mkReport
  }
}

class IOSuiteInterpreter extends ForgeSuiteInterpreter {
  def testSimpleWriteRead() {
    runTest(SimpleWriteRunnerI)
    runTest(SimpleReadRunnerI)
    runTest(SimpleDeleteRunnerI)
  }
}
class IOSuiteCompiler extends ForgeSuiteCompiler {
  def testSimpleWriteRead() {
    runTest(SimpleWriteRunnerC)
    runTest(SimpleReadRunnerC)
    runTest(SimpleDeleteRunnerC)
  }
}

