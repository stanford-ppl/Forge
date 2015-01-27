/* Testing stream operations
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 3/14/11
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

import optiml.compiler._
import optiml.shared._
import optiml.library._
import ppl.tests.scalatest._

object StreamForeachRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with StreamForeach
object StreamForeachRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with StreamForeach
trait StreamForeach extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val s = ComputeStream[Int](1000, 100){ (i,j) => random[Int] }
    s.foreachRow { v => collect(v.length == 100 && v.sum.abs > 0) }

    mkReport
  }
}

object StreamCorrectSmallRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with StreamCorrectSmall
object StreamCorrectSmallRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with StreamCorrectSmall
trait StreamCorrectSmall extends ForgeTestModule with OptiMLApplication {
  def main() = {

    //should be row vectors starting at 0, 10, ... 90
    val s = ComputeStream[Int](10,10){ (i,j) => i*10+j }
    s.foreachRow { v => collect(v(2) % 10 == 2) }

    mkReport
  }
}

object StreamCorrectLargeRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with StreamCorrectLarge
object StreamCorrectLargeRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with StreamCorrectLarge
trait StreamCorrectLarge extends ForgeTestModule with OptiMLApplication {
  def main() = {

    //should be row vectors starting at 0, 10, ... 109990
    val s = ComputeStream[Int](11000,10){ (i,j) => i*10+j }
    s.foreachRow { v => collect(v(9) % 10 == 9) }
    //s.foreachRow { _.pprint }

    mkReport
  }
}


/* Reading and writing the same filename in the same app is not supported, as the order of operations is not guaranteed */

trait StreamSuitePaths {
  val testMat = "test.mat"
  val testMat2 = "test2.mat"
}

object FileStreamWriteARunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with FileStreamWriteA
object FileStreamWriteARunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with FileStreamWriteA
trait FileStreamWriteA extends ForgeTestModule with OptiMLApplication with StreamSuitePaths {
  def main() = {
    val x = DenseMatrix.ones(1000,100)
    writeMatrix(x, testMat)

    collect(true)
    mkReport
  }
}

object FileStreamWriteBRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with FileStreamWriteB
object FileStreamWriteBRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with FileStreamWriteB
trait FileStreamWriteB extends ForgeTestModule with OptiMLApplication with StreamSuitePaths {
  def main() = {
    val f = FileStream(testMat)
    val g = f.map(testMat2) { line =>
      val tokens = line.trim.fsplit("\\s+")
      val v = (0::array_length(tokens)) { i => array_apply(tokens, i).toDouble }
      (v+1).makeStr
    }

    // contents of testMat and testMat2 tested in next phase
    collect(true)

    mkReport
  }
}

object FileStreamReadRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with FileStreamRead
object FileStreamReadRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with FileStreamRead
trait FileStreamRead extends ForgeTestModule with OptiMLApplication with StreamSuitePaths {
  def main() = {
    val f = FileStream(testMat)
    for (line <- f) {
      val tokens = line.trim.fsplit("\\s+")
      val v = (0::array_length(tokens)) { i => array_apply(tokens, i).toDouble }
      collect(v == DenseVector.ones(100))
    }

    val total = f.reduce(0.0) { line =>
      val tokens = line.trim.fsplit("\\s+")
      val v = (0::array_length(tokens)) { i => array_apply(tokens, i).toDouble }
      sum(v)
    } {
      (a,b) => a+b
    }

    val a = readMatrix(testMat)
    collect(total == a.sum)

    val b = readMatrix(testMat2)
    collect(b == (a + 1))

    mkReport
  }
}

object FileStreamDeleteRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with FileStreamDelete
object FileStreamDeleteRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with FileStreamDelete
trait FileStreamDelete extends ForgeTestModule with OptiMLApplication with StreamSuitePaths {
  def main() = {
    deleteFile(testMat)
    deleteFile(testMat2)

    collect(true)
    mkReport
  }
}

class StreamSuiteInterpreter extends ForgeSuiteInterpreter {
  def testStreamForeach() { runTest(StreamForeachRunnerI) }
  def testStreamCorrectSmall() { runTest(StreamCorrectSmallRunnerI) }
  def testStreamCorrectLarge() { runTest(StreamCorrectLargeRunnerI) }
  def testFileStream() {
    runTest(FileStreamWriteARunnerI)
    runTest(FileStreamWriteBRunnerI)
    runTest(FileStreamReadRunnerI)
    runTest(FileStreamDeleteRunnerI)
  }
}
class StreamSuiteCompiler extends ForgeSuiteCompiler {
  def testStreamForeach() { runTest(StreamForeachRunnerC) }
  def testStreamCorrectSmall() { runTest(StreamCorrectSmallRunnerC) }
  def testStreamCorrectLarge() { runTest(StreamCorrectLargeRunnerC) }
  def testFileStream() {
    runTest(FileStreamWriteARunnerC)
    runTest(FileStreamWriteBRunnerC)
    runTest(FileStreamReadRunnerC)
    runTest(FileStreamDeleteRunnerC)
  }
}
