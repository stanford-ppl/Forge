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

    val s = ComputeStream[Int](10000, 1000){ (i,j) => random[Int] }
    s.foreachRow { v => collect(v.length == 1000 && v.sum.abs > 0) }

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

object FileStreamRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with FileStreamTest
object FileStreamRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with FileStreamTest
trait FileStreamTest extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val testMatFile = "test.mat"    
    val testMat = DenseMatrix.ones(10000,100)
    writeMatrix(testMat, testMatFile)

    val f = FileStream("test.mat")
    for (line <- f) {
      val tokens = line.trim.fsplit("\\s+")
      val v = (0::array_length(tokens)) { i => array_apply(tokens, i).toDouble }
      collect(v == DenseVector.ones(100))
    }

    f.map("test2.mat") { line => 
      val tokens = line.trim.fsplit("\\s+")
      val v = (0::array_length(tokens)) { i => array_apply(tokens, i).toDouble }
      (v+1).makeStr
    }

    val test2Mat = readMatrix("test2.mat")
    collect(test2Mat == (testMat + 1))

    val total = f.reduce(0.0) { (acc, line) =>
      val tokens = line.trim.fsplit("\\s+")
      val v = (0::array_length(tokens)) { i => array_apply(tokens, i).toDouble }
      acc + sum(v)
    }

    collect(total == testMat.sum)
    
    mkReport
  }
}

class StreamSuiteInterpreter extends ForgeSuiteInterpreter {
  def testStreamForeach() { runTest(StreamForeachRunnerI) }
  def testStreamCorrectSmall() { runTest(StreamCorrectSmallRunnerI) }
  def testStreamCorrectLarge() { runTest(StreamCorrectLargeRunnerI) }
  def testFileStream() { runTest(FileStreamRunnerI) }
}
class StreamSuiteCompiler extends ForgeSuiteCompiler {
  def testStreamForeach() { runTest(StreamForeachRunnerC) }
  def testStreamCorrectSmall() { runTest(StreamCorrectSmallRunnerC) }
  def testStreamCorrectLarge() { runTest(StreamCorrectLargeRunnerC) }
  def testFileStream() { runTest(FileStreamRunnerC) }
}
