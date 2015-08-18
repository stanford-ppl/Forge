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


//////////////////////////////////////////////////////////////////////////////////////////////////////
// ComputeStream
//////////////////////////////////////////////////////////////////////////////////////////////////////

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


//////////////////////////////////////////////////////////////////////////////////////////////////////
// FileStream
//////////////////////////////////////////////////////////////////////////////////////////////////////

/* Reading and writing the same filename in the same app is not supported, as the order of operations is not guaranteed */

trait StreamSuitePaths {
  val testMat = "test.mat"
  val testMat2 = "test2.mat"
  val testMat3 = "test3.mat"
  val testHash1 = "test1.hash"
  val testHash2 = "test2.hash"
  val testDHash1 = "test1"
  val testDHash2 = "test2"
  val testHashStreamMat = "test_hash_stream.mat"
  val testDHashStreamMat = "test_dhash_stream"
  val testHashInMemMat = "test_hash_inmem.mat"
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
      val v = (0::tokens.length) { i => tokens(i).toDouble }
      (v+1).makeStr
    }
    val h = f.map(testMat3, chunkSize = 100000) { line =>
      val tokens = line.trim.fsplit("\\s+")
      val v = (0::tokens.length) { i => tokens(i).toDouble }
      (v+1).makeStr
    }

    // contents of testMats tested in next phase
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
      val v = (0::tokens.length) { i => tokens(i).toDouble }
      collect(v == DenseVector.ones(100))
    }

    val total = f.reduce(0.0) { line =>
      val tokens = line.trim.fsplit("\\s+")
      val v = (0::tokens.length) { i => tokens(i).toDouble }
      sum(v)
    } {
      (a,b) => a+b
    }

    val a = readMatrix(testMat)
    collect(total == a.sum)

    val b = readMatrix(testMat2)
    collect(b == (a + 1))

    val c = readMatrix(testMat3)
    collect(b == c)

    mkReport
  }
}

object FileStreamDeleteRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with FileStreamDelete
object FileStreamDeleteRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with FileStreamDelete
trait FileStreamDelete extends ForgeTestModule with OptiMLApplication with StreamSuitePaths {
  def main() = {
    deleteFile(testMat)
    deleteFile(testMat2)
    deleteFile(testMat3)

    collect(true)
    mkReport
  }
}


//////////////////////////////////////////////////////////////////////////////////////////////////////
// HashStream
//////////////////////////////////////////////////////////////////////////////////////////////////////

object HashStreamWriteARunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with HashStreamWriteA
object HashStreamWriteARunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with HashStreamWriteA
trait HashStreamWriteA extends ForgeTestModule with OptiMLApplication with StreamSuitePaths {
  def main() = {
    val raw = DenseMatrix.rand(10000,8)
    val custIds = (0::10000) { i => (i % 100).toDouble }
    val accountIds = (0::10000) { i => (i % 1000).toDouble }
    val initialData = custIds.t.toMat <<| accountIds.t.toMat <<| raw
    writeMatrix(initialData, testMat, "|")

    // setup - testing occurs in subsequent phases
    collect(true)
    mkReport
  }
}

object HashStreamWriteBRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with HashStreamWriteB
object HashStreamWriteBRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with HashStreamWriteB
trait HashStreamWriteB extends ForgeTestModule with OptiMLApplication with StreamSuitePaths {
  def writeStream() = {
    val raw = FileStream(testMat)
    val data = raw.mapRows(testMat2, "\\|", "|") { v => DenseVector(v(0).toDouble, v(1).toDouble, v(5).toDouble, v(7).toDouble, v(9).toDouble) }

    // Set stream chunk byteSize to be small, so that we test HashStreams with multiple chunks
    val p = System.getProperties()
    p.setProperty("optiml.stream.chunk.bytesize", "1e5") // 10KB
    System.setProperties(p)

    // There is some weirdness going in converting a double value to a string key here.
    // We need to use a canonical representation of the double, so we use Scala's (rather than
    // the formatted version we read from the file).
    val customers = data.groupRowsBy(testHash1, "\\|")(row => ""+row(0).toDouble, _.map(_.toDouble))
    val accounts = data.groupRowsBy(testHash2, "\\|")(row => ""+row(1).toDouble, _.map(_.toDouble))

    val result = accounts.mapValues(testHashStreamMat) { (acctId, account) =>
      // account is looked up from the HashStream as a Rep[DenseMatrix[Double]] (the value in the bucket)
      val custId = account(0, 0)
      val customer = customers(""+custId)

      DenseVector[Double](
        customer.getCol(2).min,
        customer.getCol(3).max,
        customer.getCol(4).sort.sum
      )
    }

    customers.close()
    accounts.close()
  }

  def writeDStream() = {
    val raw = FileStream(testMat)
    val data = raw.mapRows(testMat2, "\\|", "|") { v => DenseVector(v(0).toDouble, v(1).toDouble, v(5).toDouble, v(7).toDouble, v(9).toDouble) }

    // Set stream chunk byteSize to be small, so that we test HashStreams with multiple chunks
    val p = System.getProperties()
    p.setProperty("optiml.stream.chunk.bytesize", "1e5") // 10KB
    p.setProperty("optiml.stream.dynamodb.threads","4") // reduce concurrent threads to prevent exception on low throughput tables
    System.setProperties(p)

    // There is some weirdness going in converting a double value to a string key here.
    // We need to use a canonical representation of the double, so we use Scala's (rather than
    // the formatted version we read from the file).
    val customers = data.groupRowsByD(testDHash1, "\\|")(row => ""+row(0).toDouble, _.map(_.toDouble))
    val accounts = data.groupRowsByD(testDHash2, "\\|")(row => ""+row(1).toDouble, _.map(_.toDouble))

    val result = accounts.mapValues(testDHashStreamMat) { (acctId, account) =>
      // account is looked up from the HashStream as a Rep[DenseMatrix[Double]] (the value in the bucket)
      val custId = account(0, 0)
      val customer = customers(""+custId)

      DenseVector[Double](
        customer.getCol(2).min,
        customer.getCol(3).max,
        customer.getCol(4).sort.sum
      )
    }
  }

  def writeInMem() = {
    val data = readMatrixAndParse[Double](testMat, v => DenseVector(v(0).toDouble, v(1).toDouble, v(5).toDouble, v(7).toDouble, v(9).toDouble), "\\|")

    val customers = data.groupRowsBy(row => row(0))
    val accounts = data.groupRowsBy(row => row(1))

    val result = accounts.toVector.map { account =>
      // account is looked up from the hash as a Rep[DenseMatrix[Double]] (the value in the bucket)
      val custId = account(0, 0)
      val customer = customers(custId)

      DenseVector[Double](
        customer.getCol(2).min,
        customer.getCol(3).max,
        customer.getCol(4).sort.sum
      )
    }

    writeMatrix(DenseMatrix(result), testHashInMemMat)
  }

  def main() = {
    writeStream()
    writeDStream()
    writeInMem()

    // setup - testing occurs in subsequent phases
    collect(true)
    mkReport
  }
}

object HashStreamReadRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with HashStreamRead
object HashStreamReadRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with HashStreamRead
trait HashStreamRead extends ForgeTestModule with OptiMLApplication with StreamSuitePaths {
  def main() = {
    val a = readMatrix(testHashStreamMat)
    val b = readMatrix(testHashInMemMat)
    val c = readMatrix(testDHashStreamMat)

    // groupRowsBy is not guaranteed to maintain ordering of rows
    val sortedAIndices = IndexVector((0::a.numRows).sortBy(i => a(i).sum))
    val sortedBIndices = IndexVector((0::b.numRows).sortBy(i => b(i).sum))
    val sortedCIndices = IndexVector((0::c.numRows).sortBy(i => c(i).sum))
    val sortedA = a(sortedAIndices)
    val sortedB = b(sortedBIndices)
    val sortedC = c(sortedCIndices)

    collect(sortedA == sortedB)
    collect(sortedB == sortedC)
    mkReport
  }
}

object HashStreamDeleteRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with HashStreamDelete
object HashStreamDeleteRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with HashStreamDelete
trait HashStreamDelete extends ForgeTestModule with OptiMLApplication with StreamSuitePaths {
  def main() = {
    deleteFile(testMat)
    deleteFile(testMat2)
    deleteFile(testHash1)
    deleteFile(testHash2)
    deleteFile(testHashStreamMat)
    deleteFile(testHashInMemMat)

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
  def testHashStream() {
    runTest(HashStreamWriteARunnerI)
    runTest(HashStreamWriteBRunnerI)
    runTest(HashStreamReadRunnerI)
    runTest(HashStreamDeleteRunnerI)
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
  def testHashStream() {
    runTest(HashStreamWriteARunnerC)
    runTest(HashStreamWriteBRunnerC)
    runTest(HashStreamReadRunnerC)
    runTest(HashStreamDeleteRunnerC)
  }
}
