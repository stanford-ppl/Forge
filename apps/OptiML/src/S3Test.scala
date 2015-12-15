import optiml.compiler._
import optiml.library._
import optiml.shared._

object S3TestCompiler extends OptiMLApplicationCompiler with S3Test
object S3TestInterpreter extends OptiMLApplicationInterpreter with S3Test

trait S3Test extends OptiMLApplication {

  def main() = {
    // 80 GB
    // val testMat = DenseMatrix.rand(100000000,100)

    // 8 GB
    // val testMat = DenseMatrix.rand(10000000,100)

    // 800 MB
    // val testMat = DenseMatrix.rand(1000000,100)
    // val testLabels = DenseVector.rand(1000000) map { i => if (i > 0.5) 1.0 else 0.0 }

    // 80 MB
    // val testMat = DenseMatrix.rand(100000,100)
    // val testLabels = DenseVector.rand(100000) map { i => if (i > 0.5) 1.0 else 0.0 }

    val testMat = readMatrix("s3n://mines-test/x.dat")
    val testVector = readVector("s3n://mines-test/y.dat")

    val m2 = testMat*5
    val v2 = testVector+3

    writeMatrix(m2, "s3n://mines-test/x2.dat")
    writeVector(v2, "s3n://mines-test/y2.dat")

    // println("read testMat with " + testMat.numRows + " rows and " + testMat.numCols + " cols")
    // println("first 2 rows: ")
    // testMat(0::2).pprint

    // println("read testVector with " + testVector.length + " elements")
    // println("first 2 elements: ")
    // testVector(0::2).pprint
  }

}
