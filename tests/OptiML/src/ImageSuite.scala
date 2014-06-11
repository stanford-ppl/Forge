import optiml.compiler._
import optiml.shared._
import optiml.library._
import ppl.tests.scalatest._

object GrayscaleImageOpsRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with GrayscaleImageOps
object GrayscaleImageOpsRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with GrayscaleImageOps
trait GrayscaleImageOps extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val img = GrayscaleImage(DenseMatrix((12,55,92,13), (0,255,255,255), (17,33,33,10), (0,0,0,0)).toDouble)
    
    val hist = img.histogram
    collect(hist.length == 256)
    collect(hist(12) == 1)
    collect(hist(55) == 1)
    collect(hist(92) == 1)
    collect(hist(13) == 1)
    collect(hist(0) == 5)
    collect(hist(255) == 3)
    collect(hist(17) == 1)
    collect(hist(33) == 2)
    collect(hist(10) == 1)
    collect(hist(50) == 0)

    val subsample = img.downsample(2,2) { sample => val s = sample.data.sum; if (s > 255) 255 else s }
    collect(subsample.numRows == 2)
    collect(subsample.numCols == 2)

    val blurred = img.windowedFilter(3,3) { sample => mean(sample.data) }
    collect(blurred.data.apply(0,0) == mean(0,0,0,0,12,55,0,0,255))

    mkReport
  }
}

class ImageSuiteInterpreter extends ForgeSuiteInterpreter {
  def testGrayscaleImageOps() { runTest(GrayscaleImageOpsRunnerI) }  
}
class ImageSuiteCompiler extends ForgeSuiteCompiler {
  def testGrayscaleImageOps() { runTest(GrayscaleImageOpsRunnerC) }  
}
