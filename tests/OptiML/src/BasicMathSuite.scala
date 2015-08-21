import optiml.compiler._
import optiml.shared._
import optiml.library._
import ppl.tests.scalatest._

object NormalizationRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Normalization
object NormalizationRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Normalization

trait Normalization extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = DenseVector(5.5, 9.2, -102358.0, 6536.9, 0.0, 1.0, -88.0)

    val v1 = normalize(v, Std)
    val v2 = v.map(e => normalizeStdScalarUsing(e, mean(v), stddev(v)))
    collect(sum(abs(v1 - DenseVector(0.34986, 0.34996, -2.26338, 0.51660, 0.34972, 0.34975, 0.34748))) < 0.1)
    collect(v1 == v2)

    val v3 = normalize(v, Unity)
    val v4 = v.map(e => normalizeUnityScalarUsing(e, min(v), max(v)))
    collect(sum(abs(v3 - DenseVector(0.88004, 0.88011, -1.00000, 1.00000, 0.87994, 0.87996, 0.87832))) < 0.1)
    collect(v3 == v4)

    mkReport
  }
}

class BasicMathSuiteInterpreter extends ForgeSuiteInterpreter {
  def testNormalization() { runTest(NormalizationRunnerI) }
}
class BasicMathSuiteCompiler extends ForgeSuiteCompiler {
  def testNormalization() { runTest(NormalizationRunnerC) }
}
