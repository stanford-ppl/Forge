import optiml.compiler._
import optiml.shared._
import optiml.library._
import ppl.tests.scalatest._

object DateTimeRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with DateTime
object DateTimeRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with DateTime
trait DateTime extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val t = DateFeature("yyyyMMddHHmmss")
    val piDay = t("20150314150000")

    collect(DateFeature.year(piDay) == 2015)
    collect(DateFeature.month(piDay) == 3)
    collect(DateFeature.day(piDay) == 14)
    collect(DateFeature.weekday(piDay) == 6)
    collect(DateFeature.hour(piDay) == 15)
    mkReport
  }
}

object UniqueRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Unique
object UniqueRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Unique
trait Unique extends ForgeTestModule with OptiMLApplication {
  def main() = {
    deleteFile("test_unique_db")
    deleteFile("test_unique_db_reverse")
    loadUniqueMappings("test_unique_db")

    val names = (0::1000) { i => "hello_" + i }
    val ids = names.map(n => unique(n))
    val reverseIds = ids.map(i => reverseUnique(i))

    collect(ids.distinct.length == 1000)
    collect(names == reverseIds)

    dumpUniqueMappings("test_unique_db")
    mkReport
  }
}

// This test checks that subsequent modifications to the unique DB do not collide with existing entries.
object Unique2RunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Unique2
object Unique2RunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Unique2
trait Unique2 extends ForgeTestModule with OptiMLApplication {
  def main() = {
    loadUniqueMappings("test_unique_db")

    val names = (0::1000) { i => "hello_" + i }
    val next = (0::1000) { i => unique("new_" + i) }
    val reverseIds = (0::1000) { i => reverseUnique(i) }
    collect(names.sort == reverseIds.sort)

    dumpUniqueMappings("test_unique_db")
    mkReport
  }
}

object IndicatorRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Indicator
object IndicatorRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Indicator
trait Indicator extends ForgeTestModule with OptiMLApplication {
  def main() = {
    def my_feature = DiscreteFeature("0","1","2","3","4","5","6","7","8","9")

    val totals1 = sum(0,1000) { i =>
      if (i % 2 == 0) {
        my_feature.indicator("3")
      }
      else {
        my_feature.indicator("9")
      }
    }

    collect(totals1(3) == 500)
    collect(totals1(9) == 500)
    collect(totals1.sum == 1000)
    mkReport
  }
}

class FeatureSuiteInterpreter extends ForgeSuiteInterpreter {
  def testDateTimeOps() { runTest(DateTimeRunnerI) }
  def testUniqueOps() { runTests(UniqueRunnerI, Unique2RunnerI) }
  def testIndicatorOps() { runTest(IndicatorRunnerI) }
}
class FeatureSuiteCompiler extends ForgeSuiteCompiler {
  override def enforceFullCoverage = false
  def testDateTimeOps() { runTest(DateTimeRunnerC) }
  def testUniqueOps() { runTests(UniqueRunnerC, Unique2RunnerC) }
  def testIndicatorOps() { runTest(IndicatorRunnerC) }
}
