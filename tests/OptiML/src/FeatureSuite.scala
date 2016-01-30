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
    loadUniqueMappings("test_unique_db")

    val names = (0::1000) { i => "hello_" + i }
    val ids = names.map(n => unique(n))
    val reverseIds = ids.map(i => reverseUnique(i))

    collect(ids.distinct.length == 1000)
    collect(names == reverseIds)

    dumpUniqueMappings("test_unique_db_out")
    mkReport
  }
}


class FeatureSuiteInterpreter extends ForgeSuiteInterpreter {
  def testDateTimeOps() { runTest(DateTimeRunnerI) }
  def testUniqueOps() { runTest(UniqueRunnerI) }
}
class FeatureSuiteCompiler extends ForgeSuiteCompiler {
  def testDateTimeOps() { runTest(DateTimeRunnerC) }
  def testUniqueOps() { runTest(UniqueRunnerC) }
}
