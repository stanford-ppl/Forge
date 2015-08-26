import optiml.compiler._
import optiml.library._
import optiml.shared._
import ppl.tests.scalatest._

object SumRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Sum
object SumRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Sum
trait Sum extends ForgeTestModule with OptiMLApplication {
  def main() = {

		val x = sum(0,10) { i =>
			i*2
		}
		collect(x == 90)
    mkReport
  }
}

object SumIfRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with SumIf
object SumIfRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with SumIf
trait SumIf extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val y = DenseVector(true, false, true, false, true, false, false, false, true, true)
    val x = sumIf(0,10) { y(_) } { i => DenseVector.ones(5) }
    //x.pprint
		collect(x == DenseVector(5.0, 5.0, 5.0, 5.0, 5.0))
    mkReport
  }
}

// object AggregateIfRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with AggregateIf
// object AggregateIfRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with AggregateIf
// trait AggregateIf extends ForgeTestModule with OptiMLApplication {
//   def main() = {

// 		val x = aggregateIf(0, 10) { i => i > 4 } { i =>
// 			i*2
// 		}
// 		collect(x == DenseVector(10, 12, 14, 16, 18))
//     mkReport
//   }
// }

// object Aggregate2dRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Aggregate2d
// object Aggregate2dRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Aggregate2d
// trait Aggregate2d extends ForgeTestModule with OptiMLApplication {
//   def main() = {

//     val x = aggregate(10::13, 3::6) { (i,j) =>
//       i*j
//     }
//     collect(x == DenseVector(30, 40, 50, 33, 44, 55, 36, 48, 60))
//     mkReport
//   }
// }

// object Aggregate2dIfRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Aggregate2dIf
// object Aggregate2dIfRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Aggregate2dIf
// trait Aggregate2dIf extends ForgeTestModule with OptiMLApplication {
//   def main() = {

//     val x = aggregateIf(0::3, 0::4) { (i,j) => i < j } { (i,j) =>
//       i*j
//     }
//     collect(x == DenseVector(0, 0, 0, 2, 3, 6))
//     mkReport
//   }
// }

object IndexVectorConstructRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with IndexVectorConstruct
object IndexVectorConstructRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with IndexVectorConstruct
trait IndexVectorConstruct extends ForgeTestModule with OptiMLApplication {
  def main() = {

		val x = (0::10) { i => i*2 }
		collect(x == DenseVector(0,2,4,6,8,10,12,14,16,18))
    mkReport
  }
}

object IndexVectorConstruct2RunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with IndexVectorConstruct2
object IndexVectorConstruct2RunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with IndexVectorConstruct2
trait IndexVectorConstruct2 extends ForgeTestModule with OptiMLApplication {
  def main() = {

		val x = (0::10, *) { i => DenseVector.ones(2) * i	}
		collect(x == DenseMatrix(DenseVector(0.0,0.0), DenseVector(1.0,1.0), DenseVector(2.0,2.0), DenseVector(3.0,3.0), DenseVector(4.0,4.0), DenseVector(5.0,5.0), DenseVector(6.0, 6.0), DenseVector(7.0,7.0), DenseVector(8.0,8.0), DenseVector(9.0,9.0)))

		val y = (0::2, 0::3) { (i,j) =>	i*j	}
		collect(y == DenseMatrix(DenseVector(0,0,0),DenseVector(0,1,2)))

    mkReport
  }
}


class LanguageSuiteInterpreter extends ForgeSuiteInterpreter {
  def testSum() { runTest(SumRunnerI) }
  def testSumIf() { runTest(SumIfRunnerI) }
  // def testAggregateIf() { runTest(AggregateIfRunnerI) }
  // def testAggregate2d() { runTest(Aggregate2dRunnerI) }
  // def testAggregate2dIf() { runTest(Aggregate2dIfRunnerI) }
  def testIndexVector() { runTest(IndexVectorConstructRunnerI) }
  def testIndexVector2() { runTest(IndexVectorConstruct2RunnerI) }
}

class LanguageSuiteCompiler extends ForgeSuiteCompiler {
  def testSum() { runTest(SumRunnerC) }
  def testSumIf() { runTest(SumIfRunnerC) }
  // def testAggregateIf() { runTest(AggregateIfRunnerC) }
  // def testAggregate2d() { runTest(Aggregate2dRunnerC) }
  // def testAggregate2dIf() { runTest(Aggregate2dIfRunnerC) }
  def testIndexVector() { runTest(IndexVectorConstructRunnerC) }
  def testIndexVector2() { runTest(IndexVectorConstruct2RunnerC) }
}

