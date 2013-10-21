import optiml.compiler._
import optiml.library._
import optiml.shared._
import ppl.tests.scalatest._

object SumRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Sum
object SumRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Sum
trait Sum extends ForgeTestModule with OptiMLApplication {
  def main() = {

		val x = sum(0,10) { i =>
			i*2
		}
		collect(x == 90)
    mkReport
  }
}

object SumIfRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with SumIf
object SumIfRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with SumIf
trait SumIf extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val y = DenseVector(true, false, true, false, true, false, false, false, true, true)
    val x = sumIf(0,10) { y(_) } { i => DenseVector.ones(5) }
    //x.pprint
		collect(x == DenseVector(5.0, 5.0, 5.0, 5.0, 5.0))
    mkReport
  }
}

// object AggregateIfRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with AggregateIf
// object AggregateIfRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with AggregateIf
// trait AggregateIf extends ForgeTestModule with OptiMLApplication {
//   def main() = {

// 		val x = aggregateIf(0, 10) { i => i > 4 } { i =>
// 			i*2
// 		}
// 		collect(x == DenseVector(10, 12, 14, 16, 18))
//     mkReport
//   }
// }

// object Aggregate2dRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Aggregate2d
// object Aggregate2dRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Aggregate2d
// trait Aggregate2d extends ForgeTestModule with OptiMLApplication {
//   def main() = {

//     val x = aggregate(10::13, 3::6) { (i,j) =>
//       i*j
//     }
//     collect(x == DenseVector(30, 40, 50, 33, 44, 55, 36, 48, 60))
//     mkReport
//   }
// }

// object Aggregate2dIfRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Aggregate2dIf
// object Aggregate2dIfRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Aggregate2dIf
// trait Aggregate2dIf extends ForgeTestModule with OptiMLApplication {
//   def main() = {

//     val x = aggregateIf(0::3, 0::4) { (i,j) => i < j } { (i,j) =>
//       i*j
//     }
//     collect(x == DenseVector(0, 0, 0, 2, 3, 6))
//     mkReport
//   }
// }

object IndexVectorConstructRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with IndexVectorConstruct
object IndexVectorConstructRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with IndexVectorConstruct
trait IndexVectorConstruct extends ForgeTestModule with OptiMLApplication {
  def main() = {

		val x = (0::10) { i => i*2 }
		collect(x == DenseVector(0,2,4,6,8,10,12,14,16,18))
    mkReport
  }
}

object IndexVectorConstruct2RunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with IndexVectorConstruct2
object IndexVectorConstruct2RunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with IndexVectorConstruct2
trait IndexVectorConstruct2 extends ForgeTestModule with OptiMLApplication {
  def main() = {

		val x = (0::10, *) { i => DenseVector.ones(2) * i	}
		collect(x == DenseMatrix((0.,0.), (1.,1.), (2.,2.), (3.,3.), (4.,4.), (5.,5.), (6., 6.), (7.,7.), (8.,8.), (9.,9.)))

		val y = (0::2, 0::3) { (i,j) =>	i*j	}
		collect(y == DenseMatrix((0,0,0),(0,1,2)))

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

