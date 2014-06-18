import optiql.compiler._
import optiql.shared._
import optiql.library._
import ppl.tests.scalatest._
import scala.virtualization.lms.common.Record


trait TestRecord extends OptiQLApplication {

  type Item = Record {
    val id: Int
    val quantity: Int
    val price: Double
    val status: Char
  }

  def Item(_id: Rep[Int], _quantity: Rep[Int], _price: Rep[Double], _status: Rep[Char]) = new Record {
    val id = _id
    val quantity = _quantity
    val price = _price
    val status = _status
  }

  lazy val items = Table(Item(0, 10, 2.49, 'N'), Item(1, 0, 49.95, 'B'), Item(2, 1000, 0.99, 'N'), Item(3, 18, 5.99, 'S'))
  val itemsSize = 4

  lazy val items2 = Table(Item(0, 10, 2.49, 'N'), Item(1, 0, 49.95, 'B'), Item(2, 1000, 0.99, 'N'), Item(3, 18, 5.99, 'S'), Item(4, 22, 2.99, 'N'))

  lazy val emptyTable = Table[Item]()

  //allow some floating point error
  def approx(lhs: Rep[Double], rhs: Rep[Double]) = {
    def abs(value: Rep[Double]) = if (value < 0) 0-value else value
    abs(lhs - rhs) < .001
  }

}

object QueryableSelectRunnerC extends ForgeTestRunnerCompiler with OptiQLApplicationCompiler with QueryableSelectTest
object QueryableSelectRunnerI extends ForgeTestRunnerInterpreter with OptiQLApplicationInterpreter with QueryableSelectTest
trait QueryableSelectTest extends ForgeTestModule with OptiQLApplication with TestRecord {
  def main() = {
    val scalarResult = items Select (item => item.id)
    
    collect(scalarResult.size == itemsSize)
    for (i <- 0 until itemsSize) {
      collect(scalarResult(i) == i)
    }

    val recordResult = items Select(item => new Record {
      val id = item.id
      val maxRevenue = item.quantity * item.price
    })

    collect(recordResult.size == itemsSize)
    for (i <- 0 until itemsSize) {
      collect(recordResult(i).id == i)
    }

    collect(approx(recordResult(0).maxRevenue, 24.9))
    collect(recordResult(1).maxRevenue == 0)
    collect(approx(recordResult(2).maxRevenue, 990))
    collect(approx(recordResult(3).maxRevenue, 107.82))
    mkReport
  }
}

object QueryableWhereRunnerC extends ForgeTestRunnerCompiler with OptiQLApplicationCompiler with QueryableWhereTest
object QueryableWhereRunnerI extends ForgeTestRunnerInterpreter with OptiQLApplicationInterpreter with QueryableWhereTest
trait QueryableWhereTest extends ForgeTestModule with OptiQLApplication with TestRecord {
  def main() = {
    val result = items Where(_.status == 'N') Select(item => new Record {
      val id = item.id
      val maxRevenue = item.quantity * item.price
    })

    collect(result.size == 2)

    collect(result.First.id == 0)
    collect(result.Last.id == 2)

    collect(approx(result.First.maxRevenue, 24.9))
    collect(approx(result.Last.maxRevenue, 990))

    val res2 = items Where(_.status == 'T') Select(item => item.id)
    collect(res2.size == 0)
    mkReport
  }
}

object QueryableReduceRunnerC extends ForgeTestRunnerCompiler with OptiQLApplicationCompiler with QueryableReduceTest
object QueryableReduceRunnerI extends ForgeTestRunnerInterpreter with OptiQLApplicationInterpreter with QueryableReduceTest
trait QueryableReduceTest extends ForgeTestModule with OptiQLApplication with TestRecord {
  def main() = {
    val sumQuantity = items Sum(_.quantity)
    collect(sumQuantity == 1028)

    val minQuantity = items Min(_.price)
    collect(minQuantity == 0.99)

    val maxId = items Max(_.id)
    collect(maxId == 3)

    val avgPrice = items Average(_.price)
    collect(approx(avgPrice, 14.855))
    mkReport
  }
}

object QueryableGroupByReduceRunnerC extends ForgeTestRunnerCompiler with OptiQLApplicationCompiler with QueryableGroupByReduceTest
object QueryableGroupByReduceRunnerI extends ForgeTestRunnerInterpreter with OptiQLApplicationInterpreter with QueryableGroupByReduceTest
trait QueryableGroupByReduceTest extends ForgeTestModule with OptiQLApplication with TestRecord {
  def main() = {
    val res1 = items GroupBy(_.status) Select(g => new Record {
      val status = g.key
      val sumQuantity = g.values.Sum(_.quantity)
      val minPrice = g.values.Min(_.price)
      val count = g.values.Count
    })
    collect(res1.size == 3)
    collect(res1(0).status == 'N' && res1(0).sumQuantity == 1010 && res1(0).minPrice == 0.99 && res1(0).count == 2)
    collect(res1(1).status == 'B' && res1(1).sumQuantity == 0 && res1(1).minPrice == 49.95 && res1(1).count == 1)
    collect(res1(2).status == 'S' && res1(2).sumQuantity == 18 && res1(2).minPrice == 5.99 && res1(2).count == 1)

    val res2 = items Where(_.quantity > 0) GroupBy(_.status) Select(g => new Record {
      val status = g.key
      val sumQuantity = g.values.Sum(_.quantity)
      val maxQuantity = g.values.Max(_.quantity)
      val avgPrice = g.values.Average(_.price)
      val count = g.values.Count
    })
    collect(res2.size == 2)
    collect(res2.First.status == 'N' && res2.First.sumQuantity == 1010 && res2.First.maxQuantity == 1000 && approx(res2.First.avgPrice, 1.74) && res2.First.count == 2)
    collect(res2.Last.status == 'S' && res2.Last.sumQuantity == 18 && res2.Last.maxQuantity == 18 && approx(res2.Last.avgPrice, 5.99) && res2.Last.count == 1)

    val res3 = items Where(_.status == 'T') GroupBy(_.status) Select(g => g.key)
    collect(res3.size == 0)

    //val res4 = emptyTable GroupBy (_.status) Select(g => g.key) //FIXME: empty tables are marked mutable by the effects system
    //collect(res4.size == 0)
    mkReport
  }
}

object QueryableGroupByRunnerC extends ForgeTestRunnerCompiler with OptiQLApplicationCompiler with QueryableGroupByTest
object QueryableGroupByRunnerI extends ForgeTestRunnerInterpreter with OptiQLApplicationInterpreter with QueryableGroupByTest
trait QueryableGroupByTest extends ForgeTestModule with OptiQLApplication with TestRecord {
  def main() = {
    val res = items GroupBy(_.status) SelectMany(g => g.values.Select(_.quantity))
    collect(res.size == items.size)
    collect(res(0) == 10) //N
    collect(res(1) == 1000) //N
    collect(res(2) == 0) //B
    collect(res(3) == 18) //S
    mkReport
  }
}

object QueryableSortRunnerC extends ForgeTestRunnerCompiler with OptiQLApplicationCompiler with QueryableSortTest
object QueryableSortRunnerI extends ForgeTestRunnerInterpreter with OptiQLApplicationInterpreter with QueryableSortTest
trait QueryableSortTest extends ForgeTestModule with OptiQLApplication with TestRecord {
  def main() = {
    val sort1 = items OrderBy(asc(_.id))
    for (i <- 0 until itemsSize) {
      collect(sort1(i).id == i)
    }

    val sort2 = items OrderBy(asc(_.quantity), desc(_.price))
    collect(sort2(0).quantity == 0)
    collect(sort2(1).quantity == 10)
    collect(sort2(2).quantity == 18)
    collect(sort2(3).quantity == 1000)

    val sort3 = items OrderBy(desc(_.status), asc(_.quantity))
    collect(sort3(0).status == 'S')
    collect(sort3(1).status == 'N' && sort3(1).quantity == 10)
    collect(sort3(2).status == 'N' && sort3(2).quantity == 1000)
    collect(sort3(3).status == 'B')
    mkReport
  }
}

object QueryableJoinRunnerC extends ForgeTestRunnerCompiler with OptiQLApplicationCompiler with QueryableJoinTest
object QueryableJoinRunnerI extends ForgeTestRunnerInterpreter with OptiQLApplicationInterpreter with QueryableJoinTest
trait QueryableJoinTest extends ForgeTestModule with OptiQLApplication with TestRecord {
  def main() = {
    val res = items.Join(items2)(_.id, _.id)((a,b) => new Record {
      val id = a.id
      val quantity = b.quantity
    })

    collect(res.size == items.size)
    collect(res(0).id == 0 && res(0).quantity == 10)
    collect(res(1).id == 1 && res(1).quantity == 0)
    collect(res(2).id == 2 && res(2).quantity == 1000)
    collect(res(3).id == 3 && res(3).quantity == 18)
    mkReport
  }
}


class QuerySuiteInterpreter extends ForgeSuiteInterpreter {
  def testSelect() { runTest(QueryableSelectRunnerI) }
  def testWhere() { runTest(QueryableWhereRunnerI) }
  def testReduce() { runTest(QueryableReduceRunnerI) }
  def testGroupBy() { runTest(QueryableGroupByRunnerI) }
  def testGroupByReduce() { runTest(QueryableGroupByReduceRunnerI) }
  def testSort() { runTest(QueryableSortRunnerI) }
  def testJoin() { runTest(QueryableJoinRunnerI) }
}

class QuerySuiteCompiler extends ForgeSuiteCompiler {
  def testSelect() { runTest(QueryableSelectRunnerC) }
  def testWhere() { runTest(QueryableWhereRunnerC) }
  def testReduce() { runTest(QueryableReduceRunnerC) }
  def testGroupBy() { runTest(QueryableGroupByRunnerC) }
  def testGroupByReduce() { runTest(QueryableGroupByReduceRunnerC) }
  def testSort() { runTest(QueryableSortRunnerC) }
  def testJoin() { runTest(QueryableJoinRunnerC) }
}
