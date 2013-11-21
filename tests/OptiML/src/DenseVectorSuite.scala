/* Unit tests for OptiML vectors.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 22, 2010
 * modified: Mar 31, 2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import optiml.compiler._
import optiml.library._
import optiml.shared._
import ppl.tests.scalatest._

object DenseVectorAccessorsRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with DenseVectorAccessors
object DenseVectorAccessorsRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with DenseVectorAccessors
trait DenseVectorAccessors extends ForgeTestModule with OptiMLApplication {
  def main() {
    val v = DenseVector.rand(1000)

    collect(v.length == 1000)

    val elem = v(92)
    collect(v(92) == elem)

    val first = v.first
    collect(first == v(0))

    val last = v.last
    collect(last == v(v.length-1))

    val twenty = v.slice(30, 50)
    collect(twenty.length == 20)
    var i = 0
    while (i < twenty.length){
      collect(twenty(i) == v(i+30))
      i += 1
    }

    val firstTen = v.take(10)
    collect(firstTen.length == 10)
    i = 0
    while (i < firstTen.length){
      collect(firstTen(i) == v(i))
      i += 1
    }

    val allExceptTen = v.drop(10)
    collect(allExceptTen.length == (v.length - 10))
    i = 0
    while (i < allExceptTen.length){
      collect(allExceptTen(i) == v(i+10))
      i += 1
    }

    val v2 = DenseVector(1,2,3,4,5)
    val vSlice = v2(0::2)
    collect(vSlice == DenseVector(1,2))
    val vSlice2 = v2(3::5)
    collect(vSlice2 == DenseVector(4,5))
    val vSlice3 = v2(IndexVector((4,2,0)))
    collect(vSlice3 == DenseVector(5,3,1))

    mkReport
  }
}

object DenseVectorOperatorsRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with DenseVectorOperators
object DenseVectorOperatorsRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with DenseVectorOperators
trait DenseVectorOperators extends ForgeTestModule with OptiMLApplication {
  def main() {
    val v = DenseVector.rand(1000)

    val vt = v.t
    collect(vt.isRow != v.isRow)

    val vc = v.Clone
    collect(vc == v)

    val v2 = DenseVector(1,2,3,4,5)
    collect(median(v2) == 3)
    collect(mean(v2) == 3)
    collect(max(v2) == 5)
    collect(min(v2) == 1)
    collect(mean(3,6,2,5) == 4.0)

    mkReport
  }
}

object DenseVectorUpdatesRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with DenseVectorUpdates
object DenseVectorUpdatesRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with DenseVectorUpdates
trait DenseVectorUpdates extends ForgeTestModule with OptiMLApplication {
  def main() {
    val v = DenseVector.rand(1000).mutable
    val vb = DenseVector.rand(10).mutable

    v(7) = 0.9123
    collect(v(7) == 0.9123)

    val twov = (v << v)
    collect(twov.length == v.length*2)
    collect(twov(1000) == v(0))

    var vlen = v.length
    v <<= 9.2
    collect(v.length == vlen + 1)
    collect(v(vlen) == 9.2)
    vlen += 1

    v <<= vb
    collect(v.length == vlen+vb.length)
    vlen += vb.length
    var i = 0
    while (i < vb.length){
      collect(v(vlen-vb.length+i) == vb(i))
      i += 1
    }

    v.copyFrom(100, vb)
    i = 0
    while (i < vb.length){
      collect(v(i+100) == vb(i))
      i += 1
    }

    v.insert(500, 9.21)
    collect(v.length == vlen+1)
    collect(v(500) == 9.21)
    vlen += 1

    v.insertAll(13, vb)
    collect(v.length == vlen + vb.length)
    i = 0
    while (i < vb.length){
      collect(v(i+13) == vb(i))
      i += 1
    }
    vlen += vb.length

    var shifted = v(72)
    v.remove(71)
    collect(v.length == vlen-1)
    collect(v(71) == shifted)
    vlen -= 1

    shifted = v(102)
    v.removeAll(99,3)
    collect(v.length == vlen-3)
    collect(v(99) == shifted)
    vlen -= 3

    v.trim
    collect(v.length == vlen)

    // vector of struct
    val a1 = DenseVector(1,2,3,4)
    val a = DenseVector[DenseVector[Int]](0, true)

    // infers Any without being passed explicitly when "<<=" is infix. why?
    // infix_<<=[DenseVector[Int]](a,a1)
    a <<= a1
    collect (a(0).apply(2) == 3)

    a(0) = DenseVector(5,6,7,8)
    collect (a(0).apply(2) == 7)

    mkReport
  }
}

object DenseVectorRangeRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with DenseVectorRange
object DenseVectorRangeRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with DenseVectorRange
trait DenseVectorRange extends ForgeTestModule with OptiMLApplication {
  def main() {
    val rangeEasy = (0::1000)
    val rangeHard = DenseVector.uniform(11, 2, 100)

    collect(rangeEasy(0) == 0)
    collect(rangeEasy(500) == 500)
    collect(rangeEasy(999) == 999)
    collect(rangeEasy.length == 1000)

    collect(rangeHard(0) == 11.0)
    collect(rangeHard(1) == 13.0)
    collect(rangeHard(2) == 15.0)
    collect(rangeHard(44) == 99.0)
    collect(rangeHard.length == 45)

    mkReport
  }
}

object InitRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Init
object InitRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Init
trait Init extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v1 = DenseVector(1,2,3,4,5)
    val v2 = DenseVector(1.,2.,3.,4.,5.)

    // just make sure we can reach here without an error
    collect(true)
    mkReport
  }
}

object LoopRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Loop
object LoopRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Loop
trait Loop extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val vec1 = DenseVector.rand(5)
    val vec2 = DenseVector.rand(5).mutable

    var idx = 0
    while(idx < 5) {
      vec2(idx) = vec1(idx)
      idx += 1
    }
    collect(vec2 == vec1)

    mkReport
  }
}

object CountRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Count
object CountRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Count
trait Count extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = DenseVector(1,2,3,5,5,5,7,8,9,10)
    val c = v.count { _ == 5 }
    collect(c == 3)
    mkReport
  }
}

object BulkUpdateRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with BulkUpdate
object BulkUpdateRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with BulkUpdate
trait BulkUpdate extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = DenseVector.zeros(10).mutable
    val i = (0::5)
    v(i) = 1
    collect(v == DenseVector[Double](1,1,1,1,1,0,0,0,0,0))
    mkReport
  }
}

object FindRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Find
object FindRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Find
trait Find extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = DenseVector(1,2,3,5,5,5,7,8,9,10)
    val i: Rep[DenseVector[Int]] = v.find { _ == 5 }  // AKS FIXME: the override for __equal doesn't work with IndexVectorDense because of the higher kinded type
    collect(i == DenseVector(3,4,5))
    mkReport
  }
}

object DistRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Dist
object DistRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Dist
trait Dist extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v1 = DenseVector(10.,10.,5.,5.,0.)
    val v2 = DenseVector(5.,5.,10.,10.,-5.)

    collect(dist(v1,v2) == 25)
    mkReport
  }
}

object DistinctRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Distinct
object DistinctRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Distinct
trait Distinct extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v1 = DenseVector(10.,10.,5.,5.,0.)

    collect(v1.contains(5.))
    collect(!v1.contains(7.5))
    collect(v1.distinct == DenseVector(10.,5.,0.))
    mkReport
  }
}

object MedianRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Median
object MedianRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Median
trait Median extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = DenseVector(1,5,3,4,2,6,7,8,9)
    collect(v.median == 5)
    mkReport
  }
}

// object NearestNeighborRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with NearestNeighbor
// object NearestNeighborRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with NearestNeighbor
// trait NearestNeighbor extends ForgeTestModule with OptiMLApplication {
//   def main() = {

//     val m = DenseMatrix((1,1,1,1), (9,9,9,9), (-2,-2,-2,-2), (0,0,0,0), (1,1,1,1))
//     val nearestUnique = nearestNeighborIndex(0, m, false)
//     collect(nearestUnique == 3)
//     val nearestAny = nearestNeighborIndex(0, m)
//     collect(nearestAny == 4)
//     mkReport
//   }
// }

// object SampleRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with Sample
// object SampleRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with Sample
// trait Sample extends ForgeTestModule with OptiMLApplication {
//   def main() = {

//     val v = (0::100)
//     val vs = sample[Int,DenseVector[Int]](v, 10)
//     //val vs = sample(v, 10)
//     vs foreach { e => collect(v contains e) }
//     mkReport
//   }
// }

object GroupByRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with GroupBy
object GroupByRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with GroupBy
trait GroupBy extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = DenseVector("one", "two", "two", "three", "four", "three", "four", "three", "four", "four")
    val vs = v.groupBy(e=>e)
    collect(vs.length == 4)
    for (v <- vs) {
      if (v(0) == "one") collect(v.length == 1)
      else if (v(0) == "two") collect(v.length == 2)
      else if (v(0) == "three") collect(v.length == 3)
      else if (v(0) == "four") collect(v.length == 4)
    }
    mkReport
  }
}

object SimpleFlatMapRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with SimpleFlatMap
object SimpleFlatMapRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with SimpleFlatMap
trait SimpleFlatMap extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val x = DenseVector(1,2,3,4,5)
    val y = x flatMap { e => DenseVector.zeros(e) }
    collect(y.length == x.sum)
    mkReport
  }
}

object SimpleFlattenRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with SimpleFlatten
object SimpleFlattenRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with SimpleFlatten
trait SimpleFlatten extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val x = DenseVector((1,2,3,4,5), (6,7,8,9))
    val y = DenseVector.flatten(x)
    collect(y == DenseVector(1,2,3,4,5,6,7,8,9))
    mkReport
  }
}

class DenseVectorSuiteInterpreter extends ForgeSuiteInterpreter {
  def testAccessors() { runTest(DenseVectorAccessorsRunnerI) }
  def testOperators() { runTest(DenseVectorOperatorsRunnerI) }
  def testUpdates() { runTest(DenseVectorUpdatesRunnerI) }
  def testRange() { runTest(DenseVectorRangeRunnerI) }

  def testInit() { runTest(InitRunnerI) }
  def testLoop() { runTest(LoopRunnerI) }
  def testCount() { runTest(CountRunnerI) }
  def testBulkUpdate() { runTest(BulkUpdateRunnerI) }
  def testFind() { runTest(FindRunnerI) }
  def testDist() { runTest(DistRunnerI) }
  def testDistinct() { runTest(DistinctRunnerI) }
  def testMedian() { runTest(MedianRunnerI) }
  // def testNearestNeighbor() { runTest(NearestNeighborRunnerI) }
  // def testSample() { runTest(SampleRunnerI) }
  def testGroupBy() { runTest(GroupByRunnerI) }
  def testFlatMap() { runTest(SimpleFlatMapRunnerI) }
  def testFlatten() { runTest(SimpleFlattenRunnerI) }
}

class DenseVectorSuiteCompiler extends ForgeSuiteCompiler {
  def testAccessors() { runTest(DenseVectorAccessorsRunnerC) }
  def testOperators() { runTest(DenseVectorOperatorsRunnerC) }
  def testUpdates() { runTest(DenseVectorUpdatesRunnerC) }
  def testRange() { runTest(DenseVectorRangeRunnerC) }

  def testInit() { runTest(InitRunnerC) }
  def testLoop() { runTest(LoopRunnerC) }
  def testCount() { runTest(CountRunnerC) }
  def testBulkUpdate() { runTest(BulkUpdateRunnerC) }
  def testFind() { runTest(FindRunnerC) }
  def testDist() { runTest(DistRunnerC) }
  def testDistinct() { runTest(DistinctRunnerC) }
  def testMedian() { runTest(MedianRunnerC) }
  // def testNearestNeighbor() { runTest(NearestNeighborRunnerC) }
  // def testSample() { runTest(SampleRunnerC) }
  def testGroupBy() { runTest(GroupByRunnerC) }
  def testFlatMap() { runTest(SimpleFlatMapRunnerC) }
  def testFlatten() { runTest(SimpleFlattenRunnerC) }
}

