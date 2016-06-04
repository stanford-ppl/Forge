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

object DenseVectorAccessorsRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with DenseVectorAccessors
object DenseVectorAccessorsRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with DenseVectorAccessors
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
    val vSlice3 = v2(IndexVector(DenseVector(4,2,0)))
    collect(vSlice3 == DenseVector(5,3,1))

    mkReport
  }
}

object DenseVectorOperatorsRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with DenseVectorOperators
object DenseVectorOperatorsRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with DenseVectorOperators
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
    collect(v2(v2.maxIndex) == 5)
    collect(min(v2) == 1)
    collect(v2(v2.minIndex) == 1)
    collect(mean(3,6,2,5) == 4.0)

    mkReport
  }
}

object DenseVectorUpdatesRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with DenseVectorUpdates
object DenseVectorUpdatesRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with DenseVectorUpdates
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

object DenseVectorRangeRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with DenseVectorRange
object DenseVectorRangeRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with DenseVectorRange
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

object InitRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Init
object InitRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Init
trait Init extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v1 = DenseVector(1,2,3,4,5)
    val v2 = DenseVector(1.0,2.0,3.0,4.0,5.0)

    // just make sure we can reach here without an error
    collect(true)
    mkReport
  }
}

object LoopRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Loop
object LoopRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Loop
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

object CountRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Count
object CountRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Count
trait Count extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = DenseVector(1,2,3,5,5,5,7,8,9,10)
    val c = v.count { _ == 5 }
    collect(c == 3)
    mkReport
  }
}

object BulkUpdateRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with BulkUpdate
object BulkUpdateRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with BulkUpdate
trait BulkUpdate extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = DenseVector.zeros(10).mutable
    val i = (0::5)
    v(i) = 1
    collect(v == DenseVector[Double](1,1,1,1,1,0,0,0,0,0))
    mkReport
  }
}

object FindRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Find
object FindRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Find
trait Find extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = DenseVector(1,2,3,5,5,5,7,8,9,10)
    val i: Rep[DenseVector[Int]] = v.find { _ == 5 }  // AKS FIXME: the override for __equal doesn't work with IndexVectorDense because of the higher kinded type
    collect(i == DenseVector(3,4,5))
    mkReport
  }
}

object DistRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Dist
object DistRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Dist
trait Dist extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v1 = DenseVector(10.0,10.0,5.0,5.0,0.0)
    val v2 = DenseVector(5.0,5.0,10.0,10.0,-5.0)

    collect(dist(v1,v2) == 25)
    mkReport
  }
}

object DistinctRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Distinct
object DistinctRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Distinct
trait Distinct extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v1 = DenseVector(10.0,10.0,5.0,5.0,0.0)
    collect(v1.contains(5.0))
    collect(!v1.contains(7.5))

    val v2 = v1.distinct
    collect(v2.length == 3)
    collect(v2.contains(10.0))
    collect(v2.contains(5.0))
    collect(v2.contains(0.0))

    // NOTE: distinct on vector of vectors is not currently supported as hashcode is not defined properly.

    val v3 = DenseVector(pack((unit(3),unit(2),unit(1))),
                         pack((unit(1),unit(2),unit(3))),
                         pack((unit(7),unit(7),unit(7))),
                         pack((unit(3),unit(2),unit(1))),
                         pack((unit(3),unit(2),unit(1))),
                         pack((unit(1),unit(5),unit(2))),
                         pack((unit(7),unit(7),unit(7))))
    collect(v3.distinct.length == 4)

    val v4 = DenseVector("321",
                         "123",
                         "3210",
                         "321",
                         "152",
                         "3210",
                         "777",
                         "123")
    collect(v4.distinct.length == 5)
    mkReport
  }
}

object IntersectRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Intersect
object IntersectRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Intersect
trait Intersect extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v1 = DenseVector(10.0,10.0,11.0,5.0,5.0,-3.0)
    val v2 = DenseVector(5.0, -3.0)
    val i = v1 intersect v2

    // duplicates are removed
    collect(i.sort == DenseVector(-3.0, 5.0))

    mkReport
  }
}

object MedianRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Median
object MedianRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Median
trait Median extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = DenseVector(1,5,3,4,2,6,7,8,9)
    collect(v.median == 5)
    mkReport
  }
}

// object NearestNeighborRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with NearestNeighbor
// object NearestNeighborRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with NearestNeighbor
// trait NearestNeighbor extends ForgeTestModule with OptiMLApplication {
//   def main() = {

//     val m = DenseMatrix(DenseVector(1,1,1,1), DenseVector(9,9,9,9), DenseVector(-2,-2,-2,-2), DenseVector(0,0,0,0), DenseVector(1,1,1,1))
//     val nearestUnique = nearestNeighborIndex(0, m, false)
//     collect(nearestUnique == 3)
//     val nearestAny = nearestNeighborIndex(0, m)
//     collect(nearestAny == 4)
//     mkReport
//   }
// }

object SampleRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Sample
object SampleRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Sample
trait Sample extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = (0::100)
    val vs = sample(v, 0.1)
    vs foreach { e => collect(v contains e) }
    mkReport
  }
}

object GroupByRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with GroupBy
object GroupByRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with GroupBy
trait GroupBy extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val v = DenseVector("one", "two", "two", "three", "four", "three", "four", "three", "four", "four")
    val vs = v.groupBy(k => k,v => v).toVector
    collect(vs.length == 4)
    for (v <- vs) {
      if (v(0) == "one") collect(v.length == 1)
      else if (v(0) == "two") collect(v.length == 2)
      else if (v(0) == "three") collect(v.length == 3)
      else if (v(0) == "four") collect(v.length == 4)
    }

    val v2 = DenseVector(DenseVector(1,2),DenseVector(3,4),DenseVector(5,6))
    val g2 = v2.groupBy(e => sum(e), e => e)
    collect(g2(7).apply(0) == DenseVector(3,4))
    collect(!g2.contains(5))

    mkReport
  }
}

object SimpleFlatMapRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with SimpleFlatMap
object SimpleFlatMapRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with SimpleFlatMap
trait SimpleFlatMap extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val x = DenseVector(1,2,3,4,5)
    val y = x flatMap { e => DenseVector.zeros(e) }
    collect(y.length == x.sum)
    mkReport
  }
}

object SimpleFlattenRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with SimpleFlatten
object SimpleFlattenRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with SimpleFlatten
trait SimpleFlatten extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val x = DenseVector(DenseVector(1,2,3,4,5), DenseVector(6,7,8,9))
    val y = DenseVector.flatten(x)
    collect(y == DenseVector(1,2,3,4,5,6,7,8,9))
    mkReport
  }
}

object PartitionRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Partition
object PartitionRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Partition
trait Partition extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val x = DenseVector.rand(100)
    val (a,b) = unpack(x.partition(_ > 0.5))
    collect(sum(abs((a << b).sort - x.sort)) < 0.01)

    // We had issues with broken fusion of filters and partitions, so test that it's working here.
    val xf = x.filter(_ > 0.2)
    val (a2,b2) = unpack(xf.partition(_ > 0.7))
    collect(sum(abs((a2 << b2).sort - xf.sort)) < 0.01)
    mkReport
  }
}

object SortRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Sort
object SortRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Sort
trait Sort extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val v = DenseVector(DenseVector(3,5,-31),DenseVector(-5,6,17),DenseVector(0,-12,2))
    val sorted = v.sortBy(r => r(0))
    val sortedKey = DenseVector(DenseVector(-5,6,17),DenseVector(0,-12,2),DenseVector(3,5,-31))

    // Nested DenseVector equality doesn't work at the moment, because the inner type is T
    // and implemented by reference equality.
    // collect(sorted == sortedKey)
    collect((0::sorted.length).forall(i => sorted(i) == sortedKey(i)))

    val v2 = DenseVector(3.2,-6.9,0.0,0.1,-71.5,3.14,2.96)
    val (sortedV2, sortedIndices) = v2.sortWithIndex
    collect(sortedIndices == IndexVector(DenseVector(4,1,2,3,6,5,0)))
    collect(v2(sortedIndices) == sortedV2)

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
  def testIntersect() { runTest(IntersectRunnerI) }
  def testMedian() { runTest(MedianRunnerI) }
  // def testNearestNeighbor() { runTest(NearestNeighborRunnerI) }
  def testSample() { runTest(SampleRunnerI) }
  def testGroupBy() { runTest(GroupByRunnerI) }
  def testFlatMap() { runTest(SimpleFlatMapRunnerI) }
  def testFlatten() { runTest(SimpleFlattenRunnerI) }
  def testPartition() { runTest(PartitionRunnerI) }
  def testSort() { runTest(SortRunnerI) }
}

class DenseVectorSuiteCompiler extends ForgeSuiteCompiler {
  cppWhiteList ++= Seq("sortindex_helper")
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
  def testIntersect() { runTest(IntersectRunnerC) }
  def testMedian() { runTest(MedianRunnerC) }
  // def testNearestNeighbor() { runTest(NearestNeighborRunnerC) }
  def testSample() { runTest(SampleRunnerC) }
  def testGroupBy() { runTest(GroupByRunnerC) }
  def testFlatMap() { runTest(SimpleFlatMapRunnerC) }
  def testFlatten() { runTest(SimpleFlattenRunnerC) }
  def testPartition() { runTest(PartitionRunnerC) }
  def testSort() { runTest(SortRunnerC) }
}
