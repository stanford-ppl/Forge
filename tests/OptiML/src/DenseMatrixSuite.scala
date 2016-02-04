/* Unit tests for OptiML matrices.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 22, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import optiml.compiler._
import optiml.library._
import optiml.shared._
import ppl.tests.scalatest._

object DenseMatrixAccessorsRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with DenseMatrixAccessors
object DenseMatrixAccessorsRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with DenseMatrixAccessors
trait DenseMatrixAccessors extends ForgeTestModule with OptiMLApplication {
  def main() {
    val m = DenseMatrix.rand(100,100)

    collect(m.numRows == 100)
    collect(m.numCols == 100)
    collect(m.size == m.numRows*m.numCols)

    val elem = m(92,10)
    collect(m(92,10) == elem)

    val row = m(37)
    collect(row.length == m.numCols)
    var j = 0
    while (j < m.numCols){
      collect(row(j) == m(37,j))
      j += 1
    }

    val col = m.getCol(52)
    collect(col.length == m.numRows)
    j = 0
    while (j < m.numRows){
      collect(col(j) == m(j,52))
      j += 1
    }

    val mat = m.sliceRows(3,5)
    var i = 0;
    j = 0
    while (i < mat.numRows){
      while (j < mat.numCols){
        collect(mat(i,j) == m(i+3,j))
        j += 1
      }
      i += 1
    }

    val m2 = (0::10,0::10) { (i,j) => i+j }
    val mSlice = m2(0::2,4::5)
    collect(mSlice == DenseMatrix(DenseVector(4,5)).t)
    val mSlice2 = m2(3::4)
    collect(mSlice2 == DenseMatrix(DenseVector(3,4,5,6,7,8,9,10,11,12)))
    val mSlice3 = m2(*,0::2)
    collect(mSlice3 == DenseMatrix(DenseVector(0,1,2,3,4,5,6,7,8,9),DenseVector(1,2,3,4,5,6,7,8,9,10)).t)
    val mSlice4 = m2(1::4,*)
    collect(mSlice4 == DenseMatrix(DenseVector(1,2,3,4,5,6,7,8,9,10),DenseVector(2,3,4,5,6,7,8,9,10,11),DenseVector(3,4,5,6,7,8,9,10,11,12)))
    val mSlice5 = m2(IndexVector(DenseVector(3,1,2)),IndexVector(DenseVector(4,0,6)))
    collect(mSlice5 == DenseMatrix(DenseVector(7,3,9),DenseVector(5,1,7),DenseVector(6,2,8)))

    mkReport
  }
}

object DenseMatrixOperatorsRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with DenseMatrixOperators
object DenseMatrixOperatorsRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with DenseMatrixOperators
trait DenseMatrixOperators extends ForgeTestModule with OptiMLApplication {
  def main() {
    val m_rand = DenseMatrix.rand(2,2)
    collect(m_rand(0,0) != m_rand(0,1))
    collect(m_rand(0,0) != m_rand(1,0))
    collect(m_rand(0,0) != m_rand(1,1))

    val m = DenseMatrix(DenseVector(1,2,3,4,5),DenseVector(1,2,3,4,5))
    collect(mean(m) == 3)
    collect(max(m) == 5)
    collect(min(m) == 1)

    mkReport
  }
}

object DenseMatrixUpdatesRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with DenseMatrixUpdates
object DenseMatrixUpdatesRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with DenseMatrixUpdates
trait DenseMatrixUpdates extends ForgeTestModule with OptiMLApplication {
  def main() {
    val v = DenseVector.rand(100)
    val m = DenseMatrix.rand(100,100).mutable
    val mb = DenseMatrix.rand(100,100).mutable

    val init_m = m.Clone

    m(72,5) = 3.14
    collect(m(72,5) == 3.14)

    m(3) = v
    var j = 0
    while (j < m.numCols){
      collect(v(j) == m(3,j))
      j += 1
    }

    var rows = m.numRows
    m.insertRow(6,v)
    collect(m.numRows == rows+1)
    j = 0
    while (j < m.numCols){
      collect(m(6,j) == v(j))
      j += 1
    }
    rows += 1

    m.insertAllRows(72,mb)
    collect(m.numRows == rows+mb.numRows)
    var i = 0
    j = 0
    while (i < mb.numRows){
      while (j < mb.numCols){
        collect(m(i+72,j) == mb(i,j))
        j += 1
      }
      i += 1
    }
    rows += mb.numRows

    val m2 = init_m.mutable
    rows = m2.numRows
    var cols = m.numCols
    m2.insertCol(17,v)
    collect(m2.numCols == cols+1)
    j = 0
    while (j < m2.numRows){
      collect(m2(j,17) == v(j))
      j += 1
    }
    cols += 1

    m2.insertAllCols(99,mb)
    collect(m2.numCols == cols+mb.numCols)
    i = 0
    j = 0
    while (i < mb.numRows){
      while (j < mb.numCols){
        collect(m2(i,j+99) == mb(i,j))
        j += 1
      }
      i += 1
    }
    cols += mb.numCols

    val s_row = m2(20).Clone
    m2.removeRows(10,10)
    collect(m2.numRows == rows-10)
    collect(s_row == m2(10))
    rows -= 10

    val s_col = m2.getCol(23).Clone
    m2.removeCols(13,10)
    collect(m2.numCols == cols-10)
    collect(s_col == m2.getCol(13))
    cols -= 10

    mkReport
  }
}

object GroupRowsByRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with GroupRowsBy
object GroupRowsByRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with GroupRowsBy
trait GroupRowsBy extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val m = DenseMatrix(DenseVector(1,2,3,4),
                        DenseVector(2,-2,-3,-4),
                        DenseVector(1,5,6,7),
                        DenseVector(2,-5,-6,-7))

    val ms = m.groupRowsBy(row => row(0)).toVector
    collect(ms.length == 2)
    for (m <- ms) {
      collect(m.numRows == 2)
      collect(m.numCols == 4)
    }
    mkReport
  }
}

object MapAllRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with MapAll
object MapAllRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with MapAll
trait MapAll extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val x = DenseMatrix.zeros(10,10)

    val a = x.map(e => e + 2.0)
    collect (a == (DenseMatrix.ones(10,10)*2.0))

    // Triggers fusion bug -- see https://github.com/stanford-ppl/Delite/issues/45
    // val b = x mapRows { i => DenseVector.ones(10) }
    val b = x mapRows { i => DenseVector.ones(11) }
    collect (b == DenseMatrix.ones(10,11))

    val c = x mapRows { i => DenseVector.ones(5) }
    collect (c == DenseMatrix.ones(10,5))

    val d = x mapCols { i => DenseVector.ones(11).t }
    collect (d == DenseMatrix.ones(11,10))

    val e = x mapCols { i => DenseVector.ones(5).t }
    collect (e == DenseMatrix.ones(5,10))
    mkReport
  }
}

object ReduceRowsRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with ReduceRows
object ReduceRowsRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with ReduceRows
trait ReduceRows extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val x = DenseMatrix.ones(10,10)
    val y = x reduceRows { (r1,r2) => r1 + r2 }
    collect(y == DenseVector(10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0))
    mkReport
  }
}

object ShapesRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Shapes
object ShapesRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Shapes
trait Shapes extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val a = DenseMatrix(DenseVector(1,2,3),
                        DenseVector(4,5,6),
                        DenseVector(7,8,9))

    collect(diag(a) == DenseVector(1,5,9).t)
    collect(triu(a) == DenseMatrix(DenseVector(1,2,3), DenseVector(0,5,6), DenseVector(0,0,9)))
    collect(tril(a) == DenseMatrix(DenseVector(1,0,0), DenseVector(4,5,0), DenseVector(7,8,9)))

    val b = DenseMatrix(DenseVector(1,2,3,4),
                        DenseVector(5,6,7,8))

    collect(diag(b) == DenseVector(1,6).t)
    collect(triu(b) == DenseMatrix(DenseVector(1,2,3,4), DenseVector(0,6,7,8)))
    collect(tril(b) == DenseMatrix(DenseVector(1,0,0,0), DenseVector(5,6,0,0)))

    val c = DenseMatrix(DenseVector(1,2),
                        DenseVector(3,4),
                        DenseVector(5,6),
                        DenseVector(7,8))

    collect(diag(c) == DenseVector(1,4).t)
    collect(triu(c) == DenseMatrix(DenseVector(1,2), DenseVector(0,4), DenseVector(0,0), DenseVector(0,0)))
    collect(tril(c) == DenseMatrix(DenseVector(1,0), DenseVector(3,4), DenseVector(5,6), DenseVector(7,8)))

    // n x n utriangle shape
    val tri = utriangle(a.numRows)

    collect(tri.size == 6)
    collect(tri(0)._1 == 0 && tri(0)._2 == 0)
    collect(tri(1)._1 == 0 && tri(1)._2 == 1)
    collect(tri(2)._1 == 0 && tri(2)._2 == 2)
    collect(tri(3)._1 == 1 && tri(3)._2 == 1)
    collect(tri(4)._1 == 1 && tri(4)._2 == 2)
    collect(tri(5)._1 == 2 && tri(5)._2 == 2)
    collect(tri.contains(2,0) == false)

    mkReport
  }
}

object ViewsRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with Views
object ViewsRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with Views
trait Views extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val m = DenseMatrix(DenseVector(0,1,2,3,4),DenseVector(5,6,7,8,9),DenseVector(10,11,12,13,14),DenseVector(15,16,17,18,19))
    val middleTwoRows = m.sliceRows(1,3)
    collect(middleTwoRows == DenseMatrix(DenseVector(5,6,7,8,9),DenseVector(10,11,12,13,14)))

    val middleThreeCols = m.sliceCols(1,4)
    collect(middleThreeCols == DenseMatrix(DenseVector(1,2,3),DenseVector(6,7,8),DenseVector(11,12,13),DenseVector(16,17,18)))

    // last 3 rows, last 3 cols
    val rect = m.slice(1,4,2,5)
    collect(rect == DenseMatrix(DenseVector(7,8,9),DenseVector(12,13,14),DenseVector(17,18,19)))

    val rectSecondRow = rect(1)
    collect(rectSecondRow == DenseVector(12,13,14))

    val rectSecondCol = rect.getCol(1)
    collect(rectSecondCol == DenseVector(8,13,18).t)

    mkReport
  }
}


object SortByRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with SortBy
object SortByRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with SortBy
trait SortBy extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val m = DenseMatrix(DenseVector(5,6,7,8,9),DenseVector(0,1,2,3,4),DenseVector(15,16,17,18,19),DenseVector(10,11,12,13,14))
    val sortedRows = m.sortRowsBy(v => v.sum)
    collect(sortedRows == DenseMatrix(DenseVector(0,1,2,3,4),DenseVector(5,6,7,8,9),DenseVector(10,11,12,13,14),DenseVector(15,16,17,18,19)))

    val sortedCols = m.t.sortColsBy(v => v.sum)
    collect(sortedCols == sortedRows.t)

    mkReport
  }
}

class DenseMatrixSuiteInterpreter extends ForgeSuiteInterpreter {
  def testAccessors() { runTest(DenseMatrixAccessorsRunnerI) }
  def testOperators() { runTest(DenseMatrixOperatorsRunnerI) }
  def testUpdates() { runTest(DenseMatrixUpdatesRunnerI) }
  def testGroupRowsBy() { runTest(GroupRowsByRunnerI) }
  def testMapAll() { runTest(MapAllRunnerI) }
  def testReduceRows() { runTest(ReduceRowsRunnerI) }
  def testShapes() { runTest(ShapesRunnerI) }
  def testViews() { runTest(ViewsRunnerI) }
  def testSortBy() { runTest(SortByRunnerI) }
}

class DenseMatrixSuiteCompiler extends ForgeSuiteCompiler {
  cppWhiteList ++= Seq("sortindex_helper")
  def testAccessors() { runTest(DenseMatrixAccessorsRunnerC) }
  def testOperators() { runTest(DenseMatrixOperatorsRunnerC) }
  def testUpdates() { runTest(DenseMatrixUpdatesRunnerC) }
  def testGroupRowsBy() { runTest(GroupRowsByRunnerC) }
  def testMapAll() { runTest(MapAllRunnerC) }
  def testReduceRows() { runTest(ReduceRowsRunnerC) }
  def testShapes() { runTest(ShapesRunnerC) }
  def testViews() { runTest(ViewsRunnerC) }
  def testSortBy() { runTest(SortByRunnerC) }
}
