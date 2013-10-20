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

object DenseMatrixAccessorsRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with DenseMatrixAccessors
object DenseMatrixAccessorsRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with DenseMatrixAccessors
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
    collect(mSlice2 == DenseMatrix((3,4,5,6,7,8,9,10,11,12)))
    val mSlice3 = m2(*,0::2)
    collect(mSlice3 == DenseMatrix((0,1,2,3,4,5,6,7,8,9),(1,2,3,4,5,6,7,8,9,10)).t)
    val mSlice4 = m2(1::4,*)
    collect(mSlice4 == DenseMatrix((1,2,3,4,5,6,7,8,9,10),(2,3,4,5,6,7,8,9,10,11),(3,4,5,6,7,8,9,10,11,12)))
    val mSlice5 = m2(IndexVector((3,1,2)),IndexVector((4,0,6)))
    collect(mSlice5 == DenseMatrix((7,3,9),(5,1,7),(6,2,8)))

    mkReport
  }
}

object DenseMatrixOperatorsRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with DenseMatrixOperators
object DenseMatrixOperatorsRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with DenseMatrixOperators
trait DenseMatrixOperators extends ForgeTestModule with OptiMLApplication {
  def main() {
    val m_rand = DenseMatrix.rand(2,2)
    collect(m_rand(0,0) != m_rand(0,1))
    collect(m_rand(0,0) != m_rand(1,0))
    collect(m_rand(0,0) != m_rand(1,1))

    val m = DenseMatrix((1,2,3,4,5),(1,2,3,4,5))
    collect(mean(m) == 3)
    collect(max(m) == 5)
    collect(min(m) == 1)

    mkReport
  }
}

object DenseMatrixUpdatesRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with DenseMatrixUpdates
object DenseMatrixUpdatesRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with DenseMatrixUpdates
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

// object GroupRowsByRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with GroupRowsBy
// object GroupRowsByRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with GroupRowsBy
// trait GroupRowsBy extends ForgeTestModule with OptiMLApplication {
//   def main() = {

//     val m = DenseMatrix((1,2,3,4),
//                         (2,-2,-3,-4),
//                         (1,5,6,7),
//                         (2,-5,-6,-7))

//     val ms = m.groupRowsBy(row => row(0))
//     collect(ms.length == 2)
//     for (m <- ms) {
//       m.pprint
//       collect(m.numRows == 2)
//       collect(m.numCols == 4)
//     }
//     mkReport
//   }
// }

object MapRowsRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with MapRows
object MapRowsRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with MapRows
trait MapRows extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val x = DenseMatrix.zeros(10,10)
    val y = x mapRows { i => DenseVector.ones(10) }
    collect (y == DenseMatrix.ones(10,10))
    mkReport
  }
}

object ReduceRowsRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with ReduceRows
object ReduceRowsRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with ReduceRows
trait ReduceRows extends ForgeTestModule with OptiMLApplication {
  def main() = {

    val x = DenseMatrix.ones(10,10)
    val y = x reduceRows { (r1,r2) => r1 + r2 }
    collect(y == DenseVector(10.,10.,10.,10.,10.,10.,10.,10.,10.,10.))
    mkReport
  }
}

class DenseMatrixSuiteInterpreter extends ForgeSuiteInterpreter {
  def testAccessors() { runTest(DenseMatrixAccessorsRunnerI) }
  def testOperators() { runTest(DenseMatrixOperatorsRunnerI) }
  def testUpdates() { runTest(DenseMatrixUpdatesRunnerI) }
  // def testGroupRowsBy() { runTest(GroupRowsByRunnerI) }
  def testMapRows() { runTest(MapRowsRunnerI) }
  def testReduceRows() { runTest(ReduceRowsRunnerI) }
}

class DenseMatrixSuiteCompiler extends ForgeSuiteCompiler {
  def testAccessors() { runTest(DenseMatrixAccessorsRunnerC) }
  def testOperators() { runTest(DenseMatrixOperatorsRunnerC) }
  def testUpdates() { runTest(DenseMatrixUpdatesRunnerC) }
  // def testGroupRowsBy() { runTest(GroupRowsByRunnerC) }
  def testMapRows() { runTest(MapRowsRunnerC) }
  def testReduceRows() { runTest(ReduceRowsRunnerC) }
}
