/* Unit tests for OptiML sparse matrices.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  May 23, 2012
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import optiml.compiler._
import optiml.shared._
import optiml.library._
import ppl.tests.scalatest._

object SparseMatrixDataOpsRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with SparseMatrixDataOps
object SparseMatrixDataOpsRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with SparseMatrixDataOps
trait SparseMatrixDataOps extends ForgeTestModule with OptiMLApplication {
  def main() {
    val mb = SparseMatrix[Double](1000,1000)
    mb(11,101) = 17
    mb(10,100) = 39
    mb(10,100) = 5
    mb(9,100) = 1
    mb(9,722) = 722
    mb(9,331) = 331
    mb(11,101) = 2
    mb(573,71) = 15
    mb(200,17) = 3

    val m = mb.finish
    collect(m.numRows == 1000)
    collect(m.numCols == 1000)
    collect(m.nnz == 7)
    collect(m(10,10) == 0)
    collect(m(0,100) == 0)
    collect(m(9,100) == 1)
    collect(m(9,722) == 722)
    collect(m(9,331) == 331)
    collect(m(9,500) == 0)
    collect(m(10,772) == 0)
    collect(m(10,100) == 5)
    collect(m(11,101) == 2)
    collect(m(200,17) == 3)
    collect(m(573,71) == 15)
    collect(m(500,500) == 0)
    collect(m(10,101) == 0)
    collect(m(200,71) == 0)

    // insert/remove rows/col
    val mb2 = SparseMatrix[Int](10,10)
    val v1 = SparseVector[Int](10,true)
    val v2 = SparseVector[Int](10,true)
    var i = 0
    while (i < v1.length) { v1(i) = i+1; i += 1 }
    v2(4) = 25
    mb2.insertCol(5,v1)
    mb2.insertCol(10,v2)
    mb2.removeCols(2,2)
    val m2 = mb2.finish
    collect(m2.numRows == 10)
    collect(m2.numCols == 10)
    collect(m2.nnz == 11)
    collect(m2(0,3) == 1)
    collect(m2(4,8) == 25)

    val mb3 = SparseMatrix[Int](10,10)
    mb3.insertRow(5,v1)
    mb3.insertRow(10,v2)
    mb3.removeRows(3,1)
    val m3 = mb3.finish
    collect(m3.numRows == 11)
    collect(m3.numCols == 10)
    collect(m3.nnz == 11)
    collect(m3(4,0) == 1)
    collect(m3(9,4) == 25)

    mkReport
  }
}

object SparseMatrixOperatorsRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with SparseMatrixOperators
object SparseMatrixOperatorsRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with SparseMatrixOperators
trait SparseMatrixOperators extends ForgeTestModule with OptiMLApplication {
  def main() {
    val mRand = SparseMatrix.rand(100,100,0.9)
    collect(abs(mRand.nnz-1000) < 100)

    val mT = mRand.t
    collect(mT.numCols == mRand.numRows)
    collect(mT.numRows == mRand.numCols)
    collect(mT.nnz == mRand.nnz)
    (0::100) foreach { i =>
      collect(mRand(i).t == mT.getCol(i).toSparse)
    }

    val mClone = mRand.Clone
    collect(mRand == mClone)

    val mMut = mRand.mutable.finish
    collect(mRand == mMut)

    val m = DenseMatrix(DenseVector(1,2,3,4,5),DenseVector(1,2,3,4,5)).toSparse
    collect(mean(m) == 3)
    collect(max(m) == 5)
    collect(min(m) == 1)

    mkReport
  }
}

object SparseMatrixBulkOpsRunnerC extends OptiMLApplicationCompiler with ForgeTestRunnerCompiler with SparseMatrixBulkOps
object SparseMatrixBulkOpsRunnerI extends OptiMLApplicationInterpreter with ForgeTestRunnerInterpreter with SparseMatrixBulkOps
trait SparseMatrixBulkOps extends ForgeTestModule with OptiMLApplication {
  def main() {
    val ab = SparseMatrix[Int](10000,10000)
    ab(1023,39) = 62
    ab(777,2330) = 9
    ab(5,7534) = 1
    ab(1373,9999) = 17
    val a = ab.finish

    val bb = SparseMatrix[Int](10000,10000)
    bb(9999,7534) = 328
    bb(777,2330) = 10
    val b = bb.finish

    val a2 = a mapnz { e => e + 1 }
    collect(a2.nnz == 4)
    collect(a2.numRows == 10000)
    collect(a2.numCols == 10000)
    collect(a2(1023,39) == 63)
    collect(a2(777,2330) == 10)
    collect(a2(5,7534) == 2)
    collect(a2(1373,9999) == 18)
    collect(a2(5000,5000) == 0)

    val t1 = a + b
    collect(t1.nnz == 5)
    collect(t1.numRows == 10000)
    collect(t1.numCols == 10000)
    collect(t1(1023,39) == 62)
    collect(t1(777,2330) == 19)
    collect(t1(5,7534) == 1)
    collect(t1(1373,9999) == 17)
    collect(t1(9999,7534) == 328)
    collect(t1(5000,5000) == 0)

    val t2 = a*:*b
    collect(t2.nnz == 1)
    collect(t2.numRows == 10000)
    collect(t2.numCols == 10000)
    collect(t2(1023,39) == 0)
    collect(t2(777,2330) == 90)
    collect(t2(5,7534) == 0)
    collect(t2(1373,9999) == 0)
    collect(t2(9999,7534) == 0)
    collect(t2(5000,5000) == 0)

    // val b2 = a.zip(b) { (l,r) => l % (r+2) }
    // collect(b2.nnz == 3)
    // collect(b2.numRows == 10000)
    // collect(b2.numCols == 10000)
    // collect(b2(1023,39) == 0)
    // collect(b2(777,2330) == 9)
    // collect(b2(5,7534) == 1)
    // collect(b2(1373,9999) == 1)
    // collect(b2(9999,7534) == 0)
    // collect(b2(5000,5000) == 0)

    mkReport
  }
}

class SparseMatrixSuiteInterpreter extends ForgeSuiteInterpreter {
  def testDataOps() { runTest(SparseMatrixDataOpsRunnerI) }
  def testOperators() { runTest(SparseMatrixOperatorsRunnerI) }
  def testBulkOps() { runTest(SparseMatrixBulkOpsRunnerI) }
}

class SparseMatrixSuiteCompiler extends ForgeSuiteCompiler {
  cppWhiteList ++= Seq("sortindex_helper")
  def testDataOps() { runTest(SparseMatrixDataOpsRunnerC) }
  def testOperators() { runTest(SparseMatrixOperatorsRunnerC) }
  def testBulkOps() { runTest(SparseMatrixBulkOpsRunnerC) }
}
