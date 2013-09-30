package optila.library.classes

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import optila.shared._
import optila.shared.ops._
import optila.shared.typeclass._
import optila.library._
import optila.library.classes._

import org.netlib.util.intW
import org.netlib.lapack._

trait LAPACKWrapper extends LAPACKWrapperBase with LAPACKWrapperImpl with LAPACKHelperOps {
  this: OptiLALib with OptiLAApplication =>
}

trait LAPACKWrapperBase extends LinAlgWrapper {
  this: OptiLABase with OptiLAClasses with LAPACKOps =>

  override def linsolve(__arg0: Rep[DenseMatrix[Double]],__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext) = {
    if (useLAPACK) {
      val A = densematrix_t(__arg0) // must be col-major
      val (m,n,nrhs,lda,ldb,work,lwork,info,b) = lapack_linsolve_getparams(__arg0,__arg1)

      LAPACK.getInstance().dgels("N", m, n, nrhs, A._data, lda, b._data, ldb, work, lwork, info);

      densevector_take(b,n) // answer is written in the first n entries of b
    }
    else super.linsolve(__arg0,__arg1)
  }

  override def linalg_lu(a: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext): (Rep[DenseMatrix[Double]],Rep[DenseMatrix[Double]],Rep[DenseMatrix[Int]]) = {
    if (useLAPACK) {
      val A = densematrix_t(a) // must be col-major
      val (m,n,lda,ipiv,info) = lapack_lu_getparams(a)

      LAPACK.getInstance().dgetrf(m, n, A._data, lda, ipiv._data, info);

      // transpose again because result is col-major
      postprocess_lu(densematrix_t(A),ipiv)
    }
    else super.linalg_lu(a)
  }

  override def linalg_chol(a: Rep[DenseMatrix[Double]], tri: Rep[String])(implicit __pos: SourceContext): Rep[DenseMatrix[Double]] = {
    if (useLAPACK) {
      check_chol(a,tri)

      val A = densematrix_t(a) // must be col-major
      val (n,lda,info) = lapack_chol_getparams(a,tri)

      LAPACK.getInstance().dpotrf(tri, n, A._data, lda, info);

      // transpose again because result is col-major
      postprocess_chol(densematrix_t(A), tri)
    }
    else super.linalg_chol(a)
  }
}

trait LAPACKWrapperImpl {
  this: OptiLAApplication with OptiLACompilerOps =>

  // ** getparams functions are computed on the original (untransposed) A!

  def lapack_linsolve_getparams(A: Rep[DenseMatrix[Double]], b: Rep[DenseVector[Double]])(implicit __pos: SourceContext) = {
    val M = A.numRows
    val N = A.numCols
    val lda = M // col major, so M entries between columns
    val ldb = max(M,N)
    val nrhs = 1
    val lwork = 2*M*N
    val work = array_empty[Double](lwork)
    val info = new intW(0)
    val outB = DenseVector[Double](max(M,N), false)
    outB(0::b.length) = b(0::b.length)
    (M,N,nrhs,lda,ldb,work,lwork,info,outB)
  }

  def lapack_lu_getparams(A: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext) = {
    val M = A.numRows
    val N = A.numCols
    val lda = M // col major, so M entries between columns
    val ipiv = DenseVector[Int](min(M,N), false)
    val info = new intW(0)
    (M,N,lda,ipiv,info)
  }

  def lapack_chol_getparams(A: Rep[DenseMatrix[Double]], tri: Rep[String])(implicit __pos: SourceContext) = {
    val N = A.numRows
    val lda = N // col major, so N entries between columns
    val info = new intW(0)
    (N,lda,info)
  }
}
