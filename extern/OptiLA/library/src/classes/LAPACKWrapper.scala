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

trait LAPACKWrapper extends LAPACKWrapperBase with LAPACKWrapperImpl {
  this: OptiLALib with OptiLAApplication =>
}

trait LAPACKWrapperBase extends LinAlgWrapper {
  this: OptiLABase with OptiLAClasses with LAPACKOps =>

  override def linsolve(__arg0: Rep[DenseMatrix[Double]],__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext) = {
    if (useLAPACK) {
      val A = densematrix_t(__arg0) // must be col-major
      val b = densevector_mutable(__arg1)

      val (m,n,nrhs,lda,ldb,work,lwork,info) = lapack_linsolve_getparams(__arg0)

      LAPACK.getInstance().dgels("N", m, n, nrhs, A._data, lda, b._data, ldb, work, lwork, info);

      densevector_take(b,n) // answer is written in the first n entries of b
    }
    else super.linsolve(__arg0,__arg1)
  }

}

trait LAPACKWrapperImpl {
  this: OptiLAApplication with OptiLACompilerOps =>

  def lapack_linsolve_getparams(A: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext) = {
    val M = A.numRows
    val N = A.numCols
    val lda = M // col major, so M entries between columns
    val ldb = max(M,N)
    val nrhs = 1
    val lwork = 2*M*N
    val work = array_empty[Double](lwork)
    val info = new intW(0)
    (M,N,nrhs,lda,ldb,work,lwork,info)
  }

}
