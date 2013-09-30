package optila.shared.ops

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import optila.shared._
import optila.shared.ops._
import optila.shared.typeclass._

trait LAPACKOps {
  lazy val useLAPACK = System.getProperty("optila.use.lapack", "true").toBoolean
}

trait LAPACKCompilerOps extends LAPACKOps with LAPACKHelperOps {
  this: OptiLAApplication =>
}

trait LAPACKHelperOps {
  this: OptiLAApplication =>

  def ipiv_to_P(numRows: Rep[Int], ipiv: Rep[DenseVector[Int]]): Rep[DenseMatrix[Int]] = {
    val P_indices = (0::numRows).mutable
    var i = 0
    while (i < ipiv.length) {
      // FIXME: scheduling bug in LMS/Delite if swap is a val instead of var
      var swap = P_indices(i)
      // val swap = P_indices(i)
      P_indices(i) = P_indices(ipiv(i)-1)
      P_indices(ipiv(i)-1) = swap
      i += 1
    }

    val P = DenseMatrix[Int](numRows, numRows)
    var j = 0
    while (j < P_indices.length) {
      P(j,P_indices(j)) = 1
      j += 1
    }

    P.unsafeImmutable
  }

  def postprocess_lu(a: Rep[DenseMatrix[Double]], ipiv: Rep[DenseVector[Int]])(implicit __pos: SourceContext): (Rep[DenseMatrix[Double]],Rep[DenseMatrix[Double]],Rep[DenseMatrix[Int]]) = {
    val d = min(a.numRows, a.numCols)
    val L = (0::a.numRows, 0::d) { (i,j) => if (i > j) a(i,j) else if (i == j) 1.0 else 0.0 }
    val U = (0::d, 0::a.numCols) { (i,j) => if (i <= j) a(i,j) else 0.0 }
    val P = ipiv_to_P(a.numRows, ipiv)

    (L,U,P)
  }

  def check_chol(a: Rep[DenseMatrix[Double]], tri: Rep[String]): Rep[Unit] =  {
    if (tri != "upper" && tri != "lower") fatal("chol: illegal 'tri' string")
    if (a.numRows != a.numCols) fatal("chol: A is not symmetric")
  }

  def postprocess_chol(a: Rep[DenseMatrix[Double]], tri: Rep[String]): Rep[DenseMatrix[Double]] =  {
    if (tri == "upper") {
      a.triu
    }
    else {
      a.tril
    }
  }
}
