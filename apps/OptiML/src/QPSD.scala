import optiml.compiler._
import optiml.library._
import optiml.shared._

import scala.virtualization.lms.common.Record

object QPSDCompiler extends OptiMLApplicationCompiler with QPSD {
  val mBQP: Manifest[BQP] = manifest[BQP]
}
object QPSDInterpreter extends OptiMLApplicationInterpreter with QPSD {
  val mBQP: Manifest[BQP] = manifest[BQP]
}

trait QPSD extends OptiMLApplication {

  type BQP = Record {
    val Q: DenseMatrix[Double]
    val p: DenseVector[Double]
    val lbound: DenseVector[Double]
    val ubound: DenseVector[Double]
    val diag: DenseVector[Double]
  }

  // workaround for erroneous or inaccessible type error with abstract typed record
  implicit val mBQP: Manifest[BQP]

  def newBQP(_Q: Rep[DenseMatrix[Double]], _p: Rep[DenseVector[Double]], _lbound: Rep[DenseVector[Double]], _ubound: Rep[DenseVector[Double]], _diag: Rep[DenseVector[Double]]): Rep[BQP] = new Record {
    val Q = _Q
    val p = _p
    val lbound = _lbound
    val ubound = _ubound
    val diag = _diag
  }


  /**
   * Single step of the SCD algorithm
   *
   *  @param q_i      The i-th row of Q
   *  @param i        Index of the co-ordinate
   *  @param p_i      The ith element of p vector
   *  @param q_ii     The ith diagonal entry of Q i.e Q[i,i]
   *  @param lb_i     The lower bound on the ith variable lb_i
   *  @param ub_i     The upper bound on the ith variable lb_i
   *  @param x        x-vector (Modified by reference)
   */
  def boundQPSD(q_i: Rep[DenseVectorView[Double]], i: Rep[Int], p_i: Rep[Double], lb_i: Rep[Double], ub_i: Rep[Double], q_ii: Rep[Double], x: Rep[DenseVector[Double]]) = {
    // memoize xk_i since we will write over it with x{k+1}_i
    val xk_i = x(i)
    // compute the ith component of the gradient
    val d_i = x *:* q_i
    val gradf_i = d_i + p_i
    // compute new value for the coordinate, with projection x^(k+1)_i
    val step = max(q_ii, 1e-6)
    val xkp1_i_cand = max(xk_i - gradf_i / step, lb_i)
    val xkp1_i = min(xkp1_i_cand, ub_i)

    // return updated parameter vector (TODO: use untilconverged_deltas)?
    // DenseVector(i -> xpk1_i)
    (0::x.length) { p_i => if (i == p_i) xkp1_i else x(p_i) }
  }

  /**
   * Performs 1 SCD Epoch on the given bqp
   * Assumes the bqp is normalized so diagonal entries are 1.
   *
   * @param x the variable of optimization
   * @param bqp the boxed QP to operate on
   * @param perm the permutation to use
   */
  def boundQPSDEpoch(x: Rep[DenseVector[Double]], bqp: Rep[BQP], perm: Rep[IndexVector]) = {
    // sequential: iterate over perm in order, each element in x one at a time
    // to convert to hog-wild, we should let this loop run in parallel and make mutable, racy updates (?)
    var i = -1
    untilconverged_buffered(x, maxIter = perm.length) { x =>
      i += 1
      val idx = perm(i)
      boundQPSD(bqp.Q(idx), idx, bqp.p(idx), bqp.lbound(idx), bqp.ubound(idx), bqp.diag(idx), x)
      // boundQPSD(bqp.Q.apply(idx), idx, bqp.p.apply(idx), bqp.lbound.apply(idx), bqp.ubound.apply(idx), bqp.diag.apply(idx), x)
    }
  }

  def main() = {
    // toy inputs for testing
    val x = DenseVector(1.,1.)
    val perm = (0::2)
    val bqp = newBQP(DenseMatrix((1.,2.),(3.,4.)), DenseVector(1.,1.), DenseVector(0.,0.), DenseVector(1.,1.), DenseVector(1.,4.))

    val x_star = untilconverged_buffered(x, maxIter = 100) { x =>
      // println("x: ")
      // x.pprint
      boundQPSDEpoch(x, bqp, perm)
    }

    println("found x_star: ")
    x_star.pprint
  }
}
