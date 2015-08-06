import optiml.compiler._
import optiml.library._
import optiml.shared._

import scala.virtualization.lms.common.Record

object QPSCDCompiler extends OptiMLApplicationCompiler with QPSCD {
  val mBQP: Manifest[BQP] = manifest[BQP]
}
object QPSCDInterpreter extends OptiMLApplicationInterpreter with QPSCD {
  val mBQP: Manifest[BQP] = manifest[BQP]
}

trait QPSCD extends OptiMLApplication {
  val HOGWILD = System.getProperty("qpscd.hogwild","false").toBoolean
  val NUM_EPOCHS = 100

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
  def boundQPSCD(q_i: Rep[DenseVectorView[Double]], i: Rep[Int], p_i: Rep[Double], lb_i: Rep[Double], ub_i: Rep[Double], q_ii: Rep[Double], x: Rep[DenseVector[Double]]) = {
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
    if (HOGWILD) {
      x(i) = xkp1_i
      x
    }
    else {
      (0::x.length) { p_i => if (i == p_i) xkp1_i else x(p_i) }
    }
  }

  /**
   * Performs 1 SCD Epoch on the given bqp
   * Assumes the bqp is normalized so diagonal entries are 1.
   *
   * @param x         the variable of optimization
   * @param bqp       the boxed QP to operate on
   * @param perm      the permutation to use
   */
  def boundQPSCDEpoch(x: Rep[DenseVector[Double]], bqp: Rep[BQP], perm: Rep[IndexVector]) = {
    if (HOGWILD) {
      // parallel with mutable, racy updates
      for (i <- 0::perm.length) {
        val idx = perm(i)
        boundQPSCD(bqp.Q(idx), idx, bqp.p(idx), bqp.lbound(idx), bqp.ubound(idx), bqp.diag(idx), x)
        ()
      }
      x
    }
    else {
      // sequential: iterate over perm in order, each element in x one at a time
      var i = -1
      untilconverged_buffered(x, minIter = perm.length-1, maxIter = perm.length) { x =>
        i += 1
        val idx = perm(i)
        boundQPSCD(bqp.Q(idx), idx, bqp.p(idx), bqp.lbound(idx), bqp.ubound(idx), bqp.diag(idx), x)
      }
    }
  }

  def printUsage() {
    println("Usage: QPSCD <input data folder> <output file>")
    println("<input data folder> must contain files named Q.csv, p.csv, lb.csv, ub.csv")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) printUsage()

    val in = args(0)
    val out = args(1)
    val Q = readMatrix(in + "/Q.csv", ",")
    val p = readVector(in + "/p.csv")
    val lb = readVector(in + "/lb.csv")
    val ub = readVector(in + "/ub.csv")

    val x = DenseVector.zeros(p.length)
    val perm = shuffle(0::p.length)
    val bqp = newBQP(Q, p, lb, ub, Q.diag)

    println("finished loading input. Q: " + Q.numRows + " x " + Q.numCols + ", p: " + p.length + ", lb: " + lb.length + ", ub: " + ub.length)
    if (HOGWILD) println("HogWild!")
    tic(bqp)

    // toy inputs for testing
    // val x = DenseVector(1.,1.)
    // val perm = (0::2)
    // val bqp = newBQP(DenseMatrix((1.,2.),(3.,4.)), DenseVector(1.,1.), DenseVector(0.,0.), DenseVector(1.,1.), DenseVector(1.,4.))

    val x_star =
      if (HOGWILD) {
        var i = 0
        val xm = x.mutable
        while (i < NUM_EPOCHS) {
          boundQPSCDEpoch(xm, bqp, perm)
          i += 1
        }
        xm
      }
      else {
        untilconverged_buffered(x, minIter = NUM_EPOCHS-1) { x =>
          boundQPSCDEpoch(x, bqp, perm)
        }
      }

    toc(x_star)
    writeVector(x_star, out)
    // println("found x_star: ")
    // x_star.pprint
  }
}
