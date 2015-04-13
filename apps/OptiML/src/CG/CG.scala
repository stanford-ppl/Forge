import optiml.compiler._
import optiml.library._
import optiml.shared._

object CGCompiler extends OptiMLApplicationCompiler with CG
object CGInterpreter extends OptiMLApplicationInterpreter with CG

trait CG extends OptiMLApplication { 

  def main() = {
    val A = readMatrix(args(0))
    val b = readVector(args(1)).t
    val expected_result = readVector(args(2)).t
    val x0 = DenseVector.zeros(A.numCols).t

    val r0 = b - A*x0
    val p0 = r0

    implicit def diffCG(t1: Rep[Tup3[DenseVector[Double],DenseVector[Double],DenseVector[Double]]],
                        t2: Rep[Tup3[DenseVector[Double],DenseVector[Double],DenseVector[Double]]]) = {
      val (x, r, p) = unpack(t2)
      sqrt(r *:* r)
    }

    val result = untilconverged(pack(x0, r0, p0), maxIter = 100) { cur =>
      val (x, r, p) = unpack(cur)

      val Ap = A * p
      val alpha = (r *:* r) / (p *:* Ap)
      val x_next = x + alpha * p
      val r_next = r - alpha * Ap
      val beta = (r_next *:* r_next) / (r *:* r)
      val p_next = r_next + beta*p

      pack(x_next, r_next, p_next)
    }

    val (x_soln, r_soln, p_soln) = unpack(result)

    val err = x_soln - expected_result
    println(sqrt(err *:* err))
  }

}
