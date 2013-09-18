import optiml.compiler._
import optiml.library._
import optiml.shared._

object PDIPSolverCompiler extends OptiMLApplicationCompiler with PDIPSolver
object PDIPSolverInterpreter extends OptiMLApplicationInterpreter with PDIPSolver

trait PDIPSolver extends OptiMLApplication { 
  def print_usage = {
    println("Usage: PDIPSolver <c> <G> <h> <A> <b> <x0> <s0> <y0> <z0>")
    exit(-1)
  }

  def main() = {

    if (args.length != 9) {
      print_usage
    }

    val c = readVector(args(0)).t
    val G = readMatrix(args(1))
    val h = readVector(args(2)).t
    val A = readMatrix(args(3))
    val b = readVector(args(4)).t

    val n = c.length
    val m = h.length
    val p = b.length

    val x0 = readVector(args(5)).t
    val s0 = readVector(args(6)).t
    val y0 = readVector(args(7)).t
    val z0 = readVector(args(8)).t

    if((G.numCols != c.length)||(A.numCols != c.length)||(G.numRows != h.length)||(A.numRows != b.length)) {
      println("error: matrix size mismatch")
      println("c := " + c.length)
      println("G := " + G.numCols + " x " + G.numRows)
      println("h := " + h.length)
      println("A := " + A.numCols + " x " + A.numRows)
      println("b := " + b.length)
      exit(-1)
    }

    if((x0.length != n)||(s0.length != m)) {
      println("error: primal start size mismatch")
      exit(-1)
    }

    if((y0.length != p)||(z0.length != m)) {
      println("error: dual start size mismatch")
      exit(-1)
    }

    val rc = 1.0+z0*:*s0
    val ri = h-G*x0-s0
    val re = b-A*x0
    val rd = c+G.t*z0+A.t*y0
    val ro = -(c*:*x0+h*:*z0+b*:*y0+1.0)

    val nrmh = max(1.0,norm(h))
    val nrmb = max(1.0,norm(b))
    val nrmc = max(1.0,norm(c))

    val tau0 = 1.0
    val lambda0 = 1.0
    val theta0 = 1.0
    val u0 = DenseVector(tau0) << z0
    val v0 = y0 << x0 << DenseVector(theta0)
    val w0 = DenseVector(lambda0) << s0

    val dimu = m+1
    val dimv = p+n+1
    val dimw = m+1

    implicit def diffPDIP(t1: Rep[Tup7[DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double]]],
                         t2: Rep[Tup7[DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double]]]) = unit(10.0)

    val result = untilconverged((x0,s0,y0,z0,u0,v0,w0), maxIter = 100) { cur =>
      val (x, s, y, z, u, v, w) = t7(cur)
      println(norm(x))
      (x, s, y, z, u, v, w)
    }
  }

  def norm(x: Rep[DenseVector[Double]]) = {
    //sqrt(sum((0::x.length) { i => x(i) * x(i) }))
    sqrt(x *:* x)
  }
}
