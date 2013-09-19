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

    val MAXITERS = 100;
    val ABSTOL = 1e-8;
    val RELTOL = 1e-8;
    val FEASTOL = 1e-8;
    val MINSLACK = 1e-8;      
    val STEP = 0.99;
    val EXPON = 3;

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
    val u0 = DenseVector(tau0).t << z0
    val v0 = y0 << x0 << DenseVector(theta0).t
    val w0 = DenseVector(lambda0).t << s0

    val dimu = m+1
    val dimv = p+n+1
    val dimw = m+1

    implicit def diffPDIP(t1: Rep[Tup4[DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double]]],
                          t2: Rep[Tup4[DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double]]]) = {
      val (u, v, w, viters) = t4(t2)
      val iters = viters(0)

      val tau = u(0)
      val z = u.slice(1, m+1)
      val y = v.slice(0, p)
      val x = v.slice(p, p+n)
      val theta = v(p+n)
      val lambda = w(0)
      val s = w.slice(1, m+1)

      val pcost = (c*:*x) / tau  
      val dcost = -(h*:*z + b*:*y)/ tau  
      val absgap = (u*:*w) / (tau*tau)
      val relgap = if (dcost > 0.0) {
        absgap / dcost
      } 
      else if (pcost < 0.0) {
        absgap / (-pcost)
      }
      else {
        Double.PositiveInfinity
      }
      val hresi = G*x+s
      val resi = h - hresi/tau
      val hrese = A*x
      val rese = b-hrese/tau
      val hresd = G.t*z + A.t*y
      val resd = c+hresd/tau
      val pres = max(norm(resi)/nrmh, norm(rese)/nrmb)
      val dres = norm(resd)/nrmc
      val hpres = max(norm(hresi), norm(hrese))
      val hdres = norm(hresd)
      
      println("%02.0f:% 7.0e% 8.0e% 8.0e% 8.0e ".format(iters - 1.0, absgap, relgap, pres, dres))

      if ((pres <= FEASTOL)&&(dres <= FEASTOL)&&((absgap <= ABSTOL)||(relgap <= RELTOL))) {
        // optimal
        0.0
      }
      else if ((h*:*z+b*:*y < 0.0)&&(hdres/abs(h*:*z+b*:*y)<= FEASTOL)) {
        // primal infeasible
        0.0
      }
      else if ((c*:*x < 0.0)&&(hpres/abs(c*:*x) <= FEASTOL)) {
        // dual infeasible
        0.0
      }
      else {
        // continue
        1.0
      }
    }

    val result = untilconverged((u0,v0,w0,DenseVector(0.0)), maxIter = MAXITERS) { cur =>
      val (u, v, w, viters) = t4(cur)
      val iters = viters(0)
      
      val tau = u(0)
      val z = u.slice(1, m+1)
      val y = v.slice(0, p)
      val x = v.slice(p, p+n)
      val theta = v(p+n)
      val lambda = w(0)
      val s = w.slice(1, m+1)

      println(G.numRows)
      println(G.numCols)
      println(A.numRows)
      println(A.numCols)

      val GA = G.mutable
      GA.insertAllRows(0, A.mutable)

      (u, v, w, DenseVector(iters + 1.0))
    }
  }

  def kkt_sol(d: Rep[DenseVector[Double]], G: Rep[DenseMatrix[Double]], A: Rep[DenseMatrix[Double]], rx: Rep[DenseVector[Double]], ry: Rep[DenseVector[Double]], rz: Rep[DenseVector[Double]]) = {
    val r = rx << ry << rz

  }

  def norm(x: Rep[DenseVector[Double]]) = {
    //sqrt(sum((0::x.length) { i => x(i) * x(i) }))
    sqrt(x *:* x)
  }
}
