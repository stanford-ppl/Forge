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

    println("point A")
    var rc = 1+z0.t*s0
    println("point B")
    var ri = h-G*x0-s0
    println("point C")
    var re = b-A*x0
    println("point D")
    var rd0 = G.t*z0
    println("point D1")
    var rd1 = A.t*y0
    println("point D2")
    var rd2 = rd0+rd1
    println("point D3")
    println(c)
    println(rd2)
    var rd = c+rd2
    println("point E")
    var ro = -(c.t*x0+h.t*z0+b.t*y0+1)
    println("point F")

    var nrmh = max(1.0,norm(h))
    var nrmb = max(1.0,norm(b))
    var nrmc = max(1.0,norm(c))

    println(rc)
    println(ri)
    println(re)
    println(rd)
    println(ro)

    println(nrmh)
    println(nrmb)
    println(nrmc)
  }

  def norm(x: Rep[DenseVector[Double]]) = {
    //sqrt(sum((0::x.length) { i => x(i) * x(i) }))
    sqrt(x *:* x)
  }
}
