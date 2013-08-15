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

    val c = readVector(args(0))
    val G = readMatrix(args(1))
    val h = readVector(args(2))
    val A = readMatrix(args(3))
    val b = readVector(args(4))
    val x0 = readVector(args(5))
    val s0 = readVector(args(6))
    val y0 = readVector(args(7))
    val z0 = readVector(args(8))

    if((G.numCols != c.length)||(A.numCols != c.length)||(G.numRows != h.length)||(A.numRows != b.length)) {
      println("error: matrix size mismatch")
    }

    val n = c.length
    val m = h.length
    val p = b.length

    if((x0.length != n)||(s0.length != m)) {
      println("error: primal start size mismatch")
    }

    if((y0.length != p)||(z0.length != m)) {
      println("error: dual start size mismatch")
    }
    
    var iters = 0

    while(iters < 1000) {
      println("iters: " + iters)
      iters = iters + 1
    }
  }
}
