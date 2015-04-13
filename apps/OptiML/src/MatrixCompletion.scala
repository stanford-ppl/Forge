import optiml.compiler._
import optiml.library._
import optiml.shared._

object MatrixCompletionCompiler extends OptiMLApplicationCompiler with MatrixCompletion
object MatrixCompletionInterpreter extends OptiMLApplicationInterpreter with MatrixCompletion

trait MatrixCompletion extends OptiMLApplication {
  def printUsage = {
    println("Usage: MatrixCompletion <input data file> <size>")
    exit(-1)
  }

  def main() = {
    if (args.length != 2) printUsage

    val n = args(2).toInt

    val mBuilder = SparseMatrix[Double](n, n)
    var tmpx = readVector[Unit](args(1), { line => 
      mBuilder(line(0).toInt, line(1).toInt) = line(2).toDouble 
    }, " ")
    val a: Rep[SparseMatrix[Double]] = mBuilder.finish

    tic()
    val y0: Rep[DenseVector[Double]] = DenseVector.ones(n)

    val alpha = 1.0

    val w = untilconverged(y0, maxIter = 30) { y =>
      y * (1.0 - alpha) + a * y * alpha / (y *:* y)
    }

    toc(w)
  }
}
