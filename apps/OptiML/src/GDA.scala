import optiml.compiler._
import optiml.library._
import optiml.shared._

object GDACompiler extends OptiMLApplicationCompiler with GDA
object GDAInterpreter extends OptiMLApplicationInterpreter with GDA

trait GDA extends OptiMLApplication {
  def printUsage = {
    println("Usage: GDA <input data file> <output label data file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) printUsage

    val x = readMatrix(args(0))
    val y = readVector(args(1)).map(d => if (d <= 0.0) false else true)
    tic()

    /* number of training samples */
    val m = y.length

    /* dimensionality of training data */
    val n = x.numCols

    /* phi, mu0, mu1, and sigma parameterize the GDA model, where we assume the
     * input features are continuous-valued random variables with a multivariate
     * normal distribution.
     *
     * phi is a scalar, mu0 and mu1 are n dimensional vectors,
     * where n is the width of x, and sigma is an n x n matrix.
     */
    val y_zeros = y count { _ == false }
    val y_ones = y count { _ == true }
    val mu0_num = sumRowsIf(0,m) { !y(_) } { x(_) }
    val mu1_num = sumRowsIf(0,m) { y(_) } { x(_) }

    val phi = 1./m * y_ones
    val mu0 = mu0_num / y_zeros
    val mu1 = mu1_num / y_ones

    /* calculate covariance matrix sigma */
    /* x(i) is a row vector for us, while it is defined a column vector in the formula */
    val sigma = sum(0, m) { i =>
      if (y(i) == false){
        (((x(i)-mu0).t) ** (x(i)-mu0))
      }
      else{
        (((x(i)-mu1).t) ** (x(i)-mu1))
      }
    }

    toc(sigma)
    println("  phi = " + phi)
    println("  mu0 = " ); mu0.pprint
    println("  mu1 = " ); mu1.pprint
    println("  sigma = "); sigma.sliceRows(0,10).pprint
  }
}
