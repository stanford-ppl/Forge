// import optiml.compiler._
// import optiml.library._
// import optiml.shared._

// object kMeansCompiler extends OptiMLApplicationCompiler with kMeans
// object kMeansInterpreter extends OptiMLApplicationInterpreter with kMeans

import optiml.direct._

import org.scala_lang.virtualized.virtualize  

@virtualize
trait kMeans extends OptiMLApplication {
  def printUsage = {
    println("Usage: kmeans <input data file> [initmu data file]")
    exit(-1)
  }

  private val tol = 0.001 // tolerance (for convergence)
  private val k = 16 // num clusters

  private def findNearestCluster(x_i: Rep[DenseVectorView[Double]], mu: Rep[DenseMatrix[Double]]): Rep[Int] = {
    (mu mapRowsToVector { row => dist(x_i, row, SQUARE) }).minIndex
  }

  def main() = {
    if (args.length < 1) printUsage

    val x = DenseTrainingSet(readMatrix(args(0)), DenseVector[Double]()) // no labels
    val m = x.numSamples
    val n = x.numFeatures
    val mu = if (args.length > 1) readMatrix(args(1)) else ((0::k, *) { i => x(randomInt(m)) })

    println("m:"^m^",n:"^n^",numClusters:"^k^",mu.numRows:"^mu.numRows)

    tic(mu)

    val newMu = untilconverged_withdiff(mu, tol){ (mu, iter) =>
      val c = (0::m) { e => findNearestCluster(x(e), mu) }

      val allWP = (0::m).groupByReduce(i => c(i), i => x(i).Clone, (a: Rep[DenseVector[Double]], b: Rep[DenseVector[Double]]) => a + b)
      val allP = (0::m).groupByReduce(i => c(i), i => 1, (a: Rep[Int], b: Rep[Int]) => a+b)

      (0::k, *) { j =>
        val weightedpoints = allWP(j)
        val points = allP(j)
        val d = if (points == 0) 1 else points
        weightedpoints / d
      }
    }((x, y) => dist(x, y, SQUARE)) // use SQUARE instead of the default EUC distance

    toc(newMu)
    newMu.pprint
  }
}
