import optiml.compiler._
import optiml.library._
import optiml.shared._

object RBMCompiler extends OptiMLApplicationCompiler with RBM
object RBMInterpreter extends OptiMLApplicationInterpreter with RBM

trait RBM extends OptiMLApplication {

  def printUsage = {
    println("Usage: RBM <MNIST data file> <numHiddenUnits> <numcases>")
    exit(-1)
  }

  def main() = {
    reseed

    if (args.length < 3) printUsage

    val maxEpoch = 10 // maximum number of epochs
    val numHiddenUnits = args(1).toInt

    val epsilonw = 0.1 // Learning rate for weights
    val epsilonvb = 0.1 // Learning rate for biases of visible units
    val epsilonhb = 0.1 // Learning rate for biases of hidden units
    val weightCost = 0.0002
    val initialMomentum = 0.5
    val finalMomentum = 0.9

    println("Using " + numHiddenUnits + " hidden units.")

    println("Reading MNIST dataset")
    val numCases = args(2).toInt // batchSize
    val trainingData = readMatrix(args(0))
    val numDims = trainingData.numCols
    val numBatches = trainingData.numRows / numCases

    tic(trainingData)

    // Initialize symmetric weights and biases
    val visHid = (DenseMatrix.randn(numDims, numHiddenUnits) * 0.1)
    val hidBiases = DenseVector.zeros(numHiddenUnits)
    val visBiases = DenseVector.zeros(numDims)

    val visHidInc = DenseMatrix.zeros(numDims, numHiddenUnits)
    val hidBiasInc = DenseVector.zeros(numHiddenUnits)
    val visBiasInc = DenseVector.zeros(numDims)

    val errSum: Rep[Double] = 0.0 // needed because all arguments in the tuple must be explicitly Reps
    var epochErrSum = 0.0
    var epoch = 0
    var batch = 0

    // implicit def diffRBM(t1: Rep[Tup7[Double,DenseMatrix[Double],DenseMatrix[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double]]],
    //                      t2: Rep[Tup7[Double,DenseMatrix[Double],DenseMatrix[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double]]]) = dist(t1._1,t2._1)

    implicit def diffRBM(t1: Rep[Tup6[DenseMatrix[Double],DenseMatrix[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double]]],
                         t2: Rep[Tup6[DenseMatrix[Double],DenseMatrix[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double]]]) = unit(10.0) // always run until maxIter

    // val p = untilconverged((errSum,visHidInc,visHid,visBiasInc,visBiases,hidBiasInc,hidBiases), maxIter = numBatches*maxEpoch) {

    // inside REPL, not distinguishing between Rep[Tup] and Tuple correctly -- make_tupleX must be called explicitly
    val p = untilconverged_buffered(pack(visHidInc,visHid,visBiasInc,visBiases,hidBiasInc,hidBiases), maxIter = numBatches*maxEpoch) {
      params =>
      // val (errSum,visHidInc,visHid,visBiasInc,visBiases,hidBiasInc,hidBiases) = unpack(params)
      val (visHidInc,visHid,visBiasInc,visBiases,hidBiasInc,hidBiases) = unpack(params)

      //println("Epoch: " + epoch + ", Batch: " + batch)
      // Positive phase
      val data = trainingData.sliceRows(batch * numCases, (batch + 1) * numCases) // data: numCases x numDims
      val posHidProbs = (data * visHid + hidBiases.replicate(numCases, 1)).map(sigmoid)
      val posProds = data.t * posHidProbs
      val posHidAct = posHidProbs.sumCols
      val posVisAct = data.sumCols
      val posHidStates = (posHidProbs :> DenseMatrix.rand(numCases, numHiddenUnits)).map(e => if (e) 1.0 else 0.0)

      // Negative phase
      val negData = (posHidStates * visHid.t + visBiases.replicate(numCases, 1)).map(sigmoid)
      val negHidProbs = (negData * visHid + hidBiases.replicate(numCases, 1)).map(sigmoid)
      val negProds = negData.t * negHidProbs
      val negHidAct = negHidProbs.sumCols
      val negVisAct = negData.sumCols
      val diff = data - negData
      val nextErrSum = (diff *:* diff).sum

      // Update weights and biases
      val momentum = if (epoch > 5) finalMomentum else initialMomentum
      val nextVisHidInc = visHidInc * momentum + ((posProds - negProds) / numCases - (visHid * weightCost))*epsilonw
      val nextVisHid = visHid + nextVisHidInc
      val nextVisBiasInc = visBiasInc * momentum + (posVisAct - negVisAct) * (epsilonvb / numCases)
      val nextVisBiases = visBiases + nextVisBiasInc
      val nextHidBiasInc = hidBiasInc * momentum + (posHidAct - negHidAct) * (epsilonhb / numCases)
      val nextHidBiases = hidBiases + nextHidBiasInc

      batch += 1
      epochErrSum += nextErrSum

      if (batch == numBatches) {
        batch = 0
        epoch += 1
        println("--> Epoch " + epoch)
        println(" error = " + epochErrSum)
        epochErrSum = 0.0
      }

      // pack(nextErrSum, nextVisHidInc, nextVisHid, nextVisBiasInc, nextVisBiases, nextHidBiasInc, nextHidBiases)
      pack(nextVisHidInc, nextVisHid, nextVisBiasInc, nextVisBiases, nextHidBiasInc, nextHidBiases)
    }

    toc(p)
  }
}
