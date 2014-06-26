import optiml.compiler._
import optiml.library._
import optiml.shared._

object KDDNNCompiler extends OptiMLApplicationCompiler with KDDNN
object KDDNNInterpreter extends OptiMLApplicationInterpreter with KDDNN

trait KDDNN extends OptiMLApplication with KDDCup99Data {
  def printUsage = {
    println("Usage: KDDNN <input training file> <input test file>")
    exit(-1)
  }

  def NNClassify(trainingSet: Rep[TrainingSet[Double,Int]])(inX: Rep[DenseVectorView[Double]]): Rep[Int] = {
    // mapRowsToVector appears to be allocated a new vector here for some reason instead of fusing with minIndex, 
    // though it doesn't appear to make a big difference
    val nearestIndex = trainingSet.data.mapRowsToVector(row => dist(row, inX, EUC)).minIndex    
    trainingSet.labels.apply(nearestIndex)
  }
  
  def main() = {
    if (args.length < 2) printUsage

    val trainingSet = readKDDCupData(args(0))
    val testSet = readKDDCupData(args(1))
    
    println("Training numSamples: " + trainingSet.numSamples + ", numFeatures: " + trainingSet.numFeatures)
    println("Test numSamples: " + testSet.numSamples + ", numFeatures: " + testSet.numFeatures)

    val numSamples = 10000 //testSet.numSamples 
    tic("running", numSamples)
    val stats = test(testSet, NNClassify(trainingSet), numSamples)    
    toc("running", stats)

    println("TN: " + stats(0) / numSamples.toDouble)
    println("TP: " + stats(1) / numSamples.toDouble)
    println("FP: " + stats(2) / numSamples.toDouble)
    println("FN: " + stats(3) / numSamples.toDouble)
    println("Total error rate: " + (stats(2)+stats(3))/numSamples.toDouble)
  }  
}
