import optiml.compiler._
import optiml.library._
import optiml.shared._

object kNNCompiler extends OptiMLApplicationCompiler with kNN
object kNNInterpreter extends OptiMLApplicationInterpreter with kNN

trait kNN extends OptiMLApplication {
  def printUsage = {
    println("Usage: KNN <input data file> <held back ratio 0.0 -- 1.0>")
    exit(-1)
  }

  def createDataSet = {
    val group = DenseMatrix((1.0, 1.1),(1.0, 1.0),(0.0, 0.0),(0.0, 0.1))
    val labels = DenseVector("A", "A", "B", "B")
    TrainingSet(group,labels)
  }

  def file2TraniningSet(inputFile:Rep[String]) = {
    val matrixIn = readMatrix(inputFile, s => s)
    val data = matrixIn.slice(0, matrixIn.numRows, 0, matrixIn.numCols-1).map(x => x.toDouble)
    val labels = matrixIn.getCol(matrixIn.numCols-1)
    TrainingSet(data,labels)
  }

  def file2TraniningSetAutoNorm(inputFile:Rep[String]) = {
    val matrixIn = readMatrix(inputFile, s => s)
    val data = matrixIn.slice(0, matrixIn.numRows, 0, matrixIn.numCols-1).map(x => x.toDouble)
    val maxVals = data.maxCols
    val minVals = data.minCols
    val ranges = maxVals - minVals
    val data1 = (0::data.numRows-1,*)(i => (data(i)- minVals)/ranges)
    // val data1 = data.mapRows(row => (row - minVals)/ranges)
    val labels = matrixIn.getCol(matrixIn.numCols-1)
    TrainingSet(data1,labels)
  }

  def KNNClassify[L:Manifest](data: Rep[DenseMatrix[Double]], labels: Rep[DenseVector[L]], inX: Rep[DenseVector[Double]], k:Rep[Int]) : Rep[L] = {
    val kIndices = data.mapRowsToVector(row => dist(row, inX, EUC)).sortWithIndex._2.take(k)
    val kLabels = labels(kIndices).groupBy(i => i, i => i).toVector
    val maxIndex = kLabels.map(_.length).maxIndex
    kLabels(maxIndex).apply(0) // return label associated with group
  }

  def main() = {
    if (args.length < 2) printUsage

    val trainingSet = file2TraniningSetAutoNorm(args(0))
    val hoRatio = args(1).toDouble
    if  (hoRatio < 0.0 || hoRatio > 1.0) printUsage

    val numTestVecs = floor(hoRatio*trainingSet.data.numRows)
    val testData = trainingSet.data.sliceRows(0, numTestVecs)
    val testLabels = trainingSet.labels.slice(0, numTestVecs)
    val trainData = trainingSet.data.sliceRows(numTestVecs, trainingSet.data.numRows - numTestVecs)
    val trainLabels = trainingSet.labels.slice(numTestVecs, trainingSet.data.numRows - numTestVecs)

    val errCount = sum(0, numTestVecs-1) { i =>
      val classifyLabel = KNNClassify(trainData, trainLabels, testData(i), 3)
      if (testLabels(i) == classifyLabel) {
        0
      }
      else {
        println( "the classifier came back with:"+ classifyLabel + " the real answer is:" + testLabels(i))
        1
      }
    }
    println("Total error rate:" + errCount/numTestVecs.toDouble)
    println("Total error count:" + errCount)
  }
}
