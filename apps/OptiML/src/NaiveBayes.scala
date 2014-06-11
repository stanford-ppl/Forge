import optiml.compiler._
import optiml.library._
import optiml.shared._

object NBCompiler extends OptiMLApplicationCompiler with NaiveBayes
object NBInterpreter extends OptiMLApplicationInterpreter with NaiveBayes

trait NaiveBayes extends FileUtil {
  def printUsage = {
    println("NaiveBayes <training file> <test file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) printUsage

    val trainingFile = args(0)
    val testFile = args(1)

    val trainingSet = readTokenMatrix(trainingFile)
    println("Training model on " + trainingSet.numSamples + " documents.")
    tic()
    val (phi_y1, phi_y0, phi_y) = train(trainingSet)
    toc(phi_y1)

    val testSet = readTokenMatrix(testFile)
    println("phi_y1: ")
    phi_y1.pprint
    println("phi_y0: ")
    phi_y0.pprint
    println("phi_y: "+ phi_y)

    val incorrectClassifications = test(testSet, phi_y1, phi_y0, phi_y)
    println("Test error: " + incorrectClassifications.toDouble / testSet.numSamples.toDouble)
  }

  def train(ts: Rep[TrainingSet[Int,Int]]): (Rep[DenseVector[Double]], Rep[DenseVector[Double]], Rep[Double]) = {
    val wordsPerDoc = ts.data.sumRows
    val spamIndices = ts.labels.find(_ == 1)
    val nonSpamIndices = ts.labels.find(_ == 0)


    // (manually vectorized to iterate over rows instead of cols; similar to logreg transformation)
    //
    // val spamWordCountAll = ts.data.apply(spamIndices).reduceRows((r1,r2) => r1+r2) //sumCols
    // val spamTotalWords = wordsPerDoc(spamIndices).sum
    // val phi_y1 = spamWordCountAll.map(w => (w+1) / (spamTotalWords + ts.numFeatures).toDouble)

    val spamWords = ts.data.apply(spamIndices)
    val spamTotalWords = wordsPerDoc(spamIndices).sum

    val phi_y1 = (0::ts.numFeatures) { j =>
      val spamWordCount = spamWords.getCol(j).sum
      (spamWordCount + 1) / (spamTotalWords + ts.numFeatures).toDouble
    }

    val nonSpamWords = ts.data.apply(nonSpamIndices)
    val nonSpamTotalWords = wordsPerDoc(nonSpamIndices).sum
    
    val phi_y0 = (0::ts.numFeatures) { j =>
      val nonSpamWordCount = nonSpamWords.getCol(j).sum
      (nonSpamWordCount + 1) / (nonSpamTotalWords + ts.numFeatures).toDouble
    }

    val phi_y = ts.labels.sum / ts.numSamples.toDouble

    (phi_y1, phi_y0, phi_y)
  }

  def test(ts: Rep[TrainingSet[Int,Int]], phi_y1: Rep[DenseVector[Double]], phi_y0: Rep[DenseVector[Double]], phi_y: Rep[Double]) = {
    println("Testing model on " + ts.numSamples + " documents.")

    val output = (0::ts.numSamples) { j =>
      // compute log(p(x|y=1)p(y=1)) and log(p(x|y=0)p(y=0))
      val pNorm = sumIf(0,ts.numFeatures) { i => ts(j,i) > 0 } { i => (log(phi_y0(i)) + log(1.-phi_y)) * ts(j,i) }
      val pSpam = sumIf(0,ts.numFeatures) { i => ts(j,i) > 0 } { i => (log(phi_y1(i)) + log(phi_y)) * ts(j,i) }

      if (pSpam > pNorm) 1. else 0.
    }

    // compute error on test set
    val incorrectClassifications = ts.labels.zip(output) { (a,b) => if (a != b) 1 else 0 }
    incorrectClassifications.sum
  }
}
