import optiml.compiler._
import optiml.library._
import optiml.shared._

object NBCompiler extends OptiMLApplicationCompiler with NaiveBayes
object NBInterpreter extends OptiMLApplicationInterpreter with NaiveBayes

trait NaiveBayes extends OptiMLApplication { 
  def printUsage = {
    println("NaiveBayes <training file> <test file>")
    exit(-1)
  }

  /* The input file is expected to follow the format:
   *
   *  <header>
   *  <num documents> <num tokens>
   *  <tokenlist>
   *  <document word matrix, where each row repesents a document and each column a distinct token>
   *    each line of the doc word matrix begins with class (0 or 1) and ends with -1
   *    the matrix is sparse, so each row has a tuple of (tokenIndex, number of appearances)
  */  
  def readTokenMatrix(f: Rep[String]) = {
    val lines = readVector[DenseVector[String]](f, words => words)
    val numDocs = lines(1).apply(0).toInt
    val numTokens = lines(1).apply(1).toInt
    val tokens = lines(2)

    val matrix = lines.drop(3)
    val labels = matrix.map(v => v.first.toInt).t        
    val data = (0::numDocs, *) { i =>
      val row = matrix(i).slice(1,matrix(i).length-1)
      val tupleIndices = DenseVector.uniform(0,2,row.length).map(_.toInt)
      val cumulIndices = tupleIndices.map(row(_).toInt).prefixSum

      // TODO: need to use a sparse matrix to avoid this mutation efficiently
      val denseRow = DenseVector[Int](numTokens, true) 
      (0::tupleIndices.length) foreach { j =>       
        denseRow(cumulIndices(j)) = row(tupleIndices(j)+1).toInt
      }
      denseRow.unsafeImmutable
    } 
    TrainingSet(data,labels)
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
            
    val phi_y1 = (0::ts.numFeatures) { j =>      
      val spamWordCount = ts.data.apply(spamIndices).getCol(j).sum        
      val spamTotalWords = wordsPerDoc(spamIndices).sum
      (spamWordCount + 1) / (spamTotalWords + ts.numFeatures).toDouble
    }

    val phi_y0 = (0::ts.numFeatures) { j =>
      val nonSpamWordCount = ts.data.apply(nonSpamIndices).getCol(j).sum
      val nonSpamTotalWords = wordsPerDoc(nonSpamIndices).sum
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
