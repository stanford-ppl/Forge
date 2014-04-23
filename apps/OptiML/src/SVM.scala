import optiml.compiler._
import optiml.library._
import optiml.shared._

object SVMCompiler extends OptiMLApplicationCompiler with SVM
object SVMInterpreter extends OptiMLApplicationInterpreter with SVM

trait SVM extends FileUtil {
  def printUsage = {
    println("Usage: SVM <train data file> <test data file>")//" <model filename> <num tests>")
    exit(-1)
  }

  /////////////
  // training

  def train(X: Rep[TrainingSet[Double,Double]], C: Rep[Double], tol: Rep[Double], maxPasses: Rep[Int]) = {
    println("Training SVM using the SMO algorithm")

    // init
    val numSamples = X.numSamples  
    var passes = 0
    var iter = 0

    // adjust the classification labels to -1 and +1 for SMO
    val Y = X.labels map { e => if (e == 0) -1.0 else 1.0 }

    // model parameter
    var b = 0.0

    // intermediate training info
    val alphas = DenseVector.zeros(numSamples).t.mutable // col vector
    
    while (passes < maxPasses) {
      print(".")
      iter += 1
      var numChangedAlphas = 0
      var i = 0
      while (i < numSamples) { //TR
        // TODO: x761 -- code is recalculating alphas from original definition here
        val alphasOld = alphas.Clone
        
        val f_i = (alphasOld*Y*(X.data*X(i).t)).sum + b //TR M*V alph0
        val E_i = f_i - Y(i)

        if (((Y(i)*E_i < -1.*tol) && (alphasOld(i) < C)) || ((Y(i)*E_i > tol) && (alphasOld(i) > 0.0))) {        
          // select a candidate j from the remaining numSamples-i samples at random
          var j = floor(random[Double]*(numSamples-1)).AsInstanceOf[Int]+1
          while (j == i) {
            j = floor(random[Double]*(numSamples-1)).AsInstanceOf[Int]+1
          }

          val f_j = (alphasOld*Y*(X.data*X(j).t)).sum + b //TR M*V alph0 -- inside if, cannot be fused with the one in f_i (calc actually happens further down)
          val E_j = f_j - Y(j)
                        
          val old_aj = alphasOld(j) //TR: making it a val should not move it down!
          //var old_ai = alphas(i)

          // calculate bounds L and H that must hold in order for a_i, alphas(j) to
          // satisfy constraints and check
          var L = 0.0
          var H = 0.0
          if (Y(i) != Y(j)) {
            L = max(0., alphasOld(j) - alphasOld(i))
            H = min(C, C + alphasOld(j) - alphasOld(i))
          }
          else {
            L = max(0., alphasOld(i) + alphasOld(j) - C)
            H = min(C, alphasOld(i) + alphasOld(j))
          }

          if (L != H) { //TR: problem: if/then/else will not force old_aj
            // calculate eta
            val eta = (X(i)*:*X(j)*2) - (X(i)*:*X(i)) - (X(j)*:*X(j))
            // check eta
            if (eta < 0){
              // compute new alphas(j)

              //alphas = alphas.Clone //TR
              alphas(j) = alphasOld(j) - Y(j)*(E_i-E_j)/eta //TR functionalize?

              // clip alphas(j) if necessary
              if (alphas(j) > H) alphas(j) = H
              else if (alphas(j) < L) alphas(j) = L

              // check alphas(j) convergence
              if (abs(alphas(j) - old_aj) >  .00001) {
                // find a_i to maximize objective function

                val old_ai = alphasOld(i)
                //alphas = alphas.Clone //TR
                alphas(i) = alphasOld(i) + Y(i)*Y(j)*(old_aj-alphas(j)) //TR functionalize?

                // compute the new b such that KKT conditions are satisfied
                val old_b = b
                val b1 = b - E_i - (X(i)*:*X(i))*Y(i)*(alphas(i)-old_ai) - (X(i)*:*(X(j)))*Y(j)*(alphas(j)-old_aj)
                val b2 = b - E_j - (X(i)*:*X(j))*Y(i)*(alphas(i)-old_ai) - (X(j)*:*(X(j)))*Y(j)*(alphas(j)-old_aj)
                if ((alphas(i) > 0.0) && (alphas(i) < C)){
                  b = b1
                }
                if ((alphas(j) > 0.0) && (alphas(j) < C)){
                  b = b2
                }
                if (old_b == b){
                  // neither threshold valid
                  b = ((b1+b2)/2)
                }

                numChangedAlphas += 1
              } // alpha converged?
            } // negative eta?
          } // L != H?
        } // main if (select alphas)
        i += 1 //TR
      } // for i = 1 to numSamples

      if (numChangedAlphas == 0){
        passes += 1
      }
      else {
        passes = 0;
      }
    } // while 

    // SMO finished
    println("num iterations: " + iter)

    // compute the weights (assuming a linear kernel)
    val weights = sum(0,numSamples) { i => X(i) * alphas(i) * Y(i) }    
  
    print("\n")

    (weights, b)
  }

  ////////////
  // testing

  def classify(weights: Rep[DenseVector[Double]], b: Rep[Double], testPt: Rep[DenseVector[Double]]): Rep[Double] = {
    // SVM prediction is W'*X + b
    if ((weights*:*testPt + b) < 0) {
      -1.0
    }
    else 1.0
  }

  ////////////
  // utility

  def loadModel(modelFilename: Rep[String]) = {
    val in = readVector(modelFilename)
    val b = in(in.length-1)
    val weights = in.take(in.length-1)
    (weights, b)
  }

  def saveModel(weights: Rep[DenseVector[Double]], b: Rep[Double], filename: Rep[String]) = {
    val out = weights.Clone
    out <<= b
    writeVector(out, filename)
  }

  def main() = {
    if (args.length < 2) printUsage

    val intTrainingSet = readTokenMatrix(args(0))
    val intTestSet = readTokenMatrix(args(1))
    //val modelFile = args(2)
    //val numTests = args(3).toInt

    // convert to double to use BLAS inside SMO
    val trainingSet = TrainingSet(intTrainingSet.data.toDouble, intTrainingSet.labels.toDouble)
    val testSet = TrainingSet(intTestSet.data.toDouble, intTestSet.labels.toDouble)
    
    reseed

    // -- run the SMO training algorithm
    tic()
    val (weights, b) = train(trainingSet, 1, .001, 10)
    toc()
    //saveModel(weights, b, modelFile)

    //println("SVM training finished. Model saved to " + modelFile)

    // -- compute classification error on testSet

    // adjust the classification labels to -1 and +1 for SMO
    val YTest = testSet.labels map { e => if (e == 0) -1.0 else 1.0 }
    //load(modelFile)

    val outputLabels = (0::testSet.numSamples){ i => classify(weights, b, testSet(i)) }    
    println("SVM testing finished. Calculating error..")

    val errors = sum[Int](0, testSet.numSamples) { i => if (YTest(i) != outputLabels(i)) 1 else 0 }    
    println("Classification error: " + (errors.toDouble/testSet.numSamples.toDouble))
  }
}
