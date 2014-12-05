package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait ClassifierOps {
  this: OptiMLDSL =>

  def importClassifierOps() {
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val TrainingSet = lookupTpe("TrainingSet")
    
    val Classifier = grp("Classifier")

    direct (Classifier) ("normalize", Nil, DenseVector(MDouble) :: DenseVector(MDouble)) implements composite ${
      val avg = mean($0)
      val dev = stddev($0)
      $0 map { e => (e - avg) / dev }

      // val minVal = min($0)
      // val maxVal = max($)
      // $0 map { e => (e - minVal) / (maxVal - minVal) }
    }

    direct (Classifier) ("normalize", Nil, DenseMatrix(MDouble) :: DenseMatrix(MDouble)) implements composite ${
      $0 mapCols { c => normalize(c) }        
    }

    direct (Classifier) ("logreg", Nil, MethodSignature(List(
                                            ("data",TrainingSet(MDouble,MBoolean)), 
                                            ("learningRate", MDouble, "unit(1.0)"),
                                            ("maxIter", MInt, "unit(30)")
                                          ), DenseVector(MDouble))) implements composite ${

      val theta = DenseVector.zeros(data.numFeatures)
      val alpha = learningRate

      val y = data.labels map { label => if (label) 1.0 else 0.0 }

      // batch gradient descent with logistic function
      val _maxIter = maxIter
      val w = untilconverged(theta, maxIter = _maxIter) { cur =>
        val gradient = sum((0::data.numSamples) { i =>
          data(i)*(y(i) - sigmoid(cur *:* data(i)))
        })

        cur + gradient*alpha        
      }

      w
    }
  }   
}
