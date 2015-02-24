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
      // (($0 map { e => (e - minVal) / (maxVal - minVal) }) * 2.0) - 1.0 // scale to [-1 1]
    }

    direct (Classifier) ("normalize", Nil, DenseMatrix(MDouble) :: DenseMatrix(MDouble)) implements composite ${
      $0 mapCols { c => normalize(c) }
    }

    direct (Classifier) ("logreg", Nil, MethodSignature(List(
                                            ("data",TrainingSet(MDouble,MBoolean)),
                                            ("initLearningRate", MDouble, "unit(1.0)"),
                                            ("maxIter", MInt, "unit(30)"),
                                            ("stochastic", MBoolean, "unit(true)"),
                                            ("verbose", MBoolean, "unit(false)"),
                                            ("callback", (DenseVector(MDouble), MInt) ==> MUnit, "(m,i) => unit(())")
                                          ), DenseVector(MDouble))) implements composite ${

      val theta = DenseVector.zeros(data.numFeatures)
      val y = data.labels map { label => if (label) 1.0 else 0.0 }

      val _maxIter = maxIter
      val _verbose = verbose

      // gradient descent with logistic function
      if (stochastic) {
        untilconverged(theta, maxIter = _maxIter, verbose = _verbose) { (cur, iter) =>
          val alpha = initLearningRate / (1.0 + initLearningRate*iter)
          val next = cur.mutable
          for (i <- 0 until data.numSamples) {
            val gradient = data(i)*(y(i) - sigmoid(next *:* data(i)))
            for (j <- 0 until data.numFeatures) {
              next(j) = next(j) + gradient(j)*alpha
            }
          }
          val ret = next.unsafeImmutable
          callback(ret, iter)
          ret
        }
      }
      else {
        untilconverged(theta, maxIter = _maxIter, verbose = _verbose) { (cur, iter) =>
          val alpha = initLearningRate / (1.0 + initLearningRate*iter)
          val gradient =
            ((0::data.numSamples) { i =>
              data(i)*(y(i) - sigmoid(cur *:* data(i)))
            }).sum

          val next = cur + gradient*alpha
          callback(next, iter)
          next
        }
      }
    }
  }
}
