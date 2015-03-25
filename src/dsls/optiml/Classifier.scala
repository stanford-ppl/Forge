package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait ClassifierOps {
  this: OptiMLDSL =>

  def importClassifierOps() {
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseTrainingSet = lookupTpe("DenseTrainingSet")

    val D = tpePar("D")
    val L = tpePar("L")
    val TS = hkTpePar("TS", (D,L))

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

    direct (Classifier) ("logreg", TS, MethodSignature(List(
                                            ("data",TS(MDouble,MBoolean)),
                                            ("initLearningRate", MDouble, "unit(1.0)"),
                                            ("maxIter", MInt, "unit(30)"),
                                            ("stochastic", MBoolean, "unit(true)"),
                                            ("verbose", MBoolean, "unit(false)"),
                                            ("callback", (DenseVector(MDouble), MInt) ==> MUnit, "(m,i) => unit(())")
                                          ), DenseVector(MDouble)), ("_ts", TTrainingSetLike(MDouble,MBoolean,TS(MDouble,MBoolean)))) implements composite ${

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
            val gradient = data.timesScalar(i, y(i) - sigmoid(data.dot(i, next)))
            next += gradient*alpha
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
              data.timesScalar(i, y(i) - sigmoid(data.dot(i, cur)))
            }).sum

          val next = cur + gradient*alpha
          callback(next, iter)
          next
        }
      }
    }
  }
}
