package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait ValidateOps {
  this: OptiMLDSL =>

  def importValidateOps() {
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val TrainingSet = lookupTpe("TrainingSet")
    val Tup2 = lookupTpe("Tup2")
    val T = tpePar("T")
    val L = tpePar("L")

    val Validate = grp("Validate")

    direct (Validate) ("holdOut", (T,L), (("dataSet", TrainingSet(T,L)), ("pct", MDouble)) :: Tup2(TrainingSet(T,L),TrainingSet(T,L))) implements composite ${
      val (trainSet, testSet) = unpack((0::dataSet.numSamples) partition { e => random[Double] > pct })
      val (trainIndices, testIndices) = (IndexVector(trainSet), IndexVector(testSet))

      pack((TrainingSet(dataSet.data.apply(trainIndices), dataSet.labels.apply(trainIndices)),
            TrainingSet(dataSet.data.apply(testIndices), dataSet.labels.apply(testIndices))))
    }

    direct (Validate) ("confusionMatrix", T, MethodSignature(List(
                                            ("testSet",TrainingSet(T,MBoolean)),
                                            ("classify", DenseVectorView(T) ==> MBoolean),
                                            ("numSamples", MInt, "unit(-1)")
                                          ), DenseMatrix(MInt))) implements composite ${

      val numSamplesToProcess = if (numSamples == -1) testSet.numSamples else numSamples

      // returns [TP, FN, FP, TN]
      val stats = sum(0, numSamplesToProcess) { i =>
        // if (i > 0 && i % 10000 == 0) println("sample: " + i)

        val trueLabel = testSet.labels.apply(i)
        val predictedLabel = classify(testSet(i))

        if (trueLabel && predictedLabel) {
          DenseVector(1, 0, 0, 0)
        }
        else if (trueLabel && !predictedLabel) {
          DenseVector(0, 1, 0, 0)
        }
        else if (!trueLabel && predictedLabel) {
          DenseVector(0, 0, 1, 0)
        }
        else {
          DenseVector(0, 0, 0, 1)
        }

      }

      DenseMatrix(DenseVector(stats(0), stats(1)), DenseVector(stats(2), stats(3)))
    }

    direct (Validate) ("accuracy", Nil, DenseMatrix(MInt) :: MDouble) implements composite ${
      val TP = $0(0,0)
      val TN = $0(1,1)
      (TP + TN).toDouble / sum($0).toDouble
    }

    direct (Validate) ("precision", Nil, DenseMatrix(MInt) :: MDouble) implements composite ${
      val TP = $0(0,0)
      val FP = $0(1,0)
      TP.toDouble / (TP + FP).toDouble
    }

    direct (Validate) ("sensitivity", Nil, DenseMatrix(MInt) :: MDouble) implements composite ${
      val TP = $0(0,0)
      val FN = $0(0,1)
      TP.toDouble / (TP + FN).toDouble
    }
  }
}
