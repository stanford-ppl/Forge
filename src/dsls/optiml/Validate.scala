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
    val DenseTrainingSet = lookupTpe("DenseTrainingSet")
    val IndexVector = lookupTpe("IndexVector")
    val Tup2 = lookupTpe("Tup2")
    val Tup3 = lookupTpe("Tup3")
    val T = tpePar("T")
    val L = tpePar("L")
    val M = tpePar("M")
    val R = tpePar("R")
    val TS = hkTpePar("TS", (T,L))

    val Validate = grp("Validate")

    direct (Validate) ("holdOut", (T,L,TS), (("dataSet", TS(T,L)), ("pct", MDouble)) :: Tup2(TS(T,L),TS(T,L)), TTrainingSetLike(T,L,TS(T,L))) implements composite ${
      val (trainSet, testSet) = unpack((0::dataSet.numSamples) partition { e => random[Double] > pct })
      val (trainIndices, testIndices) = (IndexVector(trainSet), IndexVector(testSet))

      pack((dataSet.getRows(trainIndices)), dataSet.getRows(testIndices))
    }

    direct (Validate) ("holdOut2", (T,L,TS), (("dataSet", TS(T,L)), ("pctValidationSamples", MDouble), ("pctTestSamples", MDouble)) :: Tup3(TS(T,L),TS(T,L),TS(T,L)), TTrainingSetLike(T,L,TS(T,L))) implements composite ${
      val pctTrainingSamples = 1.0 - (pctValidationSamples + pctTestSamples)
      val trainingNumSamples = floor(dataSet.numSamples*pctTrainingSamples)
      val validationNumSamples = floor(dataSet.numSamples*pctValidationSamples)
      val testNumSamples = floor(dataSet.numSamples*pctTestSamples)

      val shuffledIndices = shuffle(0::dataSet.numSamples)
      val trainingSampleIndices = shuffledIndices(0::trainingNumSamples)
      val validationSampleIndices = shuffledIndices(trainingNumSamples::trainingNumSamples+validationNumSamples)
      val testSampleIndices = shuffledIndices(trainingNumSamples+validationNumSamples::dataSet.numSamples)

      val trainingSet = dataSet.getRows(trainingSampleIndices)
      val validationSet = dataSet.getRows(validationSampleIndices)
      val testSet = dataSet.getRows(testSampleIndices)
      pack((trainingSet, validationSet, testSet))
    }

    compiler (Validate) ("confusionMatrixIndicator", Nil, (("trueLabel", MBoolean), ("predictedLabel", MBoolean)) :: DenseVector(MInt)) implements composite ${
      if (trueLabel && predictedLabel) {
        DenseVector(1, 0, 0, 0)
      }
      else if (!trueLabel && predictedLabel) {
        DenseVector(0, 1, 0, 0)
      }
      else if (trueLabel && !predictedLabel) {
        DenseVector(0, 0, 1, 0)
      }
      else {
        DenseVector(0, 0, 0, 1)
      }
    }

    /**
     * Generate a confusion matrix for the given classifier and testSet.
     */
    direct (Validate) ("confusionMatrix", (T,TS), MethodSignature(List(
                                            ("testSet", TS(T,MBoolean)),
                                            ("classify", MInt ==> MBoolean),
                                            ("numSamples", MInt, "unit(-1)")
                                          ), DenseMatrix(MInt)), TTrainingSetLike(T,MBoolean,TS(T,MBoolean))) implements composite ${

      val numSamplesToProcess = if (numSamples == -1) testSet.numSamples else numSamples

      // returns [TP, FP; FN, TN]
      val stats = sum(0, numSamplesToProcess) { i =>
        // if (i > 0 && i % 10000 == 0) println("sample: " + i)
        val trueLabel = testSet.labels.apply(i)
        val predictedLabel = classify(i)
        confusionMatrixIndicator(trueLabel, predictedLabel)
      }

      DenseMatrix(DenseVector(stats(0), stats(1)), DenseVector(stats(2), stats(3)))
    }

    /**
     * The same as confusionMatrix, except process testSamples as a batch.
     */
    direct (Validate) ("confusionMatrixBatch", (T,TS), MethodSignature(List(
                                            ("testSet", TS(T,MBoolean)),
                                            ("classify", IndexVector ==> DenseVector(MBoolean)),
                                            ("numSamples", MInt, "unit(-1)")
                                          ), DenseMatrix(MInt)), TTrainingSetLike(T,MBoolean,TS(T,MBoolean))) implements composite ${

      val numSamplesToProcess = if (numSamples == -1) testSet.numSamples else numSamples

      val results = classify(0::numSamplesToProcess)

      // returns [TP, FP; FN, TN]
      val stats = sum(0, numSamplesToProcess) { i =>
        // if (i > 0 && i % 10000 == 0) println("sample: " + i)
        val trueLabel = testSet.labels.apply(i)
        val predictedLabel = results(i)
        confusionMatrixIndicator(trueLabel, predictedLabel)
      }

      DenseMatrix(DenseVector(stats(0), stats(1)), DenseVector(stats(2), stats(3)))
    }

    /**
     * A generic cross validate routine that is shared by crossValidate and crossValidateBatch.
     */
    direct (Validate) ("crossValidateRaw", (T,M,R,TS), CurriedMethodSignature(List(List(
                                                 ("dataSet", TS(T,MBoolean)),
                                                 ("train", TS(T,MBoolean) ==> M),
                                                 ("_numFolds", MInt, "unit(10)")),
                                               List(
                                                 ("evalTestSet", (M,TS(T,MBoolean)) ==> R)
                                               )
                                            ), DenseVector(R)), TTrainingSetLike(T,MBoolean,TS(T,MBoolean))) implements composite ${


      fassert(dataSet.numSamples > 2, "Cannot cross validate dataset with less than 2 samples")

      val numFolds = max(min(_numFolds, dataSet.numSamples), 2)
      val shuffledIndices = shuffle(0::dataSet.numSamples)
      val numSamplesPerFold = dataSet.numSamples / numFolds

      (0::numFolds) { i =>
        val testSamplesOffset = i*numSamplesPerFold
        val testSampleIndices = testSamplesOffset::testSamplesOffset+numSamplesPerFold
        val trainingSampleLeftIndices = 0::max(0,testSamplesOffset-numSamplesPerFold).toInt
        val trainingSampleRightIndices = (testSamplesOffset+numSamplesPerFold)::dataSet.numSamples
        val trainingSampleIndices = IndexVector(trainingSampleLeftIndices << trainingSampleRightIndices)

        val sourceTrainingSampleIndices = shuffledIndices(trainingSampleIndices)
        val sourceTestSampleIndices = shuffledIndices(testSampleIndices)

        val trainingSet = dataSet.getRows(sourceTrainingSampleIndices)
        val model = train(trainingSet)

        val testSet = dataSet.getRows(sourceTestSampleIndices)
        evalTestSet(model, testSet)
      }
    }

    /**
     * Compute a cross-validated score for the classifier using a user-specified metric from a confusion matrix
     * to a score (e.g. accuracy, precision).
     */
    direct (Validate) ("crossValidate", (T,M,TS), MethodSignature(List(
                                               ("dataSet", TS(T,MBoolean)),
                                               ("train", TS(T,MBoolean) ==> M),
                                               ("classify", (M,TS(T,MBoolean),MInt) ==> MBoolean),
                                               ("metric", DenseMatrix(MInt) ==> MDouble),
                                               ("numFolds", MInt, "unit(10)"),
                                               ("verbose", MBoolean, "unit(false)")
                                            ), MDouble), TTrainingSetLike(T,MBoolean,TS(T,MBoolean))) implements composite ${

      def classifyWithModel(m: Rep[M], set: Rep[TS[T,Boolean]])(i: Rep[Int]): Rep[Boolean] = classify(m, set, i)

      val foldResults = crossValidateRaw[T,M,Double,TS](dataSet, train, numFolds) { (model, testSet) =>
        val conf = confusionMatrix(testSet, classifyWithModel(model, testSet))

        if (verbose) {
          println("confusionMatrix: [TP FP; FN TN]")
          conf.pprint
        }

        metric(conf)
      }

      mean(foldResults)
    }

   /**
    * The same as crossValidate, except with a batch of test samples at a time.
    */
    direct (Validate) ("crossValidateBatch", (T,M,TS), MethodSignature(List(
                                               ("dataSet", TS(T,MBoolean)),
                                               ("train", TS(T,MBoolean) ==> M),
                                               ("classify", (M,TS(T,MBoolean),IndexVector) ==> DenseVector(MBoolean)),
                                               ("metric", DenseMatrix(MInt) ==> MDouble),
                                               ("numFolds", MInt, "unit(10)"),
                                               ("verbose", MBoolean, "unit(false)")
                                            ), MDouble), TTrainingSetLike(T,MBoolean,TS(T,MBoolean))) implements composite ${

      def classifyWithModel(m: Rep[M], set: Rep[TS[T,Boolean]])(i: Rep[IndexVector]): Rep[DenseVector[Boolean]] = classify(m, set, i)

      val foldResults = crossValidateRaw[T,M,Double,TS](dataSet, train, numFolds) { (model, testSet) =>
        val conf = confusionMatrixBatch(testSet, classifyWithModel(model, testSet))

        if (verbose) {
          println("confusionMatrix: [TP FP; FN TN]")
          conf.pprint
        }

        metric(conf)
      }

      mean(foldResults)
    }

    /*
     * Compute the cross-validated area under the ROC curve for a probabilistic classifier returning a value between 0.0 and 1.0,
     * evaluated with different classification thresholds. For each fold, we evaluate the AUC, and take the average across folds.
     */
    direct (Validate) ("crossValidateAUC", (T,M,TS), MethodSignature(List(
                                               ("dataSet", TS(T,MBoolean)),
                                               ("train", TS(T,MBoolean) ==> M),
                                               ("classify", (M,TS(T,MBoolean),MInt) ==> MDouble),
                                               ("numFolds", MInt, "unit(10)"),
                                               ("numThresholds", MInt, "unit(10)")
                                            ), MDouble), TTrainingSetLike(T,MBoolean,TS(T,MBoolean))) implements composite ${

      def classifyWithModel(m: Rep[M], set: Rep[TS[T,Boolean]], t: Rep[Double])(i: Rep[Int]): Rep[Boolean] = classify(m, set, i) > t

      val AUCs = crossValidateRaw[T,M,Double,TS](dataSet, train, numFolds) { (model, testSet) =>
        AUC(ROCCurve(testSet, t => classifyWithModel(model, testSet, t), numThresholds))
      }

      mean(AUCs)
    }

    /**
     * Same as crossValidateAUC, except with a batch of test samples at a time.
     */
    direct (Validate) ("crossValidateAUCBatch", (T,M,TS), MethodSignature(List(
                                               ("dataSet", TS(T,MBoolean)),
                                               ("train", TS(T,MBoolean) ==> M),
                                               ("classify", (M,TS(T,MBoolean),IndexVector) ==> DenseVector(MDouble)),
                                               ("numFolds", MInt, "unit(10)"),
                                               ("numThresholds", MInt, "unit(10)")
                                            ), MDouble), TTrainingSetLike(T,MBoolean,TS(T,MBoolean))) implements composite ${

      def classifyWithModel(m: Rep[M], set: Rep[TS[T,Boolean]], t: Rep[Double])(x: Rep[IndexVector]): Rep[DenseVector[Boolean]] = classify(m, set, x).map(_ > t)

      val AUCs = crossValidateRaw[T,M,Double,TS](dataSet, train, numFolds) { (model, testSet) =>
        AUC(ROCCurveBatch(testSet, t => classifyWithModel(model, testSet, t), numThresholds))
      }

      mean(AUCs)
    }

    // The (x,y) point on the ROC curve for the given classifier, representing (fpr, tpr)
    direct (Validate) ("ROC", Nil, DenseMatrix(MInt) :: Tup2(MDouble,MDouble)) implements composite ${
      pack((fallout($0), sensitivity($0)))
    }

    direct (Validate) ("ROCCurve", (T,TS), MethodSignature(List(
                                        ("testSet", TS(T,MBoolean)),
                                        ("classify", (MDouble ==> (MInt ==> MBoolean))),
                                        ("numThresholds", MInt, "unit(10)")
                                      ), DenseVector(Tup2(MDouble,MDouble))), TTrainingSetLike(T,MBoolean,TS(T,MBoolean))) implements composite ${

      (0::numThresholds) { t =>
        val threshold = t.toDouble / numThresholds
        val conf = confusionMatrix(testSet, classify(threshold))
        ROC(conf)
      }
    }

    direct (Validate) ("ROCCurveBatch", (T,TS), MethodSignature(List(
                                             ("testSet", TS(T,MBoolean)),
                                             ("classify", (MDouble ==> (IndexVector ==> DenseVector(MBoolean)))),
                                             ("numThresholds", MInt, "unit(10)")
                                           ), DenseVector(Tup2(MDouble,MDouble))), TTrainingSetLike(T,MBoolean,TS(T,MBoolean))) implements composite ${

      (0::numThresholds) { t =>
        val threshold = t.toDouble / numThresholds
        val conf = confusionMatrixBatch(testSet, classify(threshold))
        ROC(conf)
      }
    }

    direct (Validate) ("AUC", Nil, ("unsortedROCs", DenseVector(Tup2(MDouble,MDouble))) :: MDouble) implements composite ${
      // increasing by TPR then FPR (i.e. up and to the right)
      val sorted = unsortedROCs.sortBy(t => t._1).sortBy(t => t._2)

      // add end points
      val curve = DenseVector(pack((unit(0.0),unit(0.0)))) << sorted << DenseVector(pack((unit(1.0),unit(1.0))))

      // trapezoidal approximation of area under the curve
      sum(0, curve.length-1) { i =>
        val x_i = curve(i)._1
        val x_i_next = curve(i+1)._1
        val y_i = curve(i)._2
        val y_i_next = curve(i+1)._2

        val base = x_i_next - x_i
        val height = (y_i_next + y_i) / 2.0
        base * height
      }
    }

    direct (Validate) ("accuracy", Nil, DenseMatrix(MInt) :: MDouble) implements composite ${
      val TP = $0(0,0)
      val TN = $0(1,1)
      (TP + TN).toDouble / sum($0).toDouble
    }

    // Proportion of examples classified as positive that were actually positive.
    direct (Validate) ("precision", Nil, DenseMatrix(MInt) :: MDouble) implements composite ${
      val TP = $0(0,0)
      val FP = $0(0,1)
      TP.toDouble / (TP + FP).toDouble
    }

    // a.k.a. True negative rate (TNR). Proportion of actual negative examples that were correctly classified.
    direct (Validate) ("specificity", Nil, DenseMatrix(MInt) :: MDouble) implements composite ${
      val TN = $0(1,1)
      val FP = $0(0,1)
      TN.toDouble / (FP + TN).toDouble
    }

    // a.k.a. True positive rate (TPR). Proportion of actual positive examples that were correctly classified. a.k.a. Recall.
    direct (Validate) ("sensitivity", Nil, DenseMatrix(MInt) :: MDouble) implements composite ${
      val TP = $0(0,0)
      val FN = $0(1,0)
      TP.toDouble / (TP + FN).toDouble
    }

    direct (Validate) ("recall", Nil, DenseMatrix(MInt) :: MDouble) implements redirect ${ sensitivity($0) }

    // a.k.a. False positive rate (FPR). Proportion of negative examples that were incorrectly classified.
    direct (Validate) ("fallout", Nil, DenseMatrix(MInt) :: MDouble) implements composite ${
      1.0 - specificity($0)
    }

    direct (Validate) ("fscore", Nil, DenseMatrix(MInt) :: MDouble) implements composite ${
      2.0 / ((1.0 / precision($0)) + (1.0 / recall($0)))
    }
  }
}
