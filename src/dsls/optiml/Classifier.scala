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
    val SparseVector = lookupTpe("SparseVector")
    val SparseTrainingSet = lookupTpe("SparseTrainingSet")

    val D = tpePar("D")
    val L = tpePar("L")
    val TS = hkTpePar("TS", (D,L))

    val Classifier = grp("Classifier")

    /**
     * Random forest. The training set must be dense.
     */
    val Tree = lookupTpe("DecisionTree")
    val Forest = tpe("RandomForest")
    data(Forest, ("_trees", DenseVector(Tree)))

    compiler (Forest) ("alloc_forest", Nil, ("trees", DenseVector(Tree)) :: Forest) implements allocates(Forest, ${$0})

    // For each tree, we default to using approximately 2/3rds of the total available samples
    // http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#remarks
    direct (Classifier) ("rforest", Nil, MethodSignature(List(
        ("trainingSet",DenseTrainingSet(MDouble,MBoolean)),
        ("numTrees", MInt, "unit(10)"),
        ("samplingRate", MDouble, "unit(0.66)"),
        ("maxDepth", MInt, "unit(-1)"),
        ("maxNumFeatures", MInt, "unit(-1)"),
        ("minSamplesSplit", MInt, "unit(2)"),
        ("minSamplesLeaf", MInt, "unit(1)"),
        ("verbose", MBoolean, "unit(false)")
      ), Forest)) implements composite ${

      val y = trainingSet.labels map { label => if (label) 1.0 else 0.0 }
      val data = DenseTrainingSet(trainingSet.data, y)

      if (verbose) {
        println("Building random forest with " + numTrees + " trees")
      }
      val start = time()
      val trees = (0::numTrees) { i =>
        val sampleIndices = sample(0::data.numSamples, samplingRate)
        dtree(data, maxDepth, maxNumFeatures, minSamplesSplit, minSamplesLeaf, sampleIndices, Gini)
      }
      val end = time(trees)
      if (verbose) {
        println("Random forest complete! training took " + (end-start) / 1000.0 + "s")
      }

      alloc_forest(trees)
    }

    val ForestOps = withTpe(Forest)
    ForestOps {
      infix ("trees") (Nil :: DenseVector(Tree)) implements getter(0, "_trees")

      infix ("predict") (("testPt", DenseVector(MDouble)) :: MDouble) implements composite ${
        fassert($self.trees.length > 0, "random forest is empty")

        // Take majority vote
        val predictions = $self.trees.map(t => t.predict(testPt))
        val counts = predictions.histogram
        val majorityIndex = counts.toVector.maxIndex
        counts.keys.apply(majorityIndex)
      }
    }

    /**
     * Logistic regression with dense parameters. The training set can be dense or sparse.
     */
    direct (Classifier) ("logreg", TS, MethodSignature(List(
        ("data",TS(MDouble,MBoolean)),
        ("stochastic", MBoolean, "unit(true)"),
        ("initLearningRate", MDouble, "unit(1.0)"),
        ("maxIter", MInt, "unit(30)"),
        ("lambda", MDouble, "unit(0.0)"),
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
            next += gradient*alpha - lambda*next
          }
          val ret = next.unsafeImmutable
          callback(ret, iter)
          ret
        }
      }
      else {
        untilconverged(theta, maxIter = _maxIter, verbose = _verbose) { (cur, iter) =>
          // the adaptive rate does not seem to work as well for batch
          // val alpha = initLearningRate / (1.0 + initLearningRate*iter)
          val alpha = initLearningRate
          val gradient =
            sum(0, data.numSamples) { i =>
              data.timesScalar(i, y(i) - sigmoid(data.dot(i, cur)))
            }

          val next = cur + gradient*alpha - lambda*cur
          callback(next, iter)
          next
        }
      }
    }

    /**
     * Logistic regression with sparse parameters. The training set must be sparse.
     *
     * FIXME: With regularization turned on, this is not producing exactly the same
     *        results as dense logreg. However, it still seems to work, more or less.
     */
    direct (Classifier) ("sparseLogreg", Nil, MethodSignature(List(
        ("data",SparseTrainingSet(MDouble,MBoolean)),
        ("stochastic", MBoolean, "unit(true)"),
        ("initLearningRate", MDouble, "unit(1.0)"),
        ("maxIter", MInt, "unit(30)"),
        ("lambda", MDouble, "unit(0.0)"),
        ("verbose", MBoolean, "unit(false)"),
        ("callback", (SparseVector(MDouble), MInt) ==> MUnit, "(m,i) => unit(())")
      ), SparseVector(MDouble))) implements composite ${

      val theta = SparseVector.zeros(data.numFeatures)
      val y = data.labels map { label => if (label) 1.0 else 0.0 }

      val _maxIter = maxIter
      val _verbose = verbose
      val lastUpdated = DenseVector[Int](data.numFeatures, true)
      lastUpdated(0::lastUpdated.length) = -1

      // Sparse gradient descent with logistic function
      if (stochastic) {
        untilconverged(theta, maxIter = _maxIter, verbose = _verbose) { (cur, iter) =>
          val alpha = initLearningRate / (1.0 + initLearningRate*iter)
          var next = cur

          for (i <- 0 until data.numSamples) {
            val gradient: Rep[SparseVector[Double]] =
              data(i)*(y(i) - sigmoid(data(i) *:* readVar(next)))

            // Fails to compile (not found: value gradient.nnz) without explicit types
            // and the explicit numRegularizations.toDouble call
            val denseUpdates: Rep[DenseVector[Double]] =
              gradient.indices.zip(gradient.nz) { (j: Rep[Int],ge: Rep[Double]) =>
                val numRegularizations: Rep[Int] = i - lastUpdated(j)
                lastUpdated(j) = i
                val batchRegularization: Rep[Double] =
                  pow(1.0-lambda, numRegularizations.toDouble)*next(j) - next(j)

                ge*alpha + batchRegularization
              }

            val sparseUpdates: Rep[SparseVector[Double]] =
              SparseVector.fromSortedElements(
                data.numFeatures,
                true,
                gradient.indices,
                denseUpdates
              )

            next = next + sparseUpdates
          }

          // Apply remaining batched regularization updates
          val batchRegularization = next.indices.zip(next.nz) { (j,e) =>
            val numRegularizations = data.numSamples - 1 - lastUpdated(j)
            if (numRegularizations > 0)
              pow(1.0-lambda, numRegularizations.toDouble)*e - e
            else 0.0
          }

          lastUpdated(0::lastUpdated.length) = -1

          val ret = SparseVector.fromSortedElements(
            data.numFeatures,
            true,
            next.indices,
            next.nz - batchRegularization
          )

          callback(ret, iter)
          ret
        }
      }
      else {
        // TODO: No SparseVector arith type class yet, which is needed to sum the
        // DenseVector of SparseVectors here
        fatal("Batch gradient descent is not yet supported with sparseLogreg")

        untilconverged(theta, maxIter = _maxIter, verbose = _verbose) { (cur, iter) =>
          val alpha = initLearningRate / (1.0 + initLearningRate*iter)

          val gradient = SparseVector.zeros(10)
          // val gradient =
          //   ((0::data.numSamples) { i =>
          //     data(i)*(y(i) - sigmoid(data(i) *:* cur))
          //   }).sum

          val next = cur + gradient*alpha - cur*lambda
          callback(next, iter)
          next
        }
      }
    }

  }
}
