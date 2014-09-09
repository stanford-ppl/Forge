package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * An implementation of the CART decision tree learning algorithm
 * based on the sklearn implementation:
 *   https://github.com/scikit-learn/scikit-learn/blob/master/sklearn/tree/_tree.pyx
 *
 * Reference: ftp://ftp.boulder.ibm.com/software/analytics/spss/support/Stats/Docs/Statistics/Algorithms/14.0/TREE-CART.pdf
 */
trait TreeOps {
  this: OptiMLDSL =>

  def importTreeOps() {
    val TrainingSet = lookupTpe("TrainingSet")
    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")
    val Tup2 = lookupTpe("Tup2")
    val Tup3 = lookupTpe("Tup3")
    val Tup7 = lookupTpe("Tup7")

    /* Identifiers for tree building parameters */
    val TCriterion = tpe("TCriterion", stage = compile)
    identifier (TCriterion) ("MSE")   // for continuous-valued labels
    identifier (TCriterion) ("Gini")  // for discrete-valued labels  

    val Tree = tpe("DecisionTree")

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Tree definition

    /* 
     * Array-based binary decision tree. Each array is of length N, where N is the
     * number of nodes in the tree, and index i represents information about node i.
     */
    data(Tree, 
        ("_numNodes", MInt),               // number of nodes in the tree
        ("_capacity", MInt),               // the size of the internal arrays, >= numNodes
        ("_isLeaf", MArray(MBoolean)),     // isLeaf(i) is true if node i is a leaf, false otherwise
        ("_leftChildren", MArray(MInt)),   // leftChildren(i) is the node id of the left child of i
        ("_rightChildren", MArray(MInt)),  // rightChildren(i) is the node id of the right child of i
        ("_feature", MArray(MInt)),        // feature(i) is the index j of the feature to split on for node i
        ("_threshold", MArray(MDouble)),   // threshold(i) is the splitting threshold for node i
        ("_value", MArray(MDouble)),       // value(i) is the constant prediction value for node i
        ("_impurity", MArray(MDouble)),    // impurity(i) is the value of the splitting criterion for node i
        ("_numNodeSamples", MArray(MInt))) // numNodeSamples(i) is the number of training samples reaching node i

    compiler (Tree) ("alloc_tree", Nil, ("initCapacity", MInt) :: Tree, effect = mutable) implements allocates(Tree,
      ${unit(0)},
      ${initCapacity},
      ${array_empty[Boolean](initCapacity)},
      ${array_empty[Int](initCapacity)},
      ${array_empty[Int](initCapacity)},
      ${array_empty[Int](initCapacity)},
      ${array_empty[Double](initCapacity)},
      ${array_empty[Double](initCapacity)},
      ${array_empty[Double](initCapacity)},
      ${array_empty[Int](initCapacity)}
    )

    val TreeOps = withTpe(Tree)
    TreeOps {
      // getters and setters
      infix ("numNodes") (Nil :: MInt) implements getter(0, "_numNodes")
      compiler ("infix_set_num_nodes") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numNodes", ${$1})

      infix ("capacity") (Nil :: MInt) implements getter(0, "_capacity")
      compiler ("infix_set_capacity") (MInt :: MUnit, effect = write(0)) implements setter(0, "_capacity", ${$1})

      infix("isLeaf") (Nil :: MArray(MBoolean)) implements getter(0, "_isLeaf")
      compiler ("infix_set_is_leaf") (MArray(MBoolean) :: MUnit, effect = write(0)) implements setter(0, "_isLeaf", ${$1})

      infix ("leftChildren") (Nil :: MArray(MInt)) implements getter(0, "_leftChildren")
      compiler ("infix_set_left_children") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_leftChildren", ${$1})

      infix ("rightChildren") (Nil :: MArray(MInt)) implements getter(0, "_rightChildren")
      compiler ("infix_set_right_children") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_rightChildren", ${$1})

      infix ("feature") (Nil :: MArray(MInt)) implements getter(0, "_feature")
      compiler ("infix_set_feature") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_feature", ${$1})

      infix ("threshold") (Nil :: MArray(MDouble)) implements getter(0, "_threshold")
      compiler ("infix_set_threshold") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_threshold", ${$1})

      infix ("value") (Nil :: MArray(MDouble)) implements getter(0, "_value")
      compiler ("infix_set_value") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_value", ${$1})

      infix ("impurity") (Nil :: MArray(MDouble)) implements getter(0, "_impurity")
      compiler ("infix_set_impurity") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_impurity", ${$1})

      infix ("numNodeSamples") (Nil :: MArray(MInt)) implements getter(0, "_numNodeSamples")
      compiler ("infix_set_num_node_samples") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_numNodeSamples", ${$1})

      // Adds a node to the tree. The new node registers itself as the child of its parent.
      infix ("addNode") (MethodSignature(List(
                          ("parent", MInt), ("isLeft", MBoolean), ("isLeaf", MBoolean), 
                          ("feature", MInt), ("threshold", MDouble), ("impurity", MDouble),
                          ("numNodeSamples", MInt)), MInt), effect = write(0)) implements composite ${

        val nodeId = $self.numNodes
        if (nodeId >= $self.capacity) {
          tree_realloc($self, $self.capacity+1)
        }

        if (parent != -1) { // -1 means that we are the root node
          if (isLeft)
            $self.leftChildren(parent) = nodeId        
          else 
            $self.rightChildren(parent) = nodeId
        }

        if (!isLeaf) {
          $self.feature(nodeId) = feature
          $self.threshold(nodeId) = threshold
        }

        $self.isLeaf(nodeId) = isLeaf
        $self.impurity(nodeId) = impurity
        $self.numNodeSamples(nodeId) = numNodeSamples

        $self.set_num_nodes($self.numNodes + 1)      

        nodeId  
      } 
    }

    compiler (Tree) ("tree_realloc", Nil, (("tree", Tree), ("minCapacity", MInt)) :: MUnit, effect = write(0)) implements composite ${          
      var n = max(4, tree.capacity * 2)
      while (n < minCapacity) n = n*2
      
      val newIsLeaf = array_empty[Boolean](n)
      array_copy(tree.isLeaf, 0, newIsLeaf, 0, $tree.numNodes)
      tree.set_is_leaf(newIsLeaf)

      val newLeftChildren = array_empty[Int](n)
      array_copy(tree.leftChildren, 0, newLeftChildren, 0, $tree.numNodes)
      tree.set_left_children(newLeftChildren)

      val newRightChildren = array_empty[Int](n)
      array_copy(tree.rightChildren, 0, newRightChildren, 0, $tree.numNodes)
      tree.set_right_children(newRightChildren)

      val newFeature = array_empty[Int](n)
      array_copy(tree.feature, 0, newFeature, 0, $tree.numNodes)
      tree.set_feature(newFeature)

      val newThreshold = array_empty[Double](n)
      array_copy(tree.threshold, 0, newThreshold, 0, $tree.numNodes)
      tree.set_threshold(newThreshold)

      val newValue = array_empty[Double](n)
      array_copy(tree.value, 0, newValue, 0, $tree.numNodes)
      tree.set_value(newValue)

      val newImpurity = array_empty[Double](n)
      array_copy(tree.impurity, 0, newImpurity, 0, $tree.numNodes)
      tree.set_impurity(newImpurity)
      
      val newNumNodeSamples = array_empty[Int](n)
      array_copy(tree.numNodeSamples, 0, newNumNodeSamples, 0, $tree.numNodes)
      tree.set_num_node_samples(newNumNodeSamples)

      tree.set_capacity(n)
    }
    
    compiler (Tree) ("init_tree", Nil, ("maxDepth", MInt) :: Tree) implements composite ${
      val initCapacity =
        if (maxDepth <= 10) (pow(2, maxDepth+1) - 1).toInt
        else 2047

      alloc_tree(initCapacity) 
    }


    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Tree construction

    /* Construct the decision tree in a depth-first fashion */
    direct (Tree) ("dtree", Nil, MethodSignature(List(
                                   ("trainingSet",TrainingSet(MDouble,MDouble)), 
                                   ("_maxDepth", MInt, "unit(-1)"),
                                   ("_maxNumFeatures", MInt, "unit(-1)"),                                   
                                   ("minSamplesSplit", MInt, "unit(2)"),
                                   ("minSamplesLeaf", MInt, "unit(1)"),
                                   ("criterion", TCriterion, "MSE")
                                 ), Tree)) implements composite ${

      // optional value fakery
      val maxDepth = if (_maxDepth < 0) INF.toInt else _maxDepth
      val maxNumFeatures = if (_maxNumFeatures < 0) sqrt(trainingSet.numFeatures).toInt else _maxNumFeatures

      // constants
      val MIN_IMPURITY_SPLIT = 1e-7

      val tree = init_tree(maxDepth)

      // create a lifted function we can call recursively
      val process = doLambda({(t: Rep[Tup7[IndexVector,Int,Int,Boolean,Double,DenseVector[Boolean],Any]]) =>
        // an IndexVector in [0,numSamples] containing the partition of the TrainingSet we are currently working on
        val samples = t._1 

        // the current depth of the tree
        val depth = t._2

        // the parent of the node we are currently working on
        val parent = t._3

        // true if this node is a left child of the parent, false if right
        val isLeft = t._4

        // the computed impurity for this node
        val impurity = t._5

        // a DenseVector of length numFeatures where constantFeatures(i) = true if index i is known to be constant-valued on this partition, and false otherwise
        val constantFeatures = t._6

        // the lifted lambda for this function - used to call recursively
        val continuation = t._7
        val next = continuation.AsInstanceOf[Tup7[IndexVector,Int,Int,Boolean,Double,DenseVector[Boolean],Any] => Unit]

        val numNodeSamples = samples.length

        // stopping conditions
        val isLeaf = 
          (depth >= maxDepth) || 
          (numNodeSamples < minSamplesSplit) || 
          (numNodeSamples < 2*minSamplesLeaf) || 
          (impurity <= MIN_IMPURITY_SPLIT)

        if (isLeaf) {
          val nodeId = tree.addNode(parent, isLeft, isLeaf, 0, 0.0, impurity, numNodeSamples)
          tree.value(nodeId) = tree_score(trainingSet, samples, criterion) 
          ()
        } 
        else {
          // try to split this node
          val (sortedSamples, pos, threshold, feature, impurityLeft, impurityRight, nextConstantFeatures) = unpack(tree_split(tree, trainingSet, samples, maxNumFeatures, impurity, constantFeatures, criterion))
          
          // add node to tree
          val nodeId = tree.addNode(parent, isLeft, isLeaf, feature, threshold, impurity, numNodeSamples)

          // right child
          doApply(next, pack((sortedSamples(pos::sortedSamples.length), depth+1, nodeId, unit(false), impurityRight, nextConstantFeatures, continuation)))

          // left child
          doApply(next, pack((sortedSamples(0::pos), depth+1, nodeId, unit(true), impurityLeft, nextConstantFeatures, continuation)))
        }    

        ()
      })

      // kick off the tree construction
      // this has the side-effect of updating "tree", so that we don't have to continuously copy the internal arrays)
      val allSamples = trainingSet.data.rowIndices
      doApply(process, pack((allSamples, unit(0), unit(-1), unit(false), compute_impurity(trainingSet, allSamples, criterion), (0::trainingSet.numFeatures) { i => false }, process.AsInstanceOf[Any])))

      tree.unsafeImmutable
    }

    compiler (Tree) ("tree_split", Nil, MethodSignature(List(("tree", Tree), ("trainingSet", TrainingSet(MDouble,MDouble)), ("samples", IndexVector), ("maxNumFeatures", MInt), ("impurity", MDouble), ("constantFeatures", DenseVector(MBoolean)), ("criterion", TCriterion)), Tup7(IndexVector,MInt,MDouble,MInt,MDouble,MDouble,DenseVector(MBoolean)))) implements composite ${
      fassert(samples.length > 1, "samples to split must be at least 2")

      // constants
      val FEATURE_THRESHOLD = 1e-7

      // sample up to maxNumFeatures and evaluate impurity to find the best feature to split on
      val candidateFeatures = trainingSet.data.colIndices filter { i => !constantFeatures(i) }
      val pct = maxNumFeatures.toDouble / candidateFeatures.length.toDouble 
      val testFeatures = if (pct < 1.0) sample(candidateFeatures, pct) else candidateFeatures

      // for each feature, evaluate all possible splits for improvement
      val improvements = testFeatures { j =>
        // sort samples along this feature
        val featureValues = trainingSet.data.apply(samples).getCol(j)        
        val (sortedValues, sortedIndices) = featureValues.sortWithIndex
        val sortedSamples = samples(sortedIndices)

        // constant feature value gets a -INF score
        if (sortedValues(0) == sortedValues(sortedValues.length-1)) {
          pack((-INF, sortedSamples, j, unit(0.0), unit(0.0), unit(0.0)))
        }
        else {        
          // compute scores for each candidate split
          var p = 1
          var bestPos = 1
          var bestScore = -INF
          var bestImpurityLeft = -INF
          var bestImpurityRight = -INF
          
          while (p < sortedValues.length - 1) {
            // skip to the next p at least FEATURE_THRESHOLD greater than the current value
            while ((p+1 < sortedValues.length - 1) && (abs(sortedValues.apply(p+1) - sortedValues.apply(p)) > FEATURE_THRESHOLD))
              p += 1
            
            val (improvement, impurityLeft, impurityRight) = unpack(compute_impurity_improvement(trainingSet, impurity, sortedSamples, p, criterion))

            if (improvement > bestScore) {
              bestScore = improvement
              bestPos = p
              bestImpurityLeft = impurityLeft              
              bestImpurityRight = impurityRight
            }

            p += 1
          }

          val threshold = (sortedValues(bestPos-1) + sortedValues(bestPos)) / 2.0
            
          pack((readVar(bestScore), sortedSamples, readVar(bestPos), threshold, readVar(bestImpurityLeft), readVar(bestImpurityRight)))
        }                        
      }

      val bestFeatureIndex = improvements.map(_._1).maxIndex      
      val bestFeature = testFeatures(bestFeatureIndex)
      val bestSort = improvements(bestFeatureIndex)._2
      val bestPos = improvements(bestFeatureIndex)._3
      val threshold = improvements(bestFeatureIndex)._4
      val impurityLeft = improvements(bestFeatureIndex)._5
      val impurityRight = improvements(bestFeatureIndex)._6
    
      val newConstantFeatures = improvements filter { t => t._1 == -INF } map { _._3 }
      val nextConstantFeatures = constantFeatures.mutable
      for (j <- newConstantFeatures) {
        nextConstantFeatures(j) = true      
      }      
      pack((bestSort, bestPos, threshold, bestFeature, impurityLeft, impurityRight, nextConstantFeatures))
    }  


    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Impurity Criterion 

    compiler (Tree) ("compute_impurity_improvement", Nil, (("trainingSet", TrainingSet(MDouble,MDouble)), ("impurity", MDouble), ("samples", IndexVector), ("splitPos", MInt), ("criterion", TCriterion)) :: Tup3(MDouble,MDouble,MDouble)) implements composite ${
      val leftSamples = samples(0::splitPos)
      val rightSamples = samples(splitPos::samples.length)

      val impurityLeft = compute_impurity(trainingSet, leftSamples, criterion)
      val impurityRight = compute_impurity(trainingSet, rightSamples, criterion)

      val newImpurity = (leftSamples.length.toDouble / samples.length * impurityLeft) + (rightSamples.length.toDouble / samples.length * impurityRight)
      val improvement = (samples.length.toDouble / trainingSet.numSamples) * (impurity - newImpurity)

      pack((improvement, impurityLeft, impurityRight))
    }

    compiler (Tree) ("compute_impurity", Nil, (("trainingSet", TrainingSet(MDouble,MDouble)), ("samples", IndexVector), ("criterion", TCriterion)) :: MDouble) implements composite ${
      criterion match {
        case MSE => impurity_mse(trainingSet, samples)
        case Gini => impurity_gini(trainingSet, samples)
      }
    }

    /* 
     * Mean Squared Error (MSE) regression criterion
     */
    compiler (Tree) ("impurity_mse", Nil, (("trainingSet", TrainingSet(MDouble,MDouble)), ("samples", IndexVector)) :: MDouble) implements composite ${
      variance(trainingSet.labels.apply(samples))
    }
   
    /**
     * Gini index classification criterion
     */
    compiler (Tree) ("impurity_gini", Nil, (("trainingSet", TrainingSet(MDouble,MDouble)), ("samples", IndexVector)) :: MDouble) implements composite ${
      val labels = trainingSet.labels.apply(samples)
      val numSamplesByLabel = labels.groupByReduce(l => l, l => 1, (a: Rep[Int],b: Rep[Int]) => a+b)
      
      1.0 - sum(0, numSamplesByLabel.keys.length) { ki =>
        val k = numSamplesByLabel.keys.apply(ki)
        // proportion of class k observations in the current set of samples
        val freq = numSamplesByLabel(k).toDouble / samples.length
        square(freq)
      }
    }

    /**
     * The value to store as the prediction for this leaf.
     */
    compiler (Tree) ("tree_score", Nil, (("trainingSet", TrainingSet(MDouble,MDouble)), ("samples", IndexVector), ("criterion", TCriterion)) :: MDouble) implements composite ${
      criterion match {
        case MSE => tree_score_mse(trainingSet, samples)
        case Gini => tree_score_gini(trainingSet, samples)
      }
    }

    compiler (Tree) ("tree_score_mse", Nil, (("trainingSet", TrainingSet(MDouble,MDouble)), ("samples", IndexVector)) :: MDouble) implements composite ${
      // regression prediction value is the mean of values assigned to this node 
      mean(trainingSet.labels.apply(samples))
    }  

    compiler (Tree) ("tree_score_gini", Nil, (("trainingSet", TrainingSet(MDouble,MDouble)), ("samples", IndexVector)) :: MDouble) implements composite ${
      // select the label with the highest frequency in the sample
      val allLabels = trainingSet.labels.apply(samples)
      val samplesByLabel = allLabels.groupByReduce(l => l, l => 1, (a: Rep[Int],b: Rep[Int]) => a+b)
      val mostFrequent = densevector_fromarray(fhashmap_values(samplesByLabel), true).maxIndex
      samplesByLabel.keys.apply(mostFrequent)
    }


    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Prediction

    infix (Tree) ("predict", Nil, (("tree", Tree), ("testPt", DenseVector(MDouble))) :: MDouble) implements composite ${
      fassert(tree.numNodes > 1, "decision tree is empty")

      var node = 0
      while (!tree.isLeaf.apply(node)) {
        val feature = tree.feature.apply(node)
        val threshold = tree.threshold.apply(node)
        if (testPt(feature) <= threshold) {
          node = tree.leftChildren.apply(node)
        }
        else {
          node = tree.rightChildren.apply(node)
        }
      }

      // prediction value for this leaf
      tree.value.apply(node)      
    }


    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Visualization

    infix (Tree) ("pprint", Nil, (("tree", Tree)) :: MUnit, effect = simple) implements composite ${
      val process = doLambda({(t: Rep[Tup2[Int,Any]]) => 
        val node = t._1
        val continuation = t._2
        val next = continuation.AsInstanceOf[Tup2[Int,Any] => Unit]

        val feature = tree.feature.apply(node)
        val threshold = tree.threshold.apply(node)
        val leftChild = tree.leftChildren.apply(node)
        val rightChild = tree.rightChildren.apply(node)
        val isLeaf = tree.isLeaf.apply(node)

        println("Node: " + node)
        if (!isLeaf) {
          println("If feature " + feature + " <= " + threshold + " then node " + leftChild + " else node " + rightChild)
          println()      
          doApply(next, pack((leftChild, continuation)))
          doApply(next, pack((rightChild, continuation)))
        }
        else {
          println("Leaf covering " + tree.numNodeSamples.apply(node) + " samples. Prediction value is: " + tree.value.apply(node))
          println()      
        }        
      })
      
      doApply(process, pack((unit(0), process.AsInstanceOf[Any])))
    }
  }
}
