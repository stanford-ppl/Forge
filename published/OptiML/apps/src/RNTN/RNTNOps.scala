import optiml.compiler._
import optiml.library._
import optiml.shared._

trait RNTNOps extends OptiMLApplication with Utilities {
	
	private val TREECOLS = 6
	private val PARENT = 0  		// Gives the index of the parent node of the node
	private val LKID   = 1  		// Gives the index of the left child of the node
	private val RKID   = 2  		// Gives the index of the right child of the node
	private val WORD   = 3       	// Gives the phrase index represented by the node
	private val LABEL  = 4       	// Gives the predetermined sentiment score for the node
	private val LEAFS  = 5       	// Gives the number of leaves the node is an ancestor of

	private val POSNEGNEUT = 5

	private val WORDI 		  = true
	//private val QRNN  		  = true
	private val NUMCLASSES 	  = 5
	private val NEUTRALLABEL  = if (NUMCLASSES == 2)  POSNEGNEUT else (NUMCLASSES - 1)/2
	//private val NEUTRALWEIGHT = if (NUMCLASSES == 2)  0.0		 else 1.0  	// 0.3 as default?
	private val WORDSIZE      = 25

	private val ADAZERO = true
	//private val ADAGRAD = true
	private val LR		= 0.01
	private val ADAEPS  = 0.001

	private val regC_Wc 	  = 0.0001
	private val regC_W	   	  = 0.1
	private val regC_Wt 	  = 0.15
	private val regC_Wv		  = 0.0001

	private val VERBOSE = false

	// -------------------------------------------------------------------------------------//
	//										Activation of Node						  		//
	// -------------------------------------------------------------------------------------//
	def computeNode(
		node: 	  Rep[DenseVector[Int]],
		kidsActs: Rep[DenseVector[DenseVector[Double]]],
		Wc:   	  Rep[DenseMatrix[Double]],
		W:    	  Rep[DenseMatrix[Double]],
		Wt:       Rep[DenseMatrix[Double]],
		Wv:   	  Rep[DenseMatrix[Double]]
	) = {
		val lKid = node(LKID)
		val act = 	if 		(lKid == -1 && WORDI) { Wv.getCol(node(WORD)).toDense }
					else if (lKid == -1)	  	   { Wv.getCol(node(WORD)).map(d => tanh(d)) }
					else {
						val ab = kidsActs(lKid) << kidsActs(node(RKID))
						val quad = 	(0::WORDSIZE) {
						  				row => ((ab.t * get3D(Wt, 2*WORDSIZE, row)) * ab.t).sum 
						  			}
						(W * (ab << 1.0) + quad).map(d => tanh(d))
					}	
		val out = softmaxVect(Wc * (act << 1.0))
		(act, out)
	}

	// -------------------------------------------------------------------------------------//
	//								Forward Propagation of Tree						  		//
	// -------------------------------------------------------------------------------------//
	def forwardPropTree(
		tree: Rep[DenseVector[DenseMatrix[Int]]],
		Wc:   Rep[DenseMatrix[Double]],			// NUMCLASSES X (WORDSIZE + 1)
		W:	  Rep[DenseMatrix[Double]],			// WORDSIZE X (WORDSIZE * 2 + 1)
		Wt:   Rep[DenseMatrix[Double]],			// WORDSIZE X [(WORDSIZE * 2) X (WORDSIZE * 2)]
		Wv:	  Rep[DenseMatrix[Double]] 			// WORDSIZE X allPhrases.length
	) = {
		val levels   = tree.length
		val maxLevel = levels - 1
		verbosePrint("			Activating tree with " + levels + " level(s)", VERBOSE)	

		val acts = DenseVector[DenseVector[DenseVector[Double]]](levels, true)
		val outs = DenseVector[DenseVector[DenseVector[Double]]](levels, true)
		val cost = DenseVector[Double](levels, true)

		var curLevel = maxLevel
		while (curLevel >= 0) {
			verbosePrint("				Activating level " + curLevel, VERBOSE)	
			val kidsActs = if (curLevel == maxLevel) acts(curLevel) else acts(curLevel + 1)
			val level 	 = tree(curLevel)
			val numNodes = level.numRows
			
			val levelIO = (0::numNodes) { n =>
				verbosePrint("					Activating node " + n, VERBOSE)	
				val node = level(n)
				val (act, out) = computeNode(node, kidsActs, Wc, W, Wt, Wv)
				val nodeLabel = node(LABEL)
				val cost = log(out(nodeLabel))
				pack((act, out, cost))
			}
			acts(curLevel) = levelIO.map(_._1)
			outs(curLevel) = levelIO.map(_._2)
			cost(curLevel) = levelIO.map(_._3).sum
			curLevel -= 1
		}
		(acts, outs, -cost.sum)
	}

	// -------------------------------------------------------------------------------------//
	//								Backward Propagation of Tree					  		//
	// -------------------------------------------------------------------------------------//

	def backwardPropTree (
		tree: Rep[DenseVector[DenseMatrix[Int]]],
		Wc:   Rep[DenseMatrix[Double]],					// NUMCLASSES X (WORDSIZE + 1)
		W:	  Rep[DenseMatrix[Double]],					// WORDSIZE X (WORDSIZE * 2 + 1)
		Wt:   Rep[DenseMatrix[Double]],				  	// WORDSIZE X [(WORDSIZE * 2) X (WORDSIZE * 2)]
		Wv:	  Rep[DenseMatrix[Double]], 				// WORDSIZE X allPhrases.length
		acts: Rep[DenseVector[DenseVector[DenseVector[Double]]]],
		outs: Rep[DenseVector[DenseVector[DenseVector[Double]]]]
	) = {
		verbosePrint("			Running backward propagation on tree", VERBOSE)

		val levels   = tree.length
		val maxLevel = levels - 1

		val dWc_tree = DenseMatrix[Double](NUMCLASSES, WORDSIZE + 1)
		val dW_tree  = DenseMatrix[Double](WORDSIZE, WORDSIZE*2 + 1)
		val dWt_tree = DenseMatrix[Double](WORDSIZE*WORDSIZE*2, WORDSIZE*2)
		val dWv_tree = DenseMatrix[Double](WORDSIZE, Wv.numCols)

		var curLevel = 0
		var deltas = DenseVector[DenseVector[Double]](DenseVector.zeros(WORDSIZE).t)
		while (curLevel < levels) {
			verbosePrint("				Backpropagating at level " + curLevel, VERBOSE)	
			val level  = tree(curLevel)
			val levelActs = acts(curLevel)
			val levelOuts = outs(curLevel)
			val levelAbove = min(curLevel + 1, maxLevel)	// level of children
			
			val kidsActs	  = acts(levelAbove)
			val kidsLeafs	  = tree(levelAbove).getCol(LEAFS)

			val numNodes   = level.numRows
			val numParents = if (curLevel == maxLevel) { 0 }
						     else if (curLevel == 0)   { 1 }
							 else {level.getCol(LEAFS).count(d => d > 1)}
			val numLeaves  = numNodes - numParents

			val deltas_level = DenseVector[DenseVector[Double]](tree(levelAbove).numRows, true)
			//val dWc_level = ((0::numNodes)   {n => DenseMatrix[Double]() }).mutable
			val dW_level  = DenseVector[DenseMatrix[Double]](numParents, true)
			val dWt_level = DenseVector[DenseMatrix[Double]](numParents, true)
			val dWv_level = DenseVector[DenseMatrix[Double]](numLeaves, true)

			verbosePrint("				Back-propagating the " + numNodes + " nodes at level " + curLevel, VERBOSE)
			val dWc_level = (0::numNodes) {n =>
				verbosePrint("					Backpropagating at node " + n, VERBOSE)	
				val node  	   = level(n)
				val nodeAct    = levelActs(n)		// WORDSIZE X 1
				val nodeOut    = levelOuts(n)	    // NUMCLASSES X 1
				val deltaFromParent = deltas(n)
				
				val lKid = node(LKID)
				val nodeLabel = node(LABEL) 
				// Removed neutral label weight and "node usefulness" stuff from here

				verbosePrint("					Calculating deltaDown and dWc", VERBOSE)
				val errorCats = (0::NUMCLASSES) { num => if (num == nodeLabel) { nodeOut(num) - 1 } else { nodeOut(num) } }
				val deltaDownCatOnly = Wc.t * errorCats.t
				val deltaDownAddCat  = if (lKid == -1 && WORDI) { deltaDownCatOnly * (nodeAct << 1.0) }
									   else 		   			{ deltaDownCatOnly * (nodeAct << 1.0).map(d => 1 - d*d) }

	            val deltaDownFull = deltaFromParent.t + deltaDownAddCat(0::WORDSIZE)
	            val dWc = errorCats.t.toMat * (nodeAct.t << 1.0).toMat
	            // dWc_level(n) = dWc

				if (lKid != -1) {
					val rKid = node(RKID)

					val ab  = kidsActs(lKid) << kidsActs(rKid)	// 2 * WORDSIZE X 1 (column vector)
					verbosePrint("					Calculating dWt", VERBOSE)
					val dWtbase = ab.toMat * ab.t.toMat   
					dWt_level(n) = pack3D( (0::WORDSIZE) {row => deltaDownFull(row) * dWtbase } )

					verbosePrint("					Calculating dW", VERBOSE)
					val dW = deltaDownFull.t.toMat * (ab.t << 1.0).toMat  // WORDSIZE X 1 * 1 X WORDSIZE 
					dW_level(n) = dW

					verbosePrint("					Calculating delta to children", VERBOSE)
					val linearPart = W.t * deltaDownFull.t
					val quadParts = (0::WORDSIZE) {row => 
										val Wt_row = get3D(Wt, 2*WORDSIZE, row)
										(Wt_row + Wt_row.t) * (deltaDownFull(row) * ab) 
									}
					val deltaDownBothVec = linearPart(0::2*WORDSIZE) + quadParts.sum

					// Pass on errors to each child
					deltas_level(lKid) = if (kidsLeafs(lKid) == 1 && WORDI) { deltaDownBothVec(0::WORDSIZE) }
										 else { deltaDownBothVec(0::WORDSIZE) * ab(0::WORDSIZE).map(d => 1 - d*d) }
					
					deltas_level(rKid) = if (kidsLeafs(rKid) == 1 && WORDI) { deltaDownBothVec(WORDSIZE::2*WORDSIZE) }
										 else { deltaDownBothVec(WORDSIZE::2*WORDSIZE) * ab(WORDSIZE::2*WORDSIZE).map(d => 1 - d*d) }
				}
				else {
					dWv_level(n - numParents) = (*, 0::Wv.numCols) {w => if (w == node(WORD)) deltaDownFull else DenseVector.zeros(WORDSIZE) }
				}
				(dWc)
			}
			// Sum deltas from level, add to total delta sum for each weight matrix
			dWc_tree += dWc_level.sum
			if (numParents > 0) {
				dW_tree += dW_level.sum 
				dWt_tree += dWt_level.sum
			}
			if (numLeaves > 0) {
				dWv_tree += dWv_level.sum
			}

			curLevel += 1
			deltas = deltas_level.map(d => d)	// make deltas_level immutable. there's probably a better way to do this
		}	
		verbosePrint("			Completed backward propagation of tree", VERBOSE)
		(dWc_tree, dW_tree, dWt_tree, dWv_tree)
	}

	// -------------------------------------------------------------------------------------//
	//						Tensor Cost Function (Runs Batch of Trees)				  		//
	// -------------------------------------------------------------------------------------//
	def trainBatch (
		trees: Rep[DenseVector[DenseVector[DenseMatrix[Int]]]],
		Wc:    Rep[DenseMatrix[Double]],				// NUMCLASSES X (WORDSIZE + 1)
		W:	   Rep[DenseMatrix[Double]],				// WORDSIZE X (WORDSIZE * 2 + 1)
		Wt:    Rep[DenseMatrix[Double]],				// WORDSIZE X [(WORDSIZE * 2) X (WORDSIZE * 2)]
		Wv:	   Rep[DenseMatrix[Double]] 				// WORDSIZE X allPhrases.length
	) = {
		val curBatchSize = trees.length
		val dWc_batch = DenseVector[DenseMatrix[Double]](curBatchSize, true)
		val dW_batch  = DenseVector[DenseMatrix[Double]](curBatchSize, true)
		val dWt_batch = DenseVector[DenseMatrix[Double]](curBatchSize, true)
		val dWv_batch = DenseVector[DenseMatrix[Double]](curBatchSize, true)

		tic("Batch Run")
		val costs_batch = (0::curBatchSize) { t => 
			verbosePrint("		Training on tree " + t, VERBOSE)
			tic("Current tree")
			val tree = trees(t)
			tic("Forward Propagation")
			val (acts, outs, cost) = forwardPropTree(tree, Wc, W, Wt, Wv)
			toc("Forward Propagation")
			tic("Backward Propagation")
			val (dWc, dW, dWt, dWv) = backwardPropTree(tree, Wc, W, Wt, Wv, acts, outs)
			toc("Backward Propagation")
			toc("Current tree")
			tic("Batch value update")
			verbosePrint("		Updating batch values", VERBOSE)
			dWc_batch(t) = dWc
			dW_batch(t)  = dW
			dWt_batch(t) = dWt
			dWv_batch(t) = dWv
			toc("Batch value update")
			/*val numNodes = tree.map(level => level.numRows).sum
			val actsMatrix = DenseMatrix.zeros(WORDSIZE, numNodes)
			val outsMatrix = DenseMatrix.zeros(NUMCLASSES, numNodes)
			(0::tree.length) foreach {level =>
				(0::tree(level).numRows) foreach {node =>
					val name = tree(level).apply(node, NAME)
					actsMatrix.updateCol(name, acts(level).apply(node)) 
					outsMatrix.updateCol(name, outs(level).apply(node))
				}
			}
			writeMatrix(actsMatrix.t, "/home/david/PPL/outputs/tree" + t + "_acts.txt")
			writeMatrix(outsMatrix.t, "/home/david/PPL/outputs/tree" + t + "_outs.txt")*/
			
			(cost)
		}
		verbosePrint("		Trees completed", VERBOSE)
		toc("Batch Run")

		verbosePrint("		Computing final deltas", VERBOSE)
		tic("Final computation")

		val Wshort = W.getCols(0::2*WORDSIZE)

		val numSent = curBatchSize.toDouble
		val dfWc = dWc_batch.sum * (1/numSent) + (Wc * regC_Wc)
		val dfW  = dW_batch.sum * (1/numSent) + ((Wshort <<| DenseVector.zeros(WORDSIZE)) * regC_W)
 		val dfWt = dWt_batch.sum * (1/numSent) + Wt * regC_Wt
		val dfWv = dWv_batch.sum * (1/numSent) + Wv * regC_Wv
		val cost = costs_batch.sum * (1/numSent)

		verbosePrint("		Total cost without regularization: " + cost, VERBOSE)
	
		val regularCost = cost + ( Wc.map(e => e*e).sum * regC_Wc/2 ) + 
								 ( Wshort.map(e => e*e).sum * regC_W/2 ) +
							     ( Wt.map(e => e*e).sum * 0.001/2 ) + 
							     ( Wv.map(e => e*e).sum * regC_Wv/2 ) 
		
		verbosePrint("		Total cost with regularization: " + regularCost, VERBOSE)
		//(dfWcat, W, Wt, Wv, numSent)
		verbosePrint("		Computation complete. Batch run is done.", VERBOSE)
		toc("Final computation")

		(dfWc, dfW, dfWt, dfWv, regularCost)
	}	// end of def tensorCostFunction

	def createBatch(
    	trees:     Rep[DenseVector[DenseVector[DenseMatrix[Int]]]],
    	Wv:		   Rep[DenseMatrix[Double]],
    	batchIter: Rep[Int],
    	batchSize: Rep[Int],
    	indices:   Rep[IndexVector]
    ) = {
    	val beginBatch   = batchIter*batchSize
		val endBatch     = min(beginBatch + batchSize, trees.length)
		val batchIndices = indices.slice(beginBatch, endBatch)

		verbosePrint("		Getting batch training sets", VERBOSE)
		val unmappedTrees = trees(batchIndices)

		verbosePrint("		Finding all distinct phrases in this batch...", VERBOSE)
		val batchInds = IndexVector(unmappedTrees.flatMap(tree => tree.flatMap(level => level.getCol(WORD).toDense)).distinct)

		verbosePrint("		Creating word vector for batch", VERBOSE)
		val batchWv = Wv.getCols(batchInds)

		// map phrase numbers to batchWv based on filtered word indices
		val batchMapInit = DenseVector[Int](Wv.numCols, true)
		batchMapInit.update(batchInds, 1::batchInds.length+1)
		val batchMap = batchMapInit - 1

		verbosePrint("		Mapping trees...", VERBOSE)
		val batchTrees = unmappedTrees.map( tree => 
			tree.map( level =>
				(0::level.numRows, 0::TREECOLS) { (node, col) =>
					if (col == WORD) { batchMap(level(node, WORD)) }
					else 			 { level(node, col) }
				}
			) 
		)
		(batchTrees, batchWv, batchInds, batchMap)
    }

	// -------------------------------------------------------------------------------------//
	//								Batch Training of Trees						 	 		//
	// -------------------------------------------------------------------------------------//
	def trainOnTrees(
		trees: 		Rep[DenseVector[DenseVector[DenseMatrix[Int]]]],
		Wc:   		Rep[DenseMatrix[Double]],	// NUMCLASSES X (WORDSIZE + 1)
		W:	  		Rep[DenseMatrix[Double]],	// WORDSIZE X (WORDSIZE * 2 + 1)
		Wt:   		Rep[DenseMatrix[Double]],	// WORDSIZE X [(WORDSIZE * 2) X (WORDSIZE * 2)]
		Wv:		   	Rep[DenseMatrix[Double]], 	// WORDSIZE X allPhrases.length
		ssWc:   	Rep[DenseMatrix[Double]],	// NUMCLASSES X (WORDSIZE + 1)
		ssW:	  	Rep[DenseMatrix[Double]],	// WORDSIZE X (WORDSIZE * 2 + 1)
		ssWt:   	Rep[DenseMatrix[Double]],	// WORDSIZE X [(WORDSIZE * 2) X (WORDSIZE * 2)]
		ssWv:		Rep[DenseMatrix[Double]], 	// WORDSIZE X allPhrases.length
		batchSize: 	Rep[Int]
	) {
		val numTrees   = trees.length
		val numBatches = ceil(numTrees.toDouble/batchSize.toDouble)
		println("Running " + numTrees + " trees in " + numBatches + " batches")

		// Randomize train set order - necessary for good performance w/ SGD
		verbosePrint("	Creating random training sequence...", VERBOSE)
		val randomTrain = randperm(numTrees, numTrees)

		var batchIter = 0
		while (batchIter < numBatches) {
			//println("	Starting batch " + (batchIter + 1) + "/" + numBatches)
			// Randomized batches from the dataset
			val (batchTrees, batchWv, batchInds, batchMap) = createBatch(trees, Wv, batchIter, batchSize, randomTrain)

			verbosePrint("		Running trees...", VERBOSE)
			val (dfWc, dfW, dfWt, dfBatchWv, cost) = trainBatch(batchTrees, Wc, W, Wt, batchWv)

			verbosePrint("		[TRAINING] Calculating squares...", VERBOSE)
			val dfWcSq      = dfWc.map(e => e*e)
			val dfWSq       = dfW.map(e => e*e)
			val dfWtSq      = dfWt.map(e => e*e)
			val dfBatchWvSq = dfBatchWv.map(e => e*e)

			verbosePrint("		[TRAINING] Updating sums of squares...", VERBOSE)
			if (ADAZERO && batchIter == 0) {
				setMatrix(ssWc, dfWcSq)
				setMatrix(ssW, dfWSq)
				setMatrix(ssWt, dfWtSq)
				(0::Wv.numCols) foreach { word =>
					val batchIndex = batchMap(word)
					if (batchIndex == -1) {ssWv.updateCol(word, DenseVector.zeros(Wv.numRows))}
					else				  {ssWv.updateCol(word, dfBatchWvSq.getCol(batchIndex).toDense)}
				}
			}
			else {
				ssWc += dfWcSq
				ssW  += dfWSq
				ssWt += dfWtSq
				(batchInds) foreach { word =>
					ssWv.updateCol(word, ssWv.getCol(word) + dfBatchWvSq.getCol(batchMap(word)))
				}
			}

			verbosePrint("		[TRAINING] Calculating deltas for weights...", VERBOSE)
			val dWc 	 = (dfWc * LR) / ssWc.map(e => sqrt(e) + ADAEPS)
			val dW    	 = (dfW  * LR) / ssW.map(e => sqrt(e) + ADAEPS)
			val dWt  	 = (dfWt * LR) / ssWt.map(e => sqrt(e) + ADAEPS)
			val dBatchWv = (dfBatchWv * LR) / ssWv(*, batchInds).map(e => sqrt(e) + ADAEPS)

			verbosePrint("		[TRAINING] Calculating new weights...", VERBOSE)
			Wc -= dWc
			W  -= dW
			Wt -= dWt
			val batchWv_new = batchWv - dBatchWv

			verbosePrint("		[TRAINING] Calculating new Wv...", VERBOSE)
			(batchInds) foreach { word =>
				Wv.updateCol(word, batchWv_new.getCol(batchMap(word)))
			}

			batchIter += 1
		}	// end of while loop
	}

	// -------------------------------------------------------------------------------------//
	//									Activation of Tree							  		//
	// -------------------------------------------------------------------------------------//
	def activateTree(
		tree: Rep[DenseVector[DenseMatrix[Int]]],
		Wc:   Rep[DenseMatrix[Double]],			// NUMCLASSES X (WORDSIZE + 1)
		W:	  Rep[DenseMatrix[Double]],			// WORDSIZE X (WORDSIZE * 2 + 1)
		Wt:   Rep[DenseMatrix[Double]],			// WORDSIZE X [(WORDSIZE * 2) X (WORDSIZE * 2)]
		Wv:	  Rep[DenseMatrix[Double]] 			// WORDSIZE X allPhrases.length
	) = {
		val levels   = tree.length
		val maxLevel = levels - 1
		
		val outs = DenseVector[DenseVector[DenseVector[Double]]](levels, true)

		var curLevel = maxLevel
		var kidsActs = DenseVector[DenseVector[Double]]()
		while (curLevel >= 0) {
			val level 	 = tree(curLevel)
			val numNodes = level.numRows
			
			val levelIO = (0::numNodes) { n =>
				pack(computeNode(level(n), kidsActs, Wc, W, Wt, Wv))
			} 
			outs(curLevel) = levelIO.map(_._2)
			
			curLevel -= 1
			kidsActs = levelIO.map(_._1)	// return activation for next stage of loop (next level of tree)
		}
		(outs)	// return outputs
	}

	// -------------------------------------------------------------------------------------//
	//					Forward Propagate all Trees	for Accuracy Checking				  	//
	// -------------------------------------------------------------------------------------//
    def evalBatch (
		trees: Rep[DenseVector[DenseVector[DenseMatrix[Int]]]],
		Wc:    Rep[DenseMatrix[Double]],				// NUMCLASSES X (WORDSIZE + 1)
		W:	   Rep[DenseMatrix[Double]],				// WORDSIZE X (WORDSIZE * 2 + 1)
		Wt:    Rep[DenseMatrix[Double]],				// WORDSIZE X [(WORDSIZE * 2) X (WORDSIZE * 2)]
		Wv:	   Rep[DenseMatrix[Double]] 				// WORDSIZE X allPhrases.length
	) = {
		tic("Current Batch")
		val allNodesLabels = trees.map(tree => tree.flatMap(level => level.getCol(LABEL)))
		val rootsLabels = allNodesLabels.map(tree => tree(0))

		val allBinaryTotal  = allNodesLabels.map(tree => tree.count(label => label != NEUTRALLABEL)).sum
		val rootBinaryTotal = allNodesLabels.count(tree => tree(0) != NEUTRALLABEL)

		val curBatchSize = trees.length
		val treesCorrect = (0::curBatchSize) { t =>
			verbosePrint("		Activating tree " + t, VERBOSE)
			tic("Tree Activation")
			val outs = DenseVector.flatten( activateTree(trees(t), Wc, W, Wt, Wv) )
			toc("Tree Activation")
			val binaryActs = outs.map(output => output(3) + output(4) > output(0) + output(1))	// assumes NUMCLASSES = 5
			val labelsBinaryMatch  = binaryActs.zip(allNodesLabels(t)) {(l1, l2) => (l1 && l2 > NEUTRALLABEL) || (!l1 && l2 < NEUTRALLABEL)}
			val nodesBinaryCorrect = labelsBinaryMatch.count(d => d)
			val rootBinaryCorrect  = if (labelsBinaryMatch(0)) {1} else {0}
			
			pack( (nodesBinaryCorrect, rootBinaryCorrect) )
		}

		val correctNodes = treesCorrect.map(_._1).sum
		val correctRoots = treesCorrect.map(_._2).sum
		val percentRoots = correctRoots.toDouble / rootBinaryTotal
		val percentAll   = correctNodes.toDouble / allBinaryTotal

		toc("Current Batch")
		verbosePrint("--- Batch Results ---", VERBOSE)
		verbosePrint("Root binary accuracy over all trees in batch: " + percentRoots + "  ( " + correctRoots + "/" + rootBinaryTotal + " )", VERBOSE)
		verbosePrint("Node binary accuracy over all trees in batch: " + percentAll + "  (" + correctNodes + "/" + allBinaryTotal + " )", VERBOSE)
		pack( (correctNodes, correctRoots, allBinaryTotal, rootBinaryTotal) )
	}

	def evalOnTrees(
		trees: Rep[DenseVector[DenseVector[DenseMatrix[Int]]]],
		Wc:    Rep[DenseMatrix[Double]],				// NUMCLASSES X (WORDSIZE + 1)
		W:	   Rep[DenseMatrix[Double]],				// WORDSIZE X (WORDSIZE * 2 + 1)
		Wt:    Rep[DenseMatrix[Double]],				// WORDSIZE X [(WORDSIZE * 2) X (WORDSIZE * 2)]
		Wv:	   Rep[DenseMatrix[Double]],				// WORDSIZE X allPhrases.length
		batchSize: 	Rep[Int]
	) {
		val numTrees   = trees.length
		val numBatches = ceil(numTrees.toDouble/batchSize.toDouble)
		println(numTrees + " trees to be run in " + numBatches + " batches")

		// Randomize train set order - necessary for good performance w/ SGD
		val randomTrain = randperm(numTrees, numTrees)

		println("	Running batches...")
		tic("All Batches")
		val results = (0::numBatches) { batchIter =>
			//println("	Starting batch " + (batchIter + 1) + "/" + numBatches)
			// Randomized batches from the dataset
			val (batchTrees, batchWv, batchInds, batchMap) = createBatch(trees, Wv, batchIter, batchSize, randomTrain)

			verbosePrint("		Running trees...", VERBOSE)
			evalBatch(batchTrees, Wc, W, Wt, batchWv)
		}
		val correctNodes = results.map(_._1).sum
		val correctRoots = results.map(_._2).sum
		val numNodes     = results.map(_._3).sum
		val numRoots     = results.map(_._4).sum
		val percentRoots = correctRoots.toDouble / numRoots
		val percentAll   = correctNodes.toDouble / numNodes

		toc("All Batches")
		println("-----------------Evaluation complete-----------------")
		println("Root binary accuracy over all trees: " + percentRoots + "  ( " + correctRoots + "/" + numRoots + " )")
		println("Node binary accuracy over all trees: " + percentAll + "  ( " + correctNodes + "/" + numNodes + " )")
		println("-----------------------------------------------------")
	}

}
