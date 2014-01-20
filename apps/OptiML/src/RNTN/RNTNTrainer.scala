import optiml.compiler._
import optiml.library._
import optiml.shared._

object RNTNInterpreter extends OptiMLApplicationInterpreter with RNTNTrainer
object RNTNCompiler extends OptiMLApplicationCompiler with RNTNTrainer
trait RNTNTrainer extends OptiMLApplication with RNTNOps with Utilities {
	private val TREECOLS = 6
	private val PARENT = 0  		// Gives the index of the parent node of the node
	private val LKID   = 1  		// Gives the index of the left child of the node
	private val RKID   = 2  		// Gives the index of the right child of the node
	private val WORD   = 3       	// Gives the phrase index represented by the node
	private val LABEL  = 4       	// Gives the predetermined sentiment score for the node
	private val LEAFS  = 5       	// Gives the number of leaves the node is an ancestor of

	private val POSNEGNEUT = 5

	//private val QRNN  		  = true
	private val NUMCLASSES 	  = 5
	private val WORDSIZE      = 25

	private val delim = ", "

	private def readPhrases(filename: Rep[String]): Rep[DenseVector[DenseVector[String]]] = {
		val a = ForgeFileReader.readLines(filename){ line =>
        	val tokens = line.trim.fsplit(" ")
        	(0::array_length(tokens)) { d => tokens(d)}
      	}
      	(0::array_length(a)) {d => a(d)}
	}

	private def readIndices(filename: Rep[String]): Rep[IndexVector] = {
		readMatrix(filename, (e => e.toDouble), " ").getRow(0).find(e => e > 0)
	}

	// Read condensed matrix of trees and pick out individual trees using the fact that a node has
	// a parent entry of -1 iff it is a root of a tree
	private def readTrees( filename: Rep[String] ): Rep[DenseVector[DenseVector[DenseMatrix[Int]]]] = {
		println("Loading trees...")
		val rawTrees = readMatrix(filename + "_trees.csv");
		println("Done.")
		
		//if (fullTreesRaw.numCols != TREECOLS) {
			// Assumes that data is saved such that parents are always below their children
			val roots    = rawTrees.getCol(PARENT).t.find(d => d == 0)
			val numTrees = roots.length
			println("Trees are unmodified. Pre-processing " + numTrees + " trees...")
			val treeFirstLeaves = (0::1).toDense << (roots + 1)

			(0::numTrees) { treeNum =>
				val curTree  = rawTrees.getRows(treeFirstLeaves(treeNum)::treeFirstLeaves(treeNum + 1))
				val numNodes = curTree.numRows
				val numLeafs = (numNodes + 1)/2

				val family = curTree.getCols(PARENT::LABEL).map(d => d.toInt - 1)
				val scores = curTree.getCol(LABEL)
				val levels = DenseVector[Int](numNodes, true)
				val leafs  = DenseVector[Int](numNodes, true)
				
				// Calculate level for each node
				var curNode = numNodes - 2
				while (curNode >= 0) {
					val parent = family(curNode, PARENT)
					val parentLevel = if (parent <= curNode || parent >= numNodes || parent < 0) {
	 					family.pprint
	 					errorOut("Error: There was a problem with the tree data: invalid parent given at node #" + curNode + " (requested node# " + parent + ")")
					}
					else { levels(parent) }
					levels(curNode) = parentLevel + 1
					curNode -= 1
				}
				
				// Calculate the number of leaves each node is an ancestor of (leaves are their own ancestors)
				curNode = 0
				while (curNode < numNodes) {
					leafs(curNode) = if (curNode < numLeafs) 1
								 	 else 					 leafs(family(curNode, LKID)) + leafs(family(curNode, RKID))
					curNode += 1
				}
				
				val numLevels = levels.max + 1
				val nodesAtLevels = (0::numLevels) {curLevel => 
					val nodesAtLevel = levels.find(l => l == curLevel) 
					val leafsAtLevel = leafs(nodesAtLevel)
					// within level, nodes are sorted from most to least leaf ancestors (leaves come last)
					val (leafsSorted, indices) = (-leafsAtLevel).sortWithIndex
					IndexVector(nodesAtLevel.toDense.apply(indices))
				}
				
				// Form tree levels, output tree
				(0::numLevels) { curLevel =>
					val parents  = if (curLevel == 0) { nodesAtLevels(0) } else { nodesAtLevels(curLevel - 1) }
					val children = if (curLevel == numLevels - 1) {nodesAtLevels(0)} else {nodesAtLevels(curLevel + 1)} 
					(nodesAtLevels(curLevel), *) {node =>
						(0::TREECOLS) {col =>
							if (col == PARENT) {
								val origParent = family(node, PARENT)
								if (origParent != -1) {parents.find(d => d == origParent).first } else { -1 }
							} 
							else if (col == LKID || col == RKID) { 
								val origChild = family(node, col)
								if (origChild != -1) { children.find(d => d == origChild).first } else { -1 }	
							}
							else if (col == WORD)   { family(node, WORD) }
							else if (col == LEAFS)  { leafs(node) }
							else { // (col == LABEL) 
								val score = scores(node)
								if (NUMCLASSES <= 3) {
									if 	  	( score <= 0.4 ) 	{ 0 } 
						 		  	else if ( score >= 0.6 ) 	{ NUMCLASSES - 1 }
								  	else if ( NUMCLASSES == 3 ) { 1 }
								  	else		   			    { POSNEGNEUT } // neutral
								}
								else {
									if (score > 1) { errorOut("Error: An invalid phrase score of " + score + " exists in the tree at node" + node +".") }
									else {floor(abs(score - 0.001)*NUMCLASSES) }
								}
							}
						}
					}
				}
			}
		/*}	TODO: FIX LOADING PREVIOUSLY FORMATTED TREES
		else {
			val roots    = fullTreesRaw.getCol(PARENT).t.find(d => d < 0)
			val numTrees = roots.length
			roots <<= fullTreesRaw.numRows
			(0::numTrees) { treeNum => 
				fullTreesRaw.getRows(roots(treeNum)::roots(treeNum + 1)).map(_.toInt)
			}
		}*/
	}

	def main() = {
		tic("Entire Program")
		// -------------------------------------------------------------------------------------//
		//							Init Parameters (params_sentComp)				  			//
		// -------------------------------------------------------------------------------------//
		val FILEPATH = "/home/david/PPL/data/rotten"
		val OUTPUT	 = "/home/david/PPL/outputs/"
		val DATASET = "/home/david/PPL/data/rotten"//if (args.length < 2) FILEPATH else args(0)

		val runsThroughData = 20
		val batchSize 		= 20
		val initI 			= false

		// -------------------------------------------------------------------------------------//
		//								Load Data (preProDataset.m)					  			//
		// -------------------------------------------------------------------------------------//
		println("Loading data...")
		tic("Transform trees")
		// Vector of matrices. Each matrix represent a full tree. Trees have numbered nodes
		val allTrees = readTrees(DATASET)			
		toc("Transform trees")
		// Long 1D vector containing phrases (strings) represented by all nodes in all trees
		//val allPhrases = DenseVector.ones(30) //readVector(dataset + "_words.dat")
		
		// Vector of vector of strings representing parsed sentences
		val parsedPhrases = readPhrases(DATASET + "_words.csv")

		val trainIndices = readIndices(DATASET + "_trainIndices.csv")
		val testIndices  = readIndices(DATASET + "_testIndices.csv")
		val devIndices   = readIndices(DATASET + "_devIndices.csv")

		val numWords = allTrees.map(tree => tree.map(l => l.getCol(WORD).max).max).max + 1  	// allPhrases.length
		val numTrees = allTrees.length

		//val trainPhrases = phrases(trainIndices)
		val trainTrees = allTrees(trainIndices)
		//val devPhrases   = phrases(devIndices)
		val devTrees   = allTrees(devIndices)
		//val testPhrases  = phrases(testIndices)
		val testTrees  = allTrees(testIndices)

		println("Data loaded. " + numTrees + " phrase trees with " + numWords + " total words")
 
		// -------------------------------------------------------------------------------------//
		//								Init Weights (intParams.m)						  		//
		// -------------------------------------------------------------------------------------//
		println("Initializing weights...")
		
		val fanIn   = WORDSIZE.toDouble
		val range   = 1/sqrt(fanIn)
		val rangeW  = 1/sqrt(2*fanIn)
		val rangeWt = 1/sqrt(4*fanIn)
		val sizeWt  = WORDSIZE*2

		// NUMCLASSES X (WORDSIZE + 1)
		val WcF = (0::NUMCLASSES, 0::WORDSIZE + 1) {(row, col) => 
			if 		(col < WORDSIZE - 1) 	{ random[Double] * (2*range) - range }
			else if (col == WORDSIZE - 1)	{ random[Double] }
			else 							{ 0.0 }
		}
		
		// WORDSIZE X (WORDSIZE * 2 + 1)
		val WF = (0::WORDSIZE, 0::(WORDSIZE*2 + 1)) {(row, col) => 
			if (col < WORDSIZE*2) {
				if (initI) { if (row == col || (row + WORDSIZE) == col) 0.5 else 0.0 }
				else	   { random[Double] * (2*rangeW) - rangeW }
			}
			else { 0.0 }
		}

		// WORDSIZE X (WORDSIZE * 2) X (WORDSIZE * 2)
		val WtF = DenseMatrix.randn(WORDSIZE*sizeWt, sizeWt) * (2*rangeWt) - rangeWt

		// weights associated with individual phrases
		val WvF = DenseMatrix.randn(WORDSIZE, numWords) * 0.1

		val Wc = WcF.map(d => floor(d*10000 + 0.5).toDouble/10000).mutable
		val W  = WF.map(d => floor(d*10000 + 0.5).toDouble/10000).mutable
		val Wt = WtF.map(d => floor(d*10000 + 0.5).toDouble/10000).mutable
		val Wv = WvF.map(d => floor(d*10000 + 0.5).toDouble/10000).mutable

		//writeMatrix(Wc, OUTPUT + "Wc_init.txt", delim)
		//writeMatrix(W,  OUTPUT + "W_init.txt", delim)
		//writeMatrix(Wt, OUTPUT + "Wt_init.txt", delim)
		//writeMatrix(Wv, OUTPUT + "Wv_init.txt", delim)

		val ssWc = DenseMatrix[Double](Wc.numRows, Wc.numCols)
		val ssW  = DenseMatrix[Double](W.numRows,  W.numCols)
		val ssWt = DenseMatrix[Double](Wt.numRows, Wt.numCols)
		val ssWv = DenseMatrix[Double](Wv.numRows, Wv.numCols)

		//println("Checking initial accuracy")
		//evalOnTrees(trainTrees, Wc, W, Wt, Wv, 100)

	   	var runIter = 1
	   	while (runIter <= runsThroughData) {
			println("Training run " + runIter + "/" + runsThroughData)
			trainOnTrees(trainTrees, Wc, W, Wt, Wv, ssWc, ssW, ssWt, ssWv, batchSize)
			println("Completed run " + runIter + "/" + runsThroughData)

			println("Checking accuracy for run " + runIter + "/" + runsThroughData + "...")
			evalOnTrees(trainTrees, Wc, W, Wt, Wv, 100)
			println("Completed Accuracy check for run " + runIter + "/" + runsThroughData + "...")

			runIter += 1
		}		

		println("Running test set evaluation...")
		evalOnTrees(testTrees, Wc, W, Wt, Wv, 100)

		println("Writing out results...")
		writeMatrix(Wc, OUTPUT + "Wc_final.txt", delim)
		writeMatrix(W,  OUTPUT + "W_final.txt", delim)
		writeMatrix(Wt, OUTPUT + "Wt_final.txt", delim)
		writeMatrix(Wv, OUTPUT + "Wv_final.txt", delim)

		println("All done!")
		toc("Entire Program")
	}	// end of main

}
