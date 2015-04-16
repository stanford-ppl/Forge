/* 
*
* Author: Saunders Hayes (sbdhayes@cs.stanford.edu) - actively maintaining this code. Email me for suggestions and clarifications. 
*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*/

/*
* The Neural Network Runner is used to run an already trained nerual network. The main method encapsulates a fully functional 
* training, saving, and running of a network, however it may make sense to instead override this functionality in a child trait
* and use the API defined by NeuralNetworkRunner, NeuralNetworkTrainerBatch, and NeuralNetworkTrainerStochastic. This API supports
* classic three-layer feed-forward backpropagation neural network with a logistic activation function. The network can either be
* trained using stochastic or mini-batch gradient descent. 
* 
* You can read more about the file format restrictions and the neural network API in the NNREADME file. 
*
*/


import optiml.compiler._
import optiml.library._
import optiml.shared._

object NNCompiler extends OptiMLApplicationCompiler with NeuralNetworkRunner
object NNInterpreter extends OptiMLApplicationInterpreter with NeuralNetworkRunner


trait NeuralNetworkRunner extends OptiMLApplication with NeuralNetworkTrainerStochastic with NeuralNetworkTrainerBatch with NeuralNetUtilities {

	//Initial parameters for the network. Usually never makes sense to use the default ones here, so it is recommended to also supply 
	//explicit parameters when invoking the main method.
	private val HIDDEN_NEURON_INIT = "-1"
	private val LEARNING_RATE_INIT = "0.001"
	private val NUM_ITERATIONS_INIT = "5000"
	private val BATCH_SIZE = "100"
	
	//Saves the weights to files. The encapsulating folders must already exist for this function to work. It will not create the folders for you. 
	def save(inputToHiddenWeights: Rep[DenseMatrix[Double]], hiddenToOutputWeights: Rep[DenseMatrix[Double]], inputToHiddenWeightsFilename: Rep[String], hiddenToOutputWeightsFilename: Rep[String]) = {
		writeMatrix(inputToHiddenWeights, inputToHiddenWeightsFilename)
		writeMatrix(hiddenToOutputWeights, hiddenToOutputWeightsFilename)
	}

	//Runs the network using weights that have already been saved to a file. Will most likely update this in the coming days to factor out the IO to the client
	//and require that weights matrices be supplied rather than filenames. Writes the label results from running the network to the outputFilename. 
	def run(inputToHiddenWeightsFilename: Rep[String], hiddenToOutputWeightsFilename: Rep[String], dataSetFilename: Rep[String], outputFilename: Rep[String]) = {
		val inputDataSet = readMatrix(dataSetFilename)
		val inputToHiddenWeights = readMatrix(inputToHiddenWeightsFilename)
		val hiddenToOutputWeights = readMatrix(hiddenToOutputWeightsFilename)

		val numInputNeurons = inputToHiddenWeights.numRows
		val numHiddenNeurons = hiddenToOutputWeights.numRows
		val numOutputNeurons = hiddenToOutputWeights.numCols

		var inputNeurons = (0::numInputNeurons) { index =>
			if(index == numInputNeurons - 1) {
				-1.0 //Bias neuron
			} else {
				0.0
			}
		}
		var hiddenNeurons = (0::numHiddenNeurons) { index =>
			if(index == numHiddenNeurons - 1) {
				-1.0 //Bias neuron
			} else {
				0.0
			}
		}
		var outputNeurons = DenseVector.zeros(numOutputNeurons)

		var inputToHiddenDeltas = DenseMatrix.zeros(numInputNeurons, numHiddenNeurons - 1)
		var hiddenToOutputDeltas = DenseMatrix.zeros(numHiddenNeurons, numOutputNeurons)
		var hiddenErrorGradients = DenseVector.zeros(numHiddenNeurons - 1)
		var outputErrorGradients = DenseVector.zeros(numOutputNeurons)

		def feedForward(inputValues: Rep[DenseVector[Double]]) = {
			inputNeurons = (0::inputNeurons.length) { index =>
				if(index == inputNeurons.length - 1) {
					-1.0 //Bias neuron
				} else {
					inputValues(index)
				}
			}
			hiddenNeurons = (0::hiddenNeurons.length) { hiddenIndex =>
				if(hiddenIndex == hiddenNeurons.length - 1) {
					-1.0 //Bias neuron
				} else {
					sigmoid(sum(0, inputNeurons.length) { inputIndex =>
						inputNeurons(inputIndex) * inputToHiddenWeights(inputIndex, hiddenIndex)
					})
				}
			}
			outputNeurons = (0::outputNeurons.length) { outputIndex =>
				sigmoid(sum(0, hiddenNeurons.length) { hiddenIndex =>
					hiddenNeurons(hiddenIndex) * hiddenToOutputWeights(hiddenIndex, outputIndex)
				})
			}
		}

		def runNetwork(dataSetName: Rep[String], dataSet: Rep[DenseMatrix[Double]], outputFilename: Rep[String]) = {
			println("#################################################################################")
			println("STARTING CLASSIFICATION RUN: " + dataSetName)

			val outputMatrix = DenseMatrix.zeros(0, 0).mutable
			var sampleIndex = 0
			while(sampleIndex < dataSet.numRows) {
				val inputValues = dataSet(sampleIndex)
				feedForward(inputValues)
				var guessedLabels = outputNeurons.map(value => roundValue(value))
				outputMatrix<<=(guessedLabels)
				sampleIndex+=1
			}
			writeMatrix(outputMatrix, outputFilename)
		}

		runNetwork(dataSetFilename, inputDataSet, outputFilename)
	}


	//Sample usage for stochastic: bin/delite NNCompiler inputFiles/LRTrainingSet inputFiles/LRLabelsBin inputFiles/LRGeneralizationSet inputFiles/LRGeneralizationLabelsBin inputFiles/LRGeneralizationSet outputFiles/results2 2 0.006 300 s
	//Another sample usage for mini-batch: bin/delite NNCompiler inputFiles/LRTrainingSet inputFiles/LRLabelsBin inputFiles/LRGeneralizationSet inputFiles/LRGeneralizationLabelsBin inputFiles/LRGeneralizationSet outputFiles/results2 2 0.006 300 b
	//Currently cannot customize the batch size from the command line: working on fixing this in a later update. 
	def main() = {
		if (args.length < 6 || args.length > 10) println("Error: Not invoked correctly")
		val trainArgs =
			if (args.length == 6) DenseVector(NUM_ITERATIONS_INIT, LEARNING_RATE_INIT, HIDDEN_NEURON_INIT, "s")
			else if (args.length == 7) DenseVector(args(6), LEARNING_RATE_INIT, HIDDEN_NEURON_INIT, "s")
			else if (args.length == 8) DenseVector(args(6), args(7), HIDDEN_NEURON_INIT, "s")
			else if (args.length == 9) DenseVector(args(6), args(7), args(8), "s")
			else DenseVector(args(6), args(7), args(8), args(9))



		//Set up the structure of the neural network
		val trainingSet = readMatrix(args(0)) //read the training set
		val labels = readMatrix(args(1)) //read the labels
		val genSet = readMatrix(args(2)) //read a testing set (or a generalization set)
		val genSetLabels = readMatrix(args(3)) //read the labels for that test set
		

		val outputFilename = args(5)

		def checkForFileFormatErrors() = {
			if(labels.numRows != trainingSet.numRows) {printError("Training set and labels files do not have matching number of rows. Training set rows: " + 
											trainingSet.numRows + ". Number of label rows: " + labels.numRows + ". Each training example needs a matching label and vice versa!")}
			if(genSet.numRows != genSetLabels.numRows) {printError("Generalization set and generalization set labels do not have the same number of rows. Set rows:" + 
											genSet.numRows + ". Label rows: " + genSetLabels.numRows + ". Each generalization example needs a matching label and vice versa!")}
			if(labels.numCols != genSetLabels.numCols) {printError("Error: The number of label columns in " + args(1) + " and the number of label columns in " + 
											args(3) + " do not match." )}
		}

		checkForFileFormatErrors()


		val weights = 
			if(trainArgs(3) == "s") trainStochastic(trainingSet, labels, genSet, genSetLabels, trainArgs(0).toInt, trainArgs(1).toDouble, trainArgs(2).toInt)
			else trainMBatch(trainingSet, labels, genSet, genSetLabels, trainArgs(0).toInt, trainArgs(1).toDouble, BATCH_SIZE.toInt, trainArgs(2).toInt)

		save(weights._1, weights._2, "networkFiles/NetworkWeightsiToH", "networkFiles/NetworkWeightshToO")
		run("networkFiles/NetworkWeightsiToH", "networkFiles/NetworkWeightshToO",  args(4) , outputFilename)
	}


}
