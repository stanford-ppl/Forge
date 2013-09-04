/* 
*
* Author: Saunders Hayes (sbdhayes@cs.stanford.edu)
*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*/


import optiml.compiler._
import optiml.library._
import optiml.shared._

object NNBCompiler extends OptiMLApplicationCompiler with NeuralNetworkClassifierBatch 
object NNBInterpreter extends OptiMLApplicationInterpreter with NeuralNetworkClassifierBatch 


/*
* This neural network classifier implements a classic feedforward neural network using a logistic
* activation function and backpropagation. There are three layers in this network, the input layer, 
* the hidden layer, and the output layer. Look at the README file for a description on file formatting 
* for the training set, the test set, the generalization set, and the labels.
*
* Note: Please do not train this network on a 'label sorted' training set - works better when training set is random.
* This is because the network uses stochastic gradient descent to update parameters. 
*  
* Tunable parameters:
* 		-"learningRate": defaults to 0.001
* 		-"numIterations": defaults to 5000
* 		-"numHidNeurons": defaults to the mean of the inputs (features) and outputs (number of different labels)
* 
*/


/*
* Change implementation to use point-wise arithmetic when possible
* Change implementation to use untilconverged for the loop constructs
* Use sigmoid built-in instead of helper function
* Change indentation style to two spaces rather than tabs
* 
*/


trait NeuralNetworkClassifierBatch extends OptiMLApplication {

	private val MIN_WEIGHTS_INIT = -0.01
	private val MAX_WEIGHTS_INIT = 0.01
	private val HIDDEN_NEURON_INIT = "-1"
	private val LEARNING_RATE_INIT = "0.001"
	private val NUM_ITERATIONS_INIT = "5000"
	private val BATCH_SIZE = "100"
	

	//The sigmoid function will predict values 0.5-1.0 if z > 0 and predict values 0.0-0.5 if z < 0
	//In this way (for binary classification) a large positive z returns almost 1 and a large negative z returns almost 0
	private def sigmoid(z: Rep[Double]) : Rep[Double] = {
		1.0 / (1.0 + exp(z * -1.0))
	}

	//Rounds up/down to returns a boolean result for classification rather than a decimal
	private def roundValue(value: Rep[Double]) = {
		if(value >= 0.5) {
			1.0
		} else {
			0.0
		}
	}

	//The derivative of the sigmoid function times the error
	private def outputErrorGradient(actualResult: Rep[Double], prediction: Rep[Double]) = {
		prediction * (1 - prediction) * (actualResult - prediction)
	}

	//The derivative of the sigmoid function times the sum of the output gradients (one for each output neuron) 
	//and the corresponding weight for a given hidden neuron
	//The 'prediction' here is the value of the hidden neuron.
	private def hiddenErrorGradient(prediction: Rep[Double], weightedSum: Rep[Double]) = {
		prediction * (1 - prediction) * weightedSum
	}

	//Scales a value starting at a range 0-1 to a new range lowerBound-upperBound
	private def scaleValue(lowerBound: Rep[Double], upperBound: Rep[Double], value: Rep[Double]) = {
  		((upperBound - lowerBound) * (value)) + lowerBound
  	}

	private def printError(message: Rep[String]) {
		println(message)
		exit(-1)
	}

	def train(dataFilename: Rep[String], labelsFilename: Rep[String], generalizationSetFilename: Rep[String], generalizationSetLabelsFilename: Rep[String], testSetFilename: Rep[String], outputFilename: Rep[String], numIterations: Rep[Int], learningRate: Rep[Double], batchSize: Rep[Int], numHidNeurons: Rep[Int]) = {

		val x = 0
		tic("network time", x)
		
		//Set up the structure of the neural network
		val trainingSet = readMatrix(dataFilename) //read the training set
		val labels = readMatrix(labelsFilename) //read the labels
		val testSet = readMatrix(generalizationSetFilename) //read a testing set (or a generalization set)
		val testSetLabels = readMatrix(generalizationSetLabelsFilename) //read the labels for that test set
		val secondTestSet = readMatrix(testSetFilename)
		

		def checkForFileFormatErrors() = {
			if(labels.numRows != trainingSet.numRows) {printError("Training set and labels files do not have matching number of rows. Training set rows: " + 
											trainingSet.numRows + ". Number of label rows: " + labels.numRows + ". Each training example needs a matching label and vice versa!")}
			if(testSet.numRows != testSetLabels.numRows) {printError("Generalization set and generalization set labels do not have the same number of rows. Set rows:" + 
											testSet.numRows + ". Label rows: " + testSetLabels.numRows + ". Each generalization example needs a matching label and vice versa!")}
			if(labels.numCols != testSetLabels.numCols) {printError("Error: The number of label columns in " + labelsFilename + " and the number of label columns in " + 
											generalizationSetLabelsFilename + " do not match." )}
		}

		checkForFileFormatErrors()

		val numInputNeurons = trainingSet.numCols + 1 //Add one for the bias neuron
		val numOutputNeurons = labels.numCols		
		val numHiddenNeurons = if(numHidNeurons < 2) { mean(DenseVector(numInputNeurons, numOutputNeurons)).toInt + 1} else {numHidNeurons + 1} //Add one for the bias neuron
		
		println("#################################################################################")
		println("STARTING NETWORK TRAINING")
		println("Training neural network with the following parameters: ")
		println("Learning rate: " + learningRate)
		println("Number of iterations: " + numIterations)
		println("Number of inputs: " + (numInputNeurons - 1))
		println("Number of hidden neurons: " + (numHiddenNeurons - 1))
		println("Number of outputs: " + numOutputNeurons)
		println("Batch size: " + batchSize)
		
		var inputToHiddenWeights = DenseMatrix.zeros(numInputNeurons, numHiddenNeurons - 1) 
		var hiddenToOutputWeights = DenseMatrix.zeros(numHiddenNeurons, numOutputNeurons)
		
		//Sets delta values to zero and initializes network weights to uniform random values between MIN_WEIGHTS_INIT and MAX_WEIGHTS_INIT
		def initializeWeights() = {
			inputToHiddenWeights = DenseMatrix.rand(inputToHiddenWeights.numRows, inputToHiddenWeights.numCols).map {elem => scaleValue(MIN_WEIGHTS_INIT, MAX_WEIGHTS_INIT, elem)}
			hiddenToOutputWeights = DenseMatrix.rand(hiddenToOutputWeights.numRows, hiddenToOutputWeights.numCols).map {elem => scaleValue(MIN_WEIGHTS_INIT, MAX_WEIGHTS_INIT, elem)}
		}

		//Runs 'numIterations' of training epochs (one training epoch is one time through every training example in the training set).
		//For each training example, the features are the inputs, and the labels for that example are the values we use to calculate 
		//errors during backpropagation. After calculating error values, we update the weights and move on to the next training example.
		def trainNetwork(numIterations: Rep[Int]) = {
			var trainingEpoch = 0
			while(trainingEpoch < numIterations) {
				
				println("***** Epoch " + (trainingEpoch + 1) + " *****")
				var trainingIndex = 0
				while(trainingIndex + batchSize - 1 < trainingSet.numRows) {
					val ffResults = (0::batchSize) { batchIndex =>
						val inputValues = trainingSet(trainingIndex + batchIndex)
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
						//Feeds the input values through the network by applying the sigmoid function the the weighted sum of the previous layer neurons
						//The weighted sum is a sum of the neuron value (in the previous layer) times its respective weight to a given neuron in the next layer 
						//for all neurons in the previous layer
						def feedForward(inputValues: Rep[DenseVector[Double]]) = {
							//println("Running feed forward with the following weights: ")
							//println("Input to hidden weights: ") 
							//inputToHiddenWeights.pprint
							//println("Hidden to output weights: ") 
							//hiddenToOutputWeights.pprint

					
							inputNeurons = (0::inputNeurons.length) { index =>
								if(index == inputNeurons.length - 1) {
									-1.0 //Bias neuron
								} else {
									inputValues(index)
								}
							}
							//println("Inputs: ")
							//inputNeurons.pprint
							hiddenNeurons = (0::hiddenNeurons.length) { hiddenIndex =>
								if(hiddenIndex == hiddenNeurons.length - 1) {
									-1.0 //Bias neuron
								} else {
									sigmoid(sum(0, inputNeurons.length) { inputIndex =>
										inputNeurons(inputIndex) * inputToHiddenWeights(inputIndex, hiddenIndex)
									})
								}
							}
							//println("Hidden: ")
							//hiddenNeurons.pprint
							outputNeurons = (0::outputNeurons.length) { outputIndex =>
								sigmoid(sum(0, hiddenNeurons.length) { hiddenIndex =>
									hiddenNeurons(hiddenIndex) * hiddenToOutputWeights(hiddenIndex, outputIndex)
								})
							}
							
							//println("Outputs: ")
							//outputNeurons.pprint
						}
						feedForward(inputValues)
						(readVar(hiddenNeurons), readVar(outputNeurons), readVar(inputNeurons))
					}

					val bpResults = (0::batchSize) { batchIndex =>
						val inputNeurons = ffResults(batchIndex)._3
						val hiddenNeurons = ffResults(batchIndex)._1
						val outputNeurons = ffResults(batchIndex)._2
						var inputToHiddenDeltas = DenseMatrix.zeros(numInputNeurons, numHiddenNeurons - 1)
						var hiddenToOutputDeltas = DenseMatrix.zeros(numHiddenNeurons, numOutputNeurons)
						inputToHiddenDeltas = DenseMatrix.zeros(inputToHiddenDeltas.numRows, inputToHiddenDeltas.numCols)
						hiddenToOutputDeltas = DenseMatrix.zeros(hiddenToOutputDeltas.numRows, hiddenToOutputDeltas.numCols)
						var hiddenErrorGradients = DenseVector.zeros(numHiddenNeurons - 1)
						var outputErrorGradients = DenseVector.zeros(numOutputNeurons)
						val labelValues = labels(trainingIndex + batchIndex)
						//Backpropagation returns errors from the output back to the input giving update information to the weights along the network.
						//Error gradients are calculated using the derivative of the sigmoid function which are then fed backwards to calculate reasonable 
						//delta values (learning rate * neuron value * gradient). These delta values are used to update the weights after backpropagation. 
						def backPropagate(labelValues: Rep[DenseVector[Double]]) = {
							//println("Running backpropagation with the following label values: ")
							//labelValues.pprint

							
							outputErrorGradients = (0::numOutputNeurons) { outputIndex =>
								outputErrorGradient(labelValues(outputIndex), outputNeurons(outputIndex))
							}

							//println("Output error: ")
							//outputErrorGradients.pprint

							hiddenToOutputDeltas = (0::numHiddenNeurons, 0::numOutputNeurons) { (i, j) =>
								learningRate * hiddenNeurons(i) * outputErrorGradients(j)
							}

							//println("Hidden to output deltas: ")
							//hiddenToOutputDeltas.pprint

							hiddenErrorGradients = (0::(numHiddenNeurons - 1)) { hiddenIndex =>
								hiddenErrorGradient(hiddenNeurons(hiddenIndex), sum(0, numOutputNeurons) { outputIndex =>
										hiddenToOutputWeights(hiddenIndex, outputIndex) * outputErrorGradients(outputIndex)
									}
								)
							}

							//println("Hidden error: ")
							//hiddenErrorGradients.pprint

							inputToHiddenDeltas = (0::numInputNeurons, 0::(numHiddenNeurons - 1)) { (i, j) =>
								learningRate * inputNeurons(i) * hiddenErrorGradients(j)
							}
							
							//println("Input to hidden deltas: ")
							//inputToHiddenDeltas.pprint

						}
						backPropagate(labelValues)
						(hiddenToOutputDeltas, inputToHiddenDeltas)

					}

					inputToHiddenWeights = inputToHiddenWeights + sum(0, batchSize) { batchIndex =>
						val inputToHiddenDeltas = bpResults(batchIndex)._2
						inputToHiddenDeltas
					}

					hiddenToOutputWeights = hiddenToOutputWeights + sum(0, batchSize) { batchIndex =>
						val hiddenToOutputDeltas = bpResults(batchIndex)._1
						hiddenToOutputDeltas
					}

					trainingIndex+=batchSize
				}

				trainingEpoch+=1
				
			}
		}

		//Tests the current weights of the network against new data that has labels. Basically this just feeds the 
		//inputs (features) of the test set (or rather the generalization set) through the network and 
		//calculates prediction accuracy. This is necessary to make sure that the network can generalize to new data 
		//that it was not trained on. 
		def testNetwork(dataSetName: Rep[String], dataSet: Rep[DenseMatrix[Double]], labels: Rep[DenseMatrix[Double]]) = {
			println("#################################################################################")
			println("STARTING TEST RUN: " + dataSetName)

			var countCorrect = 0
			val ffResults = (0::dataSet.numRows) { sampleIndex =>
				val inputValues = dataSet(sampleIndex)
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
				//Feeds the input values through the network by applying the sigmoid function the the weighted sum of the previous layer neurons
				//The weighted sum is a sum of the neuron value (in the previous layer) times its respective weight to a given neuron in the next layer 
				//for all neurons in the previous layer
				def feedForward(inputValues: Rep[DenseVector[Double]]) = {
					//println("Running feed forward with the following weights: ")
					//println("Input to hidden weights: ") 
					//inputToHiddenWeights.pprint
					//println("Hidden to output weights: ") 
					//hiddenToOutputWeights.pprint

			
					inputNeurons = (0::inputNeurons.length) { index =>
						if(index == inputNeurons.length - 1) {
							-1.0 //Bias neuron
						} else {
							inputValues(index)
						}
					}
					//println("Inputs: ")
					//inputNeurons.pprint
					hiddenNeurons = (0::hiddenNeurons.length) { hiddenIndex =>
						if(hiddenIndex == hiddenNeurons.length - 1) {
							-1.0 //Bias neuron
						} else {
							sigmoid(sum(0, inputNeurons.length) { inputIndex =>
								inputNeurons(inputIndex) * inputToHiddenWeights(inputIndex, hiddenIndex)
							})
						}
					}
					//println("Hidden: ")
					//hiddenNeurons.pprint
					outputNeurons = (0::outputNeurons.length) { outputIndex =>
						sigmoid(sum(0, hiddenNeurons.length) { hiddenIndex =>
							hiddenNeurons(hiddenIndex) * hiddenToOutputWeights(hiddenIndex, outputIndex)
						})
					}
					
					//println("Outputs: ")
					//outputNeurons.pprint
				}
				feedForward(inputValues)
				val labelValues = labels(sampleIndex)
				//println("Input was: ")
				//inputValues.pprint
				//println("Output was: ")
				//outputNeurons.pprint
				//println("Expecting output: ")
				//labelValues.pprint
				//println("Rounded output: ")
				var guessedLabels = outputNeurons.map(value => roundValue(value))
				//guessedLabels.pprint
				if(vectorEquals(labelValues, guessedLabels)) {
					countCorrect +=1
				}
				(readVar(hiddenNeurons), readVar(outputNeurons), readVar(inputNeurons))
			}
			println("Network accuracy: " + (countCorrect/(dataSet.numRows.toDouble) * 100.0) + "%"); 
		}

		/*def runNetwork(dataSetName: Rep[String], dataSet: Rep[DenseMatrix[Double]], outputFilename: Rep[String]) = {
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
		}*/

		initializeWeights()
		trainNetwork(numIterations)
		testNetwork(dataFilename, trainingSet, labels) 
		testNetwork(generalizationSetFilename, testSet, testSetLabels)
		//runNetwork(testSetFilename, secondTestSet, outputFilename)
 
 		val y = 0
		toc("network time", y)
		DenseVector.zeros(0)
	}

	//Helper function to test whether the rounded predicted values match the label values
	private def vectorEquals(vec1: Rep[DenseVector[Double]], vec2: Rep[DenseVector[Double]]) = {
		if(vec1.length != vec2.length) {
			false
		} else {
			var index = 0
			var isEqual = true
			while(index < vec2.length) {
				if(vec1(index) != vec2(index)) {
					isEqual = false
				}
				index+=1
			}
			isEqual
		}
	}

	def print_usage = {
		println("Usage: NNBCompiler <training set filename> <labels filename> <generalization set filename> <generalization set labels> <test set filename> <test set output labels filename> <optional numIterations> <optional learningRate> <optional numHiddenNeurons>")
		exit(-1)
	}

	def main() = {
		if (args.length < 4 || args.length > 10) print_usage
		val trainArgs =
			if (args.length == 6) DenseVector(args(0), args(1), args(2), args(3), args(4), args(5), NUM_ITERATIONS_INIT, LEARNING_RATE_INIT, BATCH_SIZE, HIDDEN_NEURON_INIT)
			else if (args.length == 7) DenseVector(args(0), args(1), args(2), args(3), args(4), args(5), args(6), LEARNING_RATE_INIT, BATCH_SIZE, HIDDEN_NEURON_INIT)
			else if (args.length == 8) DenseVector(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), BATCH_SIZE, HIDDEN_NEURON_INIT)
			else if (args.length == 9) DenseVector(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), HIDDEN_NEURON_INIT)
			else DenseVector(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9))

		train(trainArgs(0), trainArgs(1), trainArgs(2), trainArgs(3), trainArgs(4), trainArgs(5), trainArgs(6).toInt, trainArgs(7).toDouble, trainArgs(8).toInt, trainArgs(9).toInt)
	}
}










