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

/*
* This neural network classifier implements a classic feedforward neural network using a logistic
* activation function and backpropagation. There are three layers in this network, the input layer, 
* the hidden layer, and the output layer. Look at the NNREADME file for a description on file formatting 
* for the training set, the test set, the generalization set, and the labels.
*
* Stochastic works better than batch with the same number of iterations in terms of accuracy, but cannot be well parallelized. 
* Multi-threading leads to overhead rather than speedup. Rather, mini-batch training can be well parallelized (parallelism 
* increases as the batch size increases, but accuracy decreases as a result). 
*/


trait NeuralNetworkTrainerBatch extends OptiMLApplication with NeuralNetUtilities {

	private val MIN_WEIGHTS_INIT = -0.01
	private val MAX_WEIGHTS_INIT = 0.01
	

	//The sigmoid function will predict values 0.5-1.0 if z > 0 and predict values 0.0-0.5 if z < 0
	//In this way (for binary classification) a large positive z returns almost 1 and a large negative z returns almost 0
	private def sigmoid(z: Rep[Double]) : Rep[Double] = {
		1.0 / (1.0 + exp(z * -1.0))
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

	//Trains the neural network using mini-batch training. 
	// Batch training is written to take advantage of OptiML parallel constructors. A batch size number of inputs are all fed forward together.
	// Next, the errors are calculated for that entire batch. Then, the errors are summed together to update the weights. 
	def trainMBatch(trainingSet: Rep[DenseMatrix[Double]], labels: Rep[DenseMatrix[Double]], genSet: Rep[DenseMatrix[Double]], genSetLabels: Rep[DenseMatrix[Double]], numIterations: Rep[Int], learningRate: Rep[Double], batchSize: Rep[Int], numHidNeurons: Rep[Int]) = {

		val x = 0
		tic("network time", x)
		

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
			inputToHiddenWeights = DenseMatrix.rand(inputToHiddenWeights.numRows, inputToHiddenWeights.numCols).map {elem => scaleValue(elem, 0.0, 1.0, MIN_WEIGHTS_INIT, MAX_WEIGHTS_INIT)}
			hiddenToOutputWeights = DenseMatrix.rand(hiddenToOutputWeights.numRows, hiddenToOutputWeights.numCols).map {elem => scaleValue(elem, 0.0, 1.0, MIN_WEIGHTS_INIT, MAX_WEIGHTS_INIT)}
		}

		//Runs 'numIterations' of training epochs (one training epoch is one time through every training example in the training set).
		//For each training example, the features are the inputs, and the labels for that example are the values we use to calculate 
		//errors during backpropagation. After calculating error values, we update the weights and moves on to the next training example.
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
						feedForward(inputValues)
						pack(readVar(hiddenNeurons), readVar(outputNeurons), readVar(inputNeurons))
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
							
							outputErrorGradients = (0::numOutputNeurons) { outputIndex =>
								outputErrorGradient(labelValues(outputIndex), outputNeurons(outputIndex))
							}

							hiddenToOutputDeltas = (0::numHiddenNeurons, 0::numOutputNeurons) { (i, j) =>
								learningRate * hiddenNeurons(i) * outputErrorGradients(j)
							}

							hiddenErrorGradients = (0::(numHiddenNeurons - 1)) { hiddenIndex =>
								hiddenErrorGradient(hiddenNeurons(hiddenIndex), sum(0, numOutputNeurons) { outputIndex =>
										hiddenToOutputWeights(hiddenIndex, outputIndex) * outputErrorGradients(outputIndex)
									}
								)
							}

							inputToHiddenDeltas = (0::numInputNeurons, 0::(numHiddenNeurons - 1)) { (i, j) =>
								learningRate * inputNeurons(i) * hiddenErrorGradients(j)
							}
							
						}
						backPropagate(labelValues)
						pack(hiddenToOutputDeltas, inputToHiddenDeltas)

					}

					//The below code updates the weights based on the delta results from backprop

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
		def testNetwork(dataSet: Rep[DenseMatrix[Double]], labels: Rep[DenseMatrix[Double]]) = {
			println("#################################################################################")
			println("STARTING TEST RUN: ")

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
				feedForward(inputValues)
				val labelValues = labels(sampleIndex)
				
				var guessedLabels = outputNeurons.map(value => roundValue(value))
				
				if(vectorEquals(labelValues, guessedLabels)) {
					countCorrect +=1
				}
				pack(readVar(hiddenNeurons), readVar(outputNeurons), readVar(inputNeurons))
			}
			println("Network accuracy: " + (countCorrect/(dataSet.numRows.toDouble) * 100.0) + "%"); 
		}

		initializeWeights()
		trainNetwork(numIterations)
		testNetwork(trainingSet, labels) 
		testNetwork(genSet, genSetLabels)
 
 		val y = 0
		toc("network time", y)
		pack(inputToHiddenWeights, hiddenToOutputWeights) //Returns the weights as a tuple to the client
	}
}










