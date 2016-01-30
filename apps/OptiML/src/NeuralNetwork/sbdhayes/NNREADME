Author: Saunders Hayes (sbdhayes@cs.stanford.edu) - feel free to email me for questions or bugs

Pervasive Parallelism Laboratory (PPL)
Stanford University
2013

This neural network classifier implements a classic feedforward neural network using a logistic
activation function and backpropagation. There are three layers in this network, the input layer,
the hidden layer, and the output layer.


You can train using mini-batch or stochastic gradient descent. Below is the API for the neural network: 

//Trains the network and returns a tuple of Dense Matrix weights: (inputToHiddenWeights, hiddenToOutputWeights)
trainMBatch(trainingSet: Rep[DenseMatrix[Double]], labels: Rep[DenseMatrix[Double]], genSet: Rep[DenseMatrix[Double]], genSetLabels: Rep[DenseMatrix[Double]], numIterations: Rep[Int], learningRate: Rep[Double], batchSize: Rep[Int], numHidNeurons: Rep[Int]) 

//Trains the network and returns a tuple of Dense Matrix weights: (inputToHiddenWeights, hiddenToOutputWeights)
trainStochastic(trainingSet: Rep[DenseMatrix[Double]], labels: Rep[DenseMatrix[Double]], genSet: Rep[DenseMatrix[Double]], genSetLabels: Rep[DenseMatrix[Double]], numIterations: Rep[Int], learningRate: Rep[Double], numHidNeurons: Rep[Int])

//Saves weights to files (this is what you need to save a network to files - supports the notion of training once and running many times)
save(inputToHiddenWeights: Rep[DenseMatrix[Double]], hiddenToOutputWeights: Rep[DenseMatrix[Double]], inputToHiddenWeightsFilename: Rep[String], hiddenToOutputWeightsFilename: Rep[String])

//Runs a network from files - the data set here is the unlabeled test data. The predicted labels will be written to the outputFilename. 
run(inputToHiddenWeightsFilename: Rep[String], hiddenToOutputWeightsFilename: Rep[String], dataSetFilename: Rep[String], outputFilename: Rep[String])



Example command line usage: bin/delite NNCompiler inputFiles/LRTrainingSet inputFiles/LRLabelsBin inputFiles/LRGeneralizationSet inputFiles/LRGeneralizationLabelsBin inputFiles/LRGeneralizationSet outputFiles/results2 50 0.006 300 b

Example command line usage: bin/delite NNCompiler inputFiles/LRTrainingSet inputFiles/LRLabelsBin inputFiles/LRGeneralizationSet inputFiles/LRGeneralizationLabelsBin inputFiles/LRGeneralizationSet outputFiles/results2 50 0.006 300 s



Training set, test set, or generalization set file format:

Each row of the file is a new data point with features as columns. Each feature
is separated by whitespace. Every data point must have the same number of features.
The training set, test set, and generalization set must have data points with the
same number of features. It is recommended that you use 60%, 20%, 20% respectively
for the training set, generalization set, and test set and that you randomize the
data before training the network (especially if it is in labeled order).


Labels file format:

A label file must match the number of rows to its corresponding data file (each
row in the label file corresponds to a row in the training/generalization set).
The labels must be consistent in how many outputs are expected.


Labels for the neural network:

The neural network can be trained to expect any number of label outputs.
For example if you had 8 different labels classes they could be viewed as the
following labels for the network

0.0000 0.0000 0.0000 - class1
1.0000 0.0000 0.0000 - class2
0.0000 1.0000 0.0000 - class3
0.0000 0.0000 1.0000 - class4
1.0000 1.0000 0.0000 - class5
0.0000 1.0000 1.0000 - class6
1.0000 0.0000 1.0000 - class7
1.0000 1.0000 1.0000 - class8

The network in this case only needs three outputs to represent 8 classes, however
you could use more outputs to represent the 8 classes as this may improve performance.