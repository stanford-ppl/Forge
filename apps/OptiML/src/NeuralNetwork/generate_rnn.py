#!/usr/local/bin/python

# ------------------------------------------------------------------------------
#
# This script reads in an xml file describing a recurrent neural network
# and generates OptiML. See rnn_example.xml for a sample input file.
#  
#  Stanford Pervasive Parallelism Laboratory
#  http://ppl.stanford.edu/
#
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Imports
# ------------------------------------------------------------------------------
import sys
import os.path
import xml.etree.ElementTree as ET

# ------------------------------------------------------------------------------
# Utilities
# ------------------------------------------------------------------------------

def err (msg) :
	print('Error: ' + msg)
	exit(0)


def print_nonrecurrent_weight_vars_for_layer(i,l):
	# Parameters depend on the type of layer
	s = ''
	type = l.attrib['type']
	if type == 'SOFTMAX':
		# Weight and bias
		s += ('w' + str(i) + ', b' + str(i))
	elif type == 'FULLY_CONNECTED':
		# Weight and bias
		s += ('w' + str(i) + ', b' + str(i))
	else :
		err("Unsupported type in print_weight_vars")
	return s

def print_weight_vars(net):
	weights = []
	num_layers = len(net)
	for i,l in enumerate(net):
		weights.append('w' + str(i))
		weights.append('b' + str(i))
		if i != num_layers-1:
			weights.append('wR' + str(i))
	return ', '.join(weights)

def print_weight_vars_function_input(net):
	s = ''
	num_layers = len(net)
	for i,l in enumerate(net):
		s += ('''
		w''' + str(i) + ''': Rep[DenseMatrix[Double]], b''' + str(i) + ''': Rep[DenseVector[Double]]''')
		if i != num_layers-1:
			s += ''',
		wR''' + str(i) + ''': Rep[DenseMatrix[Double]], // Recurrent weight'''
	return s

def print_read_weights_from_file_for_layer(i,l,name):
	s = ''
	type = l.attrib['type']
	if type == 'SOFTMAX' or type == 'FULLY_CONNECTED':
		# Weight and bias
		s += ('''
			writeMatrix(w''' + str(i) + ''', "apps/src/NeuralNetwork/''' + name + '''/w''' + str(i) + '''.txt")
			writeVector(b''' + str(i) + ''', "apps/src/NeuralNetwork/''' + name + '''/b''' + str(i) + '''.txt")
			writeMatrix(dw''' + str(i) + ''', "apps/src/NeuralNetwork/''' + name + '''/dw''' + str(i) + '''.txt")
			writeVector(db''' + str(i) + ''', "apps/src/NeuralNetwork/''' + name + '''/db''' + str(i) + '''.txt")''')
		if i != len(net)-1:
			s += ('''
			writeMatrix(wR''' + str(i) + ''', "apps/src/NeuralNetwork/''' + name + '''/wR''' + str(i) + '''.txt")
			writeMatrix(dwR''' + str(i) + ''', "apps/src/NeuralNetwork/''' + name + '''/dwR''' + str(i) + '''.txt")''')
	else :
		err("Unsupported type in print_weight_vars")
	return s

def create_weights_file_for_layer(i,l,name,num_layers):
	type = l.attrib['type']
	if type == 'FULLY_CONNECTED' or type == 'SOFTMAX':
		filenames = ['w', 'b', 'dw', 'db']
		if i != num_layers-1:
			filenames.append('wR')
			filenames.append('dwR')
		for fname in filenames:
			# Weight and bias
			filename = name + '/' + fname + str(i) + '.txt'
			if not os.path.isfile(filename):
				f = open(filename, "w")
				f.write("0")
				f.close()
	else :
		err("Unsupported type in print_weight_vars")

def print_output_vars(net,mutable_output=False):
	s = ''
	suffix = ''
	if mutable_output:
		suffix = '_m'
	for i,l in enumerate(net):
		s += ('o' + str(i) + suffix)
		if i != num_layers-1:
			s += ', '
	return s

def generate_parameter_files(name, net):
	# Create a parameter file for each layer
	# The xml specifies the type of each layer, but
	# other parameters such as the learning rate can be adjusted
	# without having to re-generate any code
	for idx in range(len(net)):
		filename = name + '/layer_' + str(idx) + '_params.txt'
		if not os.path.exists(filename):
			param_file = open(filename, 'w')
			param_file.write('''0.0001 ; Learning Rate -- Careful! Setting too high can cause gradients to explode
0.0001 ; L2 Regularization
0.0 ; momentum -- Careful! Backprop through time with momentum can cause gradients to explode
0.1 ; initial weights
0.0 ; initial biases''')
			param_file.close()
		if idx != len(net)-1:
			filename = name + '/layer_R' + str(idx) + '_params.txt'
			if not os.path.exists(filename):
				param_file = open(filename, 'w')
				param_file.write('''0.0001 ; Learning Rate -- Careful! Setting too high can cause gradients to explode
0.0001 ; L2 Regularization
0.0 ; momentum -- Careful! Backprop through time with momentum can cause gradients to explode
0.1 ; initial weights
0.0 ; initial biases''')
				param_file.close()

	# Also create a file for global parameters
	filename = name + '/global_params.txt'
	if not os.path.exists(filename):
		param_file = open(filename, 'w')
		param_file.write('''1 ; Num Epochs
10 ; Mini-Batch size
0 ; Read model from file
1 ; Write model back to file
0 ; Read momentum from file
0 ; Print outputs
0.00001 ; Finite difference epsilon
100 ; Test mini-batch size, training set (for final error report)
1 ; Test mini-batch size, validation set (for final error report)''')
		param_file.close()

def generate_weight_files(name, net):
	for i,l in enumerate(net):
		create_weights_file_for_layer(i,l,name,len(net))


def verify_net_architecture(net):

	if not net.get('samples_per_window'):
		err('net tag needs a \"samples_per_window\" attribute')
	if not net.get('name'):
		err('net tag needs a \"name\" attribute')

	# Only valid layers
	allowed_list = ['SOFTMAX', 'FULLY_CONNECTED']
	activation_list = ['LOGISTIC', 'ReLU', 'LINEAR']
	for l in net:
		if l.attrib['type'] not in allowed_list:
			err('Layer type ' + l.attrib['type'] + ' is not supported')
		if l.attrib.get('activation'):
			activation = l.attrib['activation']
			if activation not in activation_list:
				err(activation + ' is not a valid activation. Choose from: ' + \
					', '.join(activation_list) + '. (Currently, case-sensetive)')


	found_fullyconnected = False
	for i,l in enumerate(net):
		if l.attrib['type'] == 'SOFTMAX':
			if i != len(net)-1:
				err('SOFTMAX is only supported for the final layer')
		elif l.attrib['type'] == 'FULLY_CONNECTED':
			found_fullyconnected = True
		else:
			if found_fullyconnected:
				err('Fully-connected layers can only precede other fully connected layers or softmax')

def verify_net_architecture(net):
	found_fullyconnected = False

	# Only valid layers
	allowed_list = ['SOFTMAX', 'FULLY_CONNECTED']
	for l in net:
		if l.attrib['type'] not in allowed_list:
			err('Layer type ' + l.attrib['type'] + ' is not supported')

def print_weights_for_finite_difference(net, perturbation_index, type, weight_name):
	weights = []
	for i,l in enumerate(net):
		if i == perturbation_index:
			if type == 'w':
				weights.append(weight_name)
			else:
				weights.append('w' + str(i))

			if type == 'b':
				weights.append(weight_name)
			else:
				weights.append('b' + str(i))

			if type == 'r':
				weights.append(weight_name)
			elif i != len(net)-1:
				weights.append('wR' + str(i))
		else:
			weights.append('w' + str(i))
			weights.append('b' + str(i))
			if i != len(net)-1:
				weights.append('wR' + str(i))
	return ','.join(weights)

# ------------------------------------------------------------------------------
# Writing OptiML boilerplate code
# ------------------------------------------------------------------------------

def write_optiml_header (f, fname):
	f.write('''
/* Use the following commands to run this file:
From /published/OptiML,
 
sbt compile
bin/delitec ''' + fname + '''Compiler
bin/delite ''' + fname + '''Compiler

*/

import optiml.compiler._
import optiml.library._
import optiml.shared._

object ''' + fname + '''Interpreter extends OptiMLApplicationInterpreter with ''' + fname + '''
object ''' + fname + '''Compiler extends OptiMLApplicationCompiler with ''' + fname + ''' 
trait  ''' + fname + ''' extends OptiMLApplication with NetLib { 
''')


def write_optiml_ending (f, fname):
	f.write('''
}
''')


# ------------------------------------------------------------------------------
# Writing feed-forward
# ------------------------------------------------------------------------------

def write_fw_header (f, net):
	f.write('''
	// Feed-forward pass through the network
	def feed_forward
	(
		samples_per_window: Rep[Int],
		// Data''')
	f.write('''
		X: Rep[DenseMatrix[Double]],''')
	f.write('''
		// Weights''' + print_weight_vars_function_input(net) + '''
	) = {
''')

def write_fw_layers (f,net,blas):

	blas_suffix = ''
	if blas:
		blas_suffix = '_blas'

	f.write('''
		var t = 0
		val T = X.numCols / samples_per_window''')

	for i,l in enumerate(net):
		type = l.attrib['type']
		f.write('''
		val o''' + str(i) + ' = v_of_m(T).mutable')

	f.write('''

		// TODO: The way this is written does not take advantage of task parallelism.
		// All the first matrix multiplies (from the input layer) can be done in
		// parallel. Additionally, once hidden layer L (e.g. L=2 for 2nd hidden layer)
		// at time t is computed, both layer L+1 at time t and layer L at time t+1 can
		// be computed in parallel.
		while (t < T) {

			val X_t = X.slice(0, X.numRows, t*samples_per_window, (t+1)*samples_per_window)
	''')
	for i,l in enumerate(net):
		type = l.attrib['type']
		input = 'X_t' 
		activation = l.get('activation')
		if not activation:
			activation = 'ReLU'
		else:
			activation = activation.lower()
		if i!=0:
			input = 'o' + str(i-1) + '(t)'
		recurrent_weight = ''
		if i!=len(net)-1:
			recurrent_weight = ', wR' + str(i)
		f.write('''
			// Layer ''' + str(i) + ': ' + l.attrib['name'])
		if type == 'SOFTMAX':
			f.write('''
			o''' + str(i) + '''(t) = softmax_fw''' + blas_suffix + ''' (''' + input + ', ' + print_nonrecurrent_weight_vars_for_layer(i,l) + ')')
		elif type == 'FULLY_CONNECTED':
			f.write('''
			// Note: Can also do: o(t) = { if () else () }
			if (t == 0) {
				o''' + str(i) + '''(t) = fullycon_fw''' + blas_suffix + ''' (''' + input + ', ' + 
					print_nonrecurrent_weight_vars_for_layer(i,l) + ''').map(''' + activation)
			if l.attrib.get('dropout'):
					f.write(').*(' + l.attrib['dropout'])
			f.write(''')
			} else {
				o''' + str(i) + '''(t) = fullycon_fw_RNN(''' + input + ', o' + str(i) + '(t-1), ' + 
					print_nonrecurrent_weight_vars_for_layer(i,l) + recurrent_weight + ''').map(''' + activation)
			if l.attrib.get('dropout'):
					f.write(').*(' + l.attrib['dropout'])
			f.write(''')
			}''')
	f.write('''
			t += 1
		}
		''')

def write_fw_ending (f, num_layers, net):
	f.write('''
		// Return result of each layer, including output layer
		(''' + print_output_vars(net) + ''')
	}
	''')


# ------------------------------------------------------------------------------
# Writing feed-forward for dropout
# ------------------------------------------------------------------------------

def write_fw_header_dropout (f, net):
	f.write('''
	// Feed-forward pass through the network, with dropout
	def feed_forward_dropout
	(
		samples_per_window: Rep[Int],
		// Data''')
	f.write('''
		X: Rep[DenseMatrix[Double]],''')
	f.write('''
		// Weights''' + print_weight_vars_function_input(net))
	for i,l in enumerate(net):
		if l.attrib.get('dropout'):
			f.write(''',
		layer''' + str(i) + '''_dropout: Rep[DenseVector[Double]]''')
	f.write('''
	) = {
''')

def write_fw_layers_dropout (f,net,blas):

	blas_suffix = ''
	if blas:
		blas_suffix = '_blas'

	f.write('''
		var t = 0
		val T = X.numCols / samples_per_window''')

	for i,l in enumerate(net):
		type = l.attrib['type']
		f.write('''
		val o''' + str(i) + ' = v_of_m(T).mutable')

	f.write('''

		while (t < T) {

			val X_t = X.slice(0, X.numRows, t*samples_per_window, (t+1)*samples_per_window)
	''')
	for i,l in enumerate(net):
		type = l.attrib['type']
		input = 'X_t' 
		activation = l.get('activation')
		if not activation:
			activation = 'ReLU'
		else:
			activation = activation.lower()
		if i!=0:
			input = 'o' + str(i-1) + '(t)'
		recurrent_weight = ''
		if i!=len(net)-1:
			recurrent_weight = ', wR' + str(i)
		f.write('''
			// Layer ''' + str(i) + ': ' + l.attrib['name'])
		if type == 'SOFTMAX':
			f.write('''
			o''' + str(i) + '''(t) = softmax_fw''' + blas_suffix + ''' (''' + input + ', ' + print_nonrecurrent_weight_vars_for_layer(i,l) + ')')
		elif type == 'FULLY_CONNECTED':
			dropout_suffix = '(t)'
			dropout_prefix = ''
			if l.attrib.get('dropout'):
				dropout_suffix = '_no_dropout'
				dropout_prefix = 'val '
			f.write('''
			// Note: Can also do: o(t) = { if () else () }
			if (t == 0) {
				''' + dropout_prefix + '''o''' + str(i) + dropout_suffix + ''' = fullycon_fw''' + blas_suffix + ''' (''' + input + ', ' + 
					print_nonrecurrent_weight_vars_for_layer(i,l) + ''').map(''' + activation + ''')''')
			if l.attrib.get('dropout'):
				f.write('''
				o''' + str(i) + '''(t) = (0::o''' + str(i) + '''_no_dropout.numRows, 0::o''' + str(i) + '''_no_dropout.numCols) { (r,c) => 
					o''' + str(i) + '''_no_dropout(r,c) * layer''' + str(i) + '''_dropout(c)
				}''')
			f.write('''
			} else {
				''' + dropout_prefix + '''o''' + str(i) + dropout_suffix + '''  = fullycon_fw_RNN(''' + input + ', o' + str(i) + '(t-1), ' + 
					print_nonrecurrent_weight_vars_for_layer(i,l) + recurrent_weight + ''').map(''' + activation + ''')''')
			if l.attrib.get('dropout'):
				f.write('''
				o''' + str(i) + '''(t) = (0::o''' + str(i) + '''_no_dropout.numRows, 0::o''' + str(i) + '''_no_dropout.numCols) { (r,c) => 
					o''' + str(i) + '''_no_dropout(r,c) * layer''' + str(i) + '''_dropout(c)
				}''')
			f.write('''
			}''')
	f.write('''
			t += 1
		}
		''')

# ------------------------------------------------------------------------------
# Writing training def
# ------------------------------------------------------------------------------

def write_training_header(f, net, name):

	samples_per_window = net.get('samples_per_window')

	# Declare the def and some variables
	f.write('''
	def train(X: Rep[DenseMatrix[Double]], y: Rep[DenseMatrix[Double]]) = {''')
	f.write('''

		// Initialize parameters for each layer
		val m = X.numRows	// m = # examples
		val n = X.numCols 	// n = # samples per utterance, NOT the same for each example
		val k = ''' + net[-1].attrib['num_hidden'] + ''' // k = # classes (# outputs of final layer)

		// Specific to RNNs: We also need to calculate the total # time steps
		// We assume all examples (rows of X) have been padded to contain samples for T time steps
		val samples_per_window = ''' + samples_per_window + '''
		val T = n / samples_per_window // Note: assume same # samples for all examples

		// We also need to decide whether we will have a loop over T or just replicate the
		// code. We need to store the y layer (outputs) for each timestep, for each example (32 * minib_m * T)
		// This makes sense as a matrix with #rows = minib_m, and #cols = 32*T
		// We also need to store the hidden layer outputs for the current and previous timestep
		// (But not before those 2 I think? Or do we need in backprop? Check what we need in bprop, no CTC 
		//  just assume we have dJ/dy for last t step and bprop)

		// Global settings
		val glob_params = readVector[Double]("apps/src/NeuralNetwork/''' + name +'''/global_params.txt", line => line(0).toDouble, ";" )
		val num_epochs = glob_params(0).toInt
		val minib_m = glob_params(1).toInt
		val read_model = glob_params(2).toInt
		val write_model = glob_params(3).toInt
		val read_mtm = glob_params(4).toInt
		val epsilon = glob_params(6)
	''')

	# Create parameters for each layer
	for i,l in enumerate(net):

		layer_id = 'layer' + str(i)
		f.write('''
		// Parameters for layer ''' + str(i) + ', ' + l.attrib['name'] + '''
		val ''' + layer_id + '''_params = readVector[Double]("apps/src/NeuralNetwork/''' + name +'''/layer_''' + str(i) + '''_params.txt", line => line(0).toDouble, ";" )
		val ''' + layer_id + '''_lr = /*args(0).toDouble*/''' + layer_id + '''_params(0)
		val ''' + layer_id + '''_L2reg = ''' + layer_id + '''_params(1) // L2 regularization
		val ''' + layer_id + '''_mtm = ''' + layer_id + '''_params(2) // momentum
		val ''' + layer_id + '''_init_w = ''' + layer_id + '''_params(3) // initial weights
		val ''' + layer_id + '''_init_b = ''' + layer_id + '''_params(4) // initial biases
		val ''' + layer_id + '''_num_hidden = ''' + l.attrib['num_hidden'] + '''
		''')

		if i != len(net)-1:
			layer_id = 'layerR' + str(i)
			f.write('''
		// Parameters for recurrent layer ''' + str(i) + ', ' + l.attrib['name'] + '''
		val ''' + layer_id + '''_params = readVector[Double]("apps/src/NeuralNetwork/''' + name +'''/layer_R''' + str(i) + '''_params.txt", line => line(0).toDouble, ";" )
		val ''' + layer_id + '''_lr = /*args(0).toDouble*/''' + layer_id + '''_params(0)
		val ''' + layer_id + '''_L2reg = ''' + layer_id + '''_params(1) // L2 regularization
		val ''' + layer_id + '''_mtm = ''' + layer_id + '''_params(2) // momentum
		val ''' + layer_id + '''_init_w = ''' + layer_id + '''_params(3) // initial weights
		val ''' + layer_id + '''_init_b = ''' + layer_id + '''_params(4) // initial biases
		val ''' + layer_id + '''_num_hidden = ''' + l.attrib['num_hidden'] + '''
		''')


	f.write('''
		if (read_model == 1) {
			print("Reading models from file...")
		}
	''')

	# Define the weight matrices for each layer
	# These depend on the previous layer, with the first layer's size depending on the #input columns
	cols_of_prev_layer = 'samples_per_window'
	for i,l in enumerate(net):
		
		f.write("\n")
		cols_of_this_layer = 'layer' + str(i) + '_num_hidden'

		# Create the weight matrices
		type = l.attrib['type']
		if type == 'SOFTMAX':
			f.write("\t\t// Weights for Softmax Layer\n")
			f.write("\t\t" + 'var w' + str(i) + '  = DenseMatrix.randn(' + cols_of_prev_layer + ', ' + cols_of_this_layer + ')*(layer' + str(i) + '_init_w)' + "\n")
			f.write("\t\t" + 'var b' + str(i) + '  = DenseVector.zeros(' + cols_of_this_layer + ')*(layer' + str(i) + '_init_b)' + "\n")
			f.write("\t\t" + 'var dw' + str(i) + ' = DenseMatrix.zeros(' + cols_of_prev_layer + ', ' + cols_of_this_layer + ')' + "\n")
			f.write("\t\t" + 'var db' + str(i) + ' = DenseVector.zeros(' + cols_of_this_layer + ')' + "\n")
			f.write('''
		if (read_model == 1) {
			w''' + str(i) + ''' = readMatrix[Double]("apps/src/NeuralNetwork/''' + name + '''/w''' + str(i) + '''.txt", s => s.toDouble)
			b''' + str(i) + ''' = readVector("apps/src/NeuralNetwork/''' + name + '''/b''' + str(i) + '''.txt")
		}
		if (read_mtm == 1) {
			dw''' + str(i) + ''' = readMatrix[Double]("apps/src/NeuralNetwork/''' + name + '''/dw''' + str(i) + '''.txt", s => s.toDouble)
			db''' + str(i) + ''' = readVector("apps/src/NeuralNetwork/''' + name + '''/db''' + str(i) + '''.txt")
		}
		''')
			# Set up the # rows for the next matrix
			cols_of_prev_layer = cols_of_this_layer
		elif type == 'FULLY_CONNECTED':
			f.write("\t\t// Weights for Fully-Connected Layer\n")
			f.write("\t\t" + 'var w' + str(i) + '  = DenseMatrix.randn(' + cols_of_prev_layer + ', ' + cols_of_this_layer + ')*(layer' + str(i) + '_init_w)' + "\n")
			f.write("\t\t" + 'var b' + str(i) + '  = DenseVector.ones (' + cols_of_this_layer + ')*(layer' + str(i) + '_init_b)' + "\n")
			f.write("\t\t" + 'var dw' + str(i) + ' = DenseMatrix.zeros(' + cols_of_prev_layer + ', ' + cols_of_this_layer + ')' + "\n")
			f.write("\t\t" + 'var db' + str(i) + ' = DenseVector.zeros(' + cols_of_this_layer + ')' + "\n")
			f.write('''
		if (read_model == 1) {
			w''' + str(i) + ''' = readMatrix[Double]("apps/src/NeuralNetwork/''' + name + '''/w''' + str(i) + '''.txt", s => s.toDouble)
			b''' + str(i) + ''' = readVector("apps/src/NeuralNetwork/''' + name + '''/b''' + str(i) + '''.txt")
		}
		if (read_mtm == 1) {
			dw''' + str(i) + ''' = readMatrix[Double]("apps/src/NeuralNetwork/''' + name + '''/dw''' + str(i) + '''.txt", s => s.toDouble)
			db''' + str(i) + ''' = readVector("apps/src/NeuralNetwork/''' + name + '''/db''' + str(i) + '''.txt")
		}
		''')

			if (i != len(net)-1):
				f.write("\n\t\t// Weights for Recurrent Layer\n")
				f.write("\t\t" + 'var wR' + str(i) + '  = DenseMatrix.randn(' + cols_of_this_layer + ', ' + cols_of_this_layer + ')*(layer' + str(i) + '_init_w)' + "\n")
				f.write("\t\t" + 'var dwR' + str(i) + ' = DenseMatrix.zeros(' + cols_of_this_layer + ', ' + cols_of_this_layer + ')' + "\n")
				f.write('''
		if (read_model == 1) {
			wR''' + str(i) + ''' = readMatrix[Double]("apps/src/NeuralNetwork/''' + name + '''/wR''' + str(i) + '''.txt", s => s.toDouble)
		}
		if (read_mtm == 1) {
			dwR''' + str(i) + ''' = readMatrix[Double]("apps/src/NeuralNetwork/''' + name + '''/dwR''' + str(i) + '''.txt", s => s.toDouble)
		}
		''')

			# Set up the # rows for the next matrix
			cols_of_prev_layer = cols_of_this_layer
		else:
			err("Unsupported type in write_training_header")
		

	f.write('''
		if (read_model == 1) {
			println("done")
		}
	''')

	for i,l in enumerate(net):
		if l.attrib.get('dropout'):
			f.write('''
		// Layer ''' + str(i) + ''' dropout
		val layer''' + str(i) + '''_dropout_full = ( DenseVector.rand(num_epochs * (m/minib_m) * layer''' + str(i) + '''_num_hidden) ).map( 
			e => if (e < ''' + l.attrib['dropout'] + ''') 1.0 else 0.0 
		)''')
	

def write_training_loop(f, net, use_dropout):
	num_layers = len(net)
	weight_updates = '''
					// TODO: The operations below can be computed in parallel (task-level)
					val minib_X_t = minib_X.slice(0, minib_X.numRows, t*samples_per_window, (t+1)*samples_per_window)
	'''
	blas = (net.get('blas') == '1')
	f.write('''
		var epoch = 0
		// Serial Iteration
		while(epoch < num_epochs) {

			epoch+=1
			print("\\nEpoch ")
			print(epoch)

			var minib_epoch = 0
			// Serial iteration
			while (minib_epoch < m/minib_m) {

				val start_index = minib_epoch * minib_m
				val stop_index  = (minib_epoch+1) * minib_m
				val minib_X = (start_index::stop_index, 0::X.numCols) { (r,c) => X(r,c) }
				val minib_y = (start_index::stop_index, 0::y.numCols) { (r,c) => y(r,c) }''')

	for i,l in enumerate(net):
		if l.attrib.get('dropout'):
			f.write('''
				val layer''' + str(i) + '''_dropout = layer''' + str(i) + '''_dropout_full.slice(
					(epoch-1)*(m/minib_m)*layer''' + str(i) + '''_num_hidden +
					minib_epoch*layer''' + str(i) + '''_num_hidden,
					(epoch-1)*(m/minib_m)*layer''' + str(i) + '''_num_hidden +
					minib_epoch*layer''' + str(i) + '''_num_hidden +
					layer''' + str(i) + '''_num_hidden
				) // Get a vview
		''')

	f.write('''
				minib_epoch+=1
				//print(".")

			    // FW Prop
				val (''' + print_output_vars(net,True) + ''') = ''')
	if use_dropout:
		f.write('''
					feed_forward_dropout(samples_per_window, minib_X, ''')
	else:
		f.write('''
					feed_forward(samples_per_window, minib_X, ''')
	f.write(print_weight_vars(net))
	for i,l in enumerate(net):
		if l.attrib.get('dropout'):
			f.write(', layer' + str(i) + '_dropout')
	f.write(')')
	# Create immutable copies of FW prop outputs
	f.write('''

				// Create an immutable copy of outputs''')
	for i,l in enumerate(net):
		f.write('''
				val o''' + str(i) + ' = o' + str(i) + '_m.Clone''')
	# Allocate memory for each layer but the output layer
	f.write('''

				// Allocate memory for layer derivatives
				// Note: Only need to store the last 2, not all
				// Note: Also, the size is made 1 larger than it needs to be,
				// to make iterations simpler, but should do runtime experiments to see''')
	for i,l in enumerate(net):
		if i!=len(net)-1:
			f.write('''
				val dJ_do''' + str(i) + '''IN = v_of_m(T+1).mutable
				dJ_do''' + str(i) + '''IN(T) = DenseMatrix.zeros(minib_m, w''' + str(i+1) + '''.numRows)''')
	# Create immutable copies of weights and allocate the accumulation vars
	f.write('''

				// Create an immutable copy of weights and allocate gradients for each timestep
				// TODO: Should the gradients be mutable references to immutable matrices instead?''')
	for i,l in enumerate(net):
		f.write('''
				var accumulated_dw''' + str(i) + '''  = DenseMatrix.zeros(dw''' + str(i) + '''.numRows, dw''' + str(i) + '''.numCols)
				var accumulated_db''' + str(i) + '''  = DenseVector.zeros(db''' + str(i) + '''.length)
				// TODO: Not sure if these clones are needed
				val w''' + str(i) + '''_IMM = w''' + str(i) + '''.Clone
				''')
		if i!=len(net)-1:
			f.write('''
				var accumulated_dwR''' + str(i) + ''' = DenseMatrix.zeros(dwR''' + str(i) + '''.numRows, dwR''' + str(i) + '''.numCols)
				val wR''' + str(i) + '''_IMM = wR''' + str(i) + '''.Clone
				''')
	f.write('''
			    // Back Prop

				var t = T-1
				while(t >= 0) {
	''')    
	
	# Now do back-prop wrt each other layer
	for i,l in reversed(list(enumerate(net))):

		type = l.attrib['type']

		f.write('''
				    // Layer ''' + str(i) + '''
		''')

		# Each layer has the following derivatives.
		# Each derivative is of J, but with respect to something different:
		#  * dJ/doi_OUT => derivative of J wrt output of layer i
		#  * dJ/doi_IN  => derivative of J wrt input of layer i
		#  * dJ/dWi     => derivative of J wrt weights of layer i
		#  * dJ/dbi     => derivative of J wrt biases of layer i

		# The dependencies on previous derivatives are as follows:
		#  * dJ/doi_OUT => depends on derivative of J wrt previous layer (chain rule)
		#  * dJ/doi_IN  => depends on dJ/doi_OUT
		#  * dJ/dWi     => dJ/doi_IN
		#  * dJ/dbi     => dJ/doi_IN

		# Here prev_layer means down towards the softmax, i.e. last is input and prev is ff
		prev_layer = i+1
		prev_dJ_do_IN = 'dJ_do' + str(prev_layer) + 'IN' 
		if prev_layer != len(net)-1:
			prev_dJ_do_IN += '(t)'
		dJ_doi_OUT = 'dJ_do' + str(i) + 'OUT' 
		dJ_doi_IN  = 'dJ_do' + str(i) + 'IN'
		dJ_dWi     = 'dJ_dw' + str(i)
		dJ_dbi     = 'dJ_db' + str(i)
		layer_id   = 'layer' + str(i)

	    # Write layer derivative for softmax
		if i==num_layers-1:
			if type != 'SOFTMAX':
				err("Currently, only SOFTMAX is supported for the final layer")
			f.write('''
				    // ''' + str(i) + '''a) Derivative of J wrt input of softmax layer
					val ''' + dJ_doi_IN + ''' = (0::minib_m, 0::k) { (i,j) =>
	 					val prob = (o''' + str(i) + '''(t)).apply(i,j)
						if (minib_y(i,t) == j) 1-prob
						else -1*prob
					}
			''')
	    # Write layer derivative for fully connected
		elif type == 'FULLY_CONNECTED':
			f.write('''
				    // ''' + str(i) + '''a) Derivative of J wrt output and input of layer ''' + str(i) + '''
					val ''' + dJ_doi_IN +'''_t_plus_1 = dJ_do''' + str(i) + '''IN(t+1).Clone
					''')
			if blas:
				f.write('''
				    val ''' + dJ_doi_OUT +''' = (''' + prev_dJ_do_IN + ''' * w''' + str(prev_layer) + 
				    	'''_IMM.t) + (''' + dJ_doi_IN +'''_t_plus_1 * wR''' + str(i) + '_IMM.t)')
			else:
				f.write('''
				    val ''' + dJ_doi_OUT +''' = (0::''' + prev_dJ_do_IN + '''.numRows, 0::w''' + str(prev_layer) + '''_IMM.numRows) { (r,c) =>
						val s1 = sum(0, ''' + prev_dJ_do_IN + '''.numCols) { i =>
							(''' + prev_dJ_do_IN + ''').apply(r,i) * w''' + str(prev_layer) + '''_IMM(c,i)
						}
						val s2 = sum(0, ''' + dJ_doi_IN +'''_t_plus_1.numCols) { i =>
							''' + dJ_doi_IN +'''_t_plus_1(r,i) * wR''' + str(i) + '''_IMM(c,i)
						}
						s1 + s2
					}''')

			f.write('''
				    ''' + dJ_doi_IN +'''(t) = (0::''' + dJ_doi_OUT + '''.numRows, 0::''' + dJ_doi_OUT + '''.numCols) { (r,c) => 
						''')

			# Are we using dropout?
			if l.attrib.get('dropout'):

				# Check 3: Activation unit type
				if l.attrib.get('activation') == 'LOGISTIC':
					f.write(dJ_doi_OUT + '''(r,c) * (o''' + str(i) + '''(t)).apply(r,c) * (1-(o''' + str(i) + '''(t)).apply(r,c)) * layer''' + str(i) + '''_dropout(c)''')
				elif l.attrib.get('activation') == 'LINEAR':
					f.write(dJ_doi_OUT + '''(r,c) * layer''' + str(i) + '''_dropout(c)''')
				else: # ReLU
					# Note: This is a bit faster: (1-2% or so, maybe noise)
					# 	 dJ_doiOUT(r,c) * ReLU_Deriv(oi(r,c))
					f.write('if ((o' + str(i) + '(t)).apply(r,c) > 0) (' + dJ_doi_OUT + '(r,c) * layer''' + str(i) + '''_dropout(c)) else 0.0''')
			# No dropout
			else:

				# Check 3: Activation unit type
				if l.attrib.get('activation') == 'LOGISTIC':
					f.write(dJ_doi_OUT + '''(r,c) * (o''' + str(i) + '''(t)).apply(r,c) * (1-(o''' + str(i) + '''(t)).apply(r,c))''')
				elif l.attrib.get('activation') == 'LINEAR':
					f.write(dJ_doi_OUT + '''(r,c) // TODO: Ensure this is not an allocation''')
				else: # ReLU
					# Note: This is a bit faster: (1-2% or so, maybe noise)
					# 	 dJ_doiOUT(r,c) * ReLU_Deriv(oi(r,c))
					f.write('if ((o' + str(i) + '(t)).apply(r,c) > 0) ' + dJ_doi_OUT + '(r,c) else 0.0''')
			f.write('''
					}

					val ''' + dJ_doi_IN + '''_t = ''' + dJ_doi_IN + '''(t).Clone
			''')

		if i != len(net)-1:
			dJ_doi_IN = dJ_doi_IN + '_t'

		# Get the name of the previous output layer (or the input, if this is layer 0)
		# Note: So here previous means previous in terms of layer order (i.e. L0 < L1 < L2)
		# Above, previous meant previous in terms of backprop order (L2 < L1 < L0)

		prev_layer_output = '(o' + str(i-1) + '(t))'
		if i==0:
			prev_layer_output = 'minib_X_t'

		weight_updates += '''
				    // ''' + str(i) + '''b) Derivative of J wrt layer ''' + str(i) + ''' weights

					// Derivative before L2 regularization
					'''
		if type == 'FULLY_CONNECTED' or type == 'SOFTMAX':
			if blas:
				weight_updates += '''
					val ''' + dJ_dWi + '''_no_reg = (''' + prev_layer_output + '''.t * ''' + dJ_doi_IN + ''').*(-1.0/minib_m.toDouble)'''
			else:
				weight_updates += '''
					val ''' + dJ_dWi + '''_no_reg = (0::''' + prev_layer_output + '''.numCols, 0::''' + dJ_doi_IN + '''.numCols) { (r,c) =>
						(-1.0/minib_m.toDouble)*(sum(0, ''' + prev_layer_output + '''.numRows) { i =>
							''' + prev_layer_output + '''.apply(i,r) * ''' + dJ_doi_IN + '''(i,c)
						})
					}'''
			weight_updates += '''
				    val ''' + dJ_dbi + '''_no_reg = (0::''' + dJ_doi_IN + '''.numCols) {
				    	c => (-1.0/minib_m.toDouble)*''' + dJ_doi_IN + '''.getCol(c).sum
				    }'''
		weight_updates += '''
					// Derivative with L2 regularization term, and also momentum
					accumulated_dw''' + str(i) + ''' = accumulated_dw''' + str(i) + ''' + (''' + dJ_dWi + '''_no_reg)*(-1*''' + layer_id + '''_lr)
					accumulated_db''' + str(i) + ''' = accumulated_db''' + str(i) + ''' + (''' + dJ_dbi + '''_no_reg)*(-1*''' + layer_id + '''_lr)
		'''
		if i != len(net)-1:
			weight_updates += ('''
					// Derivative before L2 regularization
					if (t != 0) {
						val dJ_dwR''' + str(i) + '''_no_reg = (0::o''' + str(i) +
							'''(t-1).numCols, 0::''' + dJ_doi_IN + 
							'''.numCols) { (r,c) =>
							(-1.0/minib_m.toDouble)*(sum(0, o''' + str(i) + '''(t-1).numRows) { i =>
								(o''' + str(i) + '''(t-1)).apply(i,r) * ''' + dJ_doi_IN + '''(i,c)
							})
						}
						// Derivative with L2 regularization term
						accumulated_dwR''' + str(i) + ''' = accumulated_dwR''' + str(i) + 
						''' + (dJ_dwR''' + str(i) + '''_no_reg)*(-1*layerR''' + str(i) + '''_lr)
				    }''')

	# The gradients have been back-propagated through each layer
	# Now, use the derivatives above to update each weight matrix
	# Note: These can all be updated in parallel
	f.write(weight_updates)

def write_training_ending(f, net, name):
	f.write('''

					t = t-1
				}  // T

				// Accumulate gradients and add momentum
				''')

	for i,l in enumerate(net):
		layer_id  = 'layer' + str(i)
		layerR_id = 'layerR' + str(i)
		f.write('dw' + str(i) + ' = accumulated_dw' + str(i) + ' + dw' + str(i) + '.*(layer' + str(i) + 
			'''_mtm) + (w''' + str(i) + '''_IMM.*(''' + layer_id + '''_L2reg))*(-1*layer''' + str(i) + '''_lr)
				''')
		f.write('db' + str(i) + ' = accumulated_db' + str(i) + ' + db' + str(i) + '.*(layer' + str(i) + '''_mtm)
				''')
		if i != len(net)-1:
			f.write('dwR' + str(i) + ' = accumulated_dwR' + str(i) + ' + dwR' + str(i) + '.*(layerR' + str(i) + 
				'''_mtm) + (wR''' + str(i) + '''_IMM.*(''' + layerR_id + '''_L2reg))*(-1*layerR''' + str(i) + '''_lr)
				''')

	f.write('''
/*
				///////////////////////////////////////////////////////////////////////////////////
				// Gradient check: 
				///////////////////////////////////////////////////////////////////////////////////
				println("")
				println("**********************")
				println("RUNNING GRADIENT CHECK")
				println("**********************")
				if (epoch == 1) {

					''')

	l2_reg_terms = ''
	for i,l in enumerate(net):
		l2_reg_terms += 'layer' + str(i) + '_L2reg'
		if i != len(net)-1:
			l2_reg_terms += ', layerR' + str(i) + '_L2reg, ' 
	for i,l in enumerate(net):
		f.write('println("Layer ' + str(i) + '''")

					print("  Cross-Layer Weights:  ")
					val w''' + str(i) + '''_finite_differences = (0::w''' + str(i) + 
						'''.numRows, 0::w''' + str(i) + '''.numCols) { (r,c) =>
						val w''' + str(i) + '''_plus = w''' + str(i) + '''.mutable
						w''' + str(i) + '''_plus(r,c) = w''' + str(i) + '''_plus(r,c) + epsilon
						val w''' + str(i) + '''_minus = w''' + str(i) + '''.mutable
						w''' + str(i) + '''_minus(r,c) = w''' + str(i) + '''_minus(r,c) - epsilon
						( J(minib_X,minib_y,''' + 
						print_weights_for_finite_difference(net, i, 'w', 'w' + str(i) + '_plus') + 
						''',''' + l2_reg_terms + ''', 1) 
							- J(minib_X,minib_y,''' + 
						print_weights_for_finite_difference(net, i, 'w', 'w' + str(i) + '_minus') + 
						''', ''' + l2_reg_terms + ''', 1) ) / (2 * epsilon)
					}
					println( (dw''' + str(i) + '''*(-1/layer''' + str(i) + '''_lr) - w''' + str(i) +
						'''_finite_differences).abs.max )
					writeMatrix(w''' + str(i) + '''_finite_differences, "apps/src/NeuralNetwork/''' + name +
					 '''/w''' + str(i) + '''_finite_differences.txt")
					writeMatrix(dw''' + str(i) + '''*(-1/layer''' + str(i) +
						'''_lr), "apps/src/NeuralNetwork/''' + name + '''/w''' + str(i) + '''_gradient.txt")

					print("  Biases:   ")
					val b''' + str(i) + '''_finite_differences = (0::b''' + str(i) + '''.length) { i =>
						val b''' + str(i) + '''_plus = b''' + str(i) + '''.mutable
						b''' + str(i) + '''_plus(i) = b''' + str(i) + '''_plus(i) + epsilon
						val b''' + str(i) + '''_minus = b''' + str(i) + '''.mutable
						b''' + str(i) + '''_minus(i) = b''' + str(i) + '''_minus(i) - epsilon
						( J(minib_X,minib_y,''' + 
						print_weights_for_finite_difference(net, i, 'b', 'b' + str(i) + '_plus') + 
						''', ''' + l2_reg_terms + ''', 1) 
							- J(minib_X,minib_y,''' + 
						print_weights_for_finite_difference(net, i, 'b', 'b' + str(i) + '_minus') + 
						''', ''' + l2_reg_terms + ''', 1) ) / (2 * epsilon)
					}
					println( (db''' + str(i) + '''*(-1/layer''' + str(i) + '''_lr) - b''' + str(i) +
					'''_finite_differences).abs.max )
					writeVector(b''' + str(i) + '''_finite_differences, "apps/src/NeuralNetwork/''' + name + 
					'''/b''' + str(i) + '''_finite_differences.txt")
					writeVector(db''' + str(i) + '''*(-1/layer''' + str(i) + 
					'''_lr), "apps/src/NeuralNetwork/''' + name + '''/b''' + str(i) + '''_gradient.txt")
					''')
		if i != len(net)-1:
			f.write('''
					print("  Cross-Time Weights:   ")
					val wR''' + str(i) + '''_finite_differences = (0::wR''' + str(i) + '''.numRows, 0::wR''' + str(i) + '''.numCols) { (r,c) =>
						val wR''' + str(i) + '''_plus = wR''' + str(i) + '''.mutable
						wR''' + str(i) + '''_plus(r,c) = wR''' + str(i) + '''_plus(r,c) + epsilon
						val wR''' + str(i) + '''_minus = wR''' + str(i) + '''.mutable
						wR''' + str(i) + '''_minus(r,c) = wR''' + str(i) + '''_minus(r,c) - epsilon
						( J(minib_X,minib_y,''' + 
						print_weights_for_finite_difference(net, i, 'r', 'wR' + str(i) + '_plus') + 
						''', ''' + l2_reg_terms + ''', 1) 
							- J(minib_X,minib_y,''' + 
						print_weights_for_finite_difference(net, i, 'r', 'wR' + str(i) + '_minus') + 
						''', ''' + l2_reg_terms + ''', 1) ) / (2 * epsilon)
					}
					println( (dwR''' + str(i) + '''*(-1/layerR''' + str(i) + 
						'''_lr) - wR''' + str(i) + '''_finite_differences).abs.max )
					writeMatrix(wR''' + str(i) + '''_finite_differences, "apps/src/NeuralNetwork/''' + name + '''/wR''' + str(i) + 
					'''_finite_differences.txt")
					writeMatrix(dwR''' + str(i) + '''*(-1/layerR''' + str(i) + 
						'''_lr), "apps/src/NeuralNetwork/''' + name + '''/wR''' + str(i) + '''_gradient.txt")

					''')
	f.write('''
				}
				///////////////////////////////////////////////////////////////////////////////////
				// END OF GRADIENT CHECK
				///////////////////////////////////////////////////////////////////////////////////
*/

				// Update parameters
				''')

	for i,l in enumerate(net):
		f.write('w' + str(i) + ' = w' + str(i) + ' + dw' + str(i) + '''
				''')
		f.write('b' + str(i) + ' = b' + str(i) + ' + db' + str(i) + '''
				''')
		if i != len(net)-1:
			f.write('wR' + str(i) + ' = wR' + str(i) + ' + dwR' + str(i) + '''
				''')

	f.write('''
			} // Mini-Batch
		}  // Epoch

		println("\\nFinished Training")

		// Save these for later
		if (write_model == 1) {
			print("Writing models to file...")''')

	for i,l in enumerate(net):
		f.write(print_read_weights_from_file_for_layer(i,l,name))

	f.write('''
			println("done")
		}

		(''' + print_weight_vars(net) + ''')
	}
	''')


def write_train_def(f, net, name, use_dropout):
	write_training_header(f, net, name)
	write_training_loop(f, net, use_dropout)
	write_training_ending(f, net, name)

# ------------------------------------------------------------------------------
# Writing main def
# ------------------------------------------------------------------------------

def write_main(f, net, name):
	dataset_path = net.get('dataset_path')
	if not dataset_path:
		err('''Please add a dataset_path attribute to the <net> tag,
either as a relative path to /published/OptiML/, or an absolute path.
E.g.: dataset_path="apps/src/NeuralNetwork/examples/stock_market/"''')
	weight_vars = print_weight_vars(net)
	f.write('''

	def main() = {
	''')
	f.write('''
		val X = readMatrix[Double]("'''+ dataset_path + '''/train_data.txt", s => s.toDouble)''')
	f.write('''
		val y = readMatrix[Double]("'''+ dataset_path +'''/train_labels.txt", s => s.toDouble)

		// Train and return parameters
		val (''' + weight_vars + ''') = train(X, y)

		// Check classification error
		val glob_params = readVector[Double]("apps/src/NeuralNetwork/''' + name + '''/global_params.txt", line => line(0).toDouble, ";" )
		print("Running on training set...")
		validate(X, y, ''' + weight_vars + ''', glob_params(7).toInt)
		''')
	f.write('''
		// Check validation error (may want to skip this too)
		val val_X = readMatrix[Double]("''' + dataset_path + '''/val_data.txt", s => s.toDouble)''')

	f.write('''
		val val_y = readMatrix[Double]("''' + dataset_path + '''/val_labels.txt", s => s.toDouble)
		print("Running on validation set...")
		validate(val_X, val_y, ''' + weight_vars + ''', glob_params(8).toInt)
	}
''')

def write_validate_def(f, net):

	samples_per_window = net.get('samples_per_window')
	name = net.get('name')

	f.write('''
	def get_cross_entropy_and_classification_error
	(
		// Data''')
	f.write('''
		X: Rep[DenseMatrix[Double]], y: Rep[DenseMatrix[Double]],''')
	f.write('''
		// Weights''' + print_weight_vars_function_input(net) + ''',

		minib_size: Rep[Int], // Can pass in 1 for online testing
		print_rnn_output: Rep[Int]
	) = {

		val samples_per_window = ''' + samples_per_window + '''
	
		val T = X.numCols / samples_per_window

		var classification_error = 0.0
		var c_entropy = 0.0 // Cross entropy
		val num_test = X.numRows
		val num_minibatches = num_test / minib_size
		var idx = 0

		while (idx < num_minibatches) {
			val (''' + print_output_vars(net) + ''') = 
				feed_forward(samples_per_window, X.sliceRows(idx*minib_size, (idx+1)*minib_size), ''' + print_weight_vars(net) + ''')
			var t = 0
			if (print_rnn_output == 1) {
				println("")
				println("")
				print(idx)
				print(":")
			}
			while (t < T) {
				val y_col = y.sliceRows(idx*minib_size, (idx+1)*minib_size).getCol(t)
				classification_error += get_classification_error(o''' + str(len(net)-1) + '''(t), y_col)
				c_entropy += cross_entropy(o''' + str(len(net)-1) + '''(t), y_col)
				if (print_rnn_output == 1) {
					print(" ")
					print(o''' + str(len(net)-1) + '''(t).getRow(0).maxIndex)
				}
				t += 1
			}
			idx += 1
		}
		(c_entropy, classification_error/num_minibatches/T)
	}

	// Validation
	def validate
	(
		// Data''')
	f.write('''
		X: Rep[DenseMatrix[Double]], y: Rep[DenseMatrix[Double]],''')
	f.write('''
		// Weights''' + print_weight_vars_function_input(net) + ''',

		minib_size: Rep[Int] // Can pass in 1 for online testing
	) = {

		val glob_params = readVector[Double]("apps/src/NeuralNetwork/''' + name + '''/global_params.txt", line => line(0).toDouble, ";" )
		val print_rnn_output = glob_params(5).toInt
		val (c_entropy, classification_error) = get_cross_entropy_and_classification_error(X, y, ''' + print_weight_vars(net) + ''', minib_size, print_rnn_output)
		print("Classification Error (")
		print(X.numRows)
		print(" examples):  ")
		print(classification_error)
		print("%, Cross Entropy (")
		print(X.numRows)
		print(" examples):  ")
		println(c_entropy)
	}

	// Returns the cost function, which is -1/m * cross entropy
	// (since we want to maximize CE, or in this case minimize its -ve)
	// and then to that term, add the L2 term
	def J
	(
		// Data''')
	f.write('''
		X: Rep[DenseMatrix[Double]], y: Rep[DenseMatrix[Double]],''')
	f.write('''
		// Weights''' + print_weight_vars_function_input(net) + ''',''')
	for i,l in enumerate(net):
		f.write('''
		layer''' + str(i) + '''_L2reg: Rep[Double],''')
		if i != len(net)-1:
			f.write('''
		layerR''' + str(i) + '''_L2reg: Rep[Double],''')
	f.write('''
		minib_size: Rep[Int] // Can pass in 1 for online testing
	) = {

		val (c_entropy, classification_error) = get_cross_entropy_and_classification_error(X, y, ''' + print_weight_vars(net) + ''', minib_size, 0)
		''')

	f.write('''
		val L2_term = ''')
	for i,l in enumerate(net):
		f.write('''
				w''' + str(i) + '''.map(e => e*e).sum * layer''' + str(i) + '''_L2reg''')
		if i != len(net)-1:
			f.write(''' +
				wR''' + str(i) + '''.map(e => e*e).sum * layerR''' + str(i) + '''_L2reg + ''')
	f.write('''
		c_entropy*(-1.0/X.numRows.toDouble) + ( L2_term / 2 )
	}
''')

# ------------------------------------------------------------------------------
# Read and parse xml file
# ------------------------------------------------------------------------------
if len(sys.argv) != 2:
	err('Usage: python generate_rnn.py net.xml')
filename = sys.argv[1]
if not os.path.isfile(filename):
	err(filename + ' does not exist')
print "\n" + 'Generating neural network from ' + filename + '...'
tree = ET.parse(filename)
# Get the name of the app
net = tree.getroot()
name = net.get('name')

use_dropout = False
for l in net:
	if l.attrib.get('dropout'):
		use_dropout = True
		break

verify_net_architecture(net)

# ------------------------------------------------------------------------------
# Generate parameter files
# ------------------------------------------------------------------------------

# Create an output directory
if not os.path.exists(name):
    os.makedirs(name)

generate_parameter_files(name, net)
generate_weight_files(name, net)

# ------------------------------------------------------------------------------
# Generate OptiML file
# ------------------------------------------------------------------------------
optiml_file = open(name + '/' + name + '.scala', 'w')

# Write the file header
write_optiml_header(optiml_file, name)

# Write the feed-forward def
num_layers = len(net)
if use_dropout:
	write_fw_header_dropout(optiml_file, net)
	write_fw_layers_dropout(optiml_file, net, net.get('blas')=='1')
	write_fw_ending(optiml_file, num_layers, net)

write_fw_header(optiml_file, net)
write_fw_layers(optiml_file, net, net.get('blas')=='1')
write_fw_ending(optiml_file, num_layers, net)

# Write train def (SGD/back-prop)
write_train_def(optiml_file,net,name, use_dropout)

# Write validation def
write_validate_def(optiml_file,net)

# Write the main
write_main(optiml_file, net, name)

# Close the file
write_optiml_ending(optiml_file, name)
optiml_file.close()

# ------------------------------------------------------------------------------
# Finish
# ------------------------------------------------------------------------------
print 'Generated ' + name + '/' + name + '''.scala. To train and run the network:
From /published/OptiML,
	sbt compile
	bin/delitec ''' + name + '''Compiler
	bin/delite ''' + name + '''Compiler
To modify parameters (e.g. learning rate, # epochs) modify the files:
	''' + name + '''/layer_*_params.txt
	''' + name + '''/global_params.txt
You can also modify ''' + name + '''/global_params.txt to view the network outputs
'''

