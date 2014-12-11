#!/usr/local/bin/python

# ------------------------------------------------------------------------------
#
# This script reads in an xml file describing a convolutional neural network
# and generates OptiML. See cnn_example.xml for a sample input file.
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
import math

# ------------------------------------------------------------------------------
# Utilities
# ------------------------------------------------------------------------------

def err (msg) :
	print('Error: ' + msg + '\n')
	exit(0)


def print_weight_vars_for_layer(i,l):
	# Parameters depend on the type of layer
	s = ''
	type = l.attrib['type']
	if type != 'MAX_POOL':
		s += ('w' + str(i) + ', b' + str(i))
	return s

def print_weight_vars(net):
	s = ''
	num_layers = len(net)
	for i,l in enumerate(net):
		s += print_weight_vars_for_layer(i,l)
		if i != num_layers-1 and l.attrib['type'] != 'MAX_POOL':
			s += ', '
	return s

def print_weight_vars_function_input_for_layer(i,l):
	s = ''
	type = l.attrib['type']
	if type != 'MAX_POOL':
		s += ('''w''' + str(i) + ''': Rep[DenseMatrix[Double]], b''' + str(i) + ''': Rep[DenseVector[Double]]''')
	return s


def print_weight_vars_function_input(net):
	s = ''
	num_layers = len(net)
	for i,l in enumerate(net):
		s += ('''
		''' + print_weight_vars_function_input_for_layer(i,l))
		if i != num_layers-1 and l.attrib['type'] != 'MAX_POOL':
			s += ', '
	return s

def print_write_weights_to_file_for_layer(i,l,write_dir):
	s = ''
	type = l.attrib['type']
	if type != 'MAX_POOL':
		# Weight and bias
		s += ('''
				writeMatrix(w'''  + str(i) + ''', "''' + write_dir + '''w''' + str(i) + '''.txt")
				writeVector(b'''  + str(i) + ''', "''' + write_dir + '''b''' + str(i) + '''.txt")
				writeMatrix(dw''' + str(i) + ''', "''' + write_dir + '''dw''' + str(i) + '''.txt")
				writeVector(db''' + str(i) + ''', "''' + write_dir + '''db''' + str(i) + '''.txt")''')
	return s

def create_weights_file_for_layer(i,l,name):
	type = l.attrib['type']
	if type != 'MAX_POOL':
		filenames = ['w', 'b', 'dw', 'db']
		for fname in filenames:
			# Weight and bias
			filename = name + '/' + fname + str(i) + '.txt'
			if not os.path.isfile(filename):
				f = open(filename, "w")
				f.write("0")
				f.close()

def print_output_vars(net):
	s = ''
	for i,l in enumerate(net):
		s += ('o' + str(i))
		if l.attrib['type'] == 'MAX_POOL':
			s += (', pool_indices_o' + str(i))
		if i != num_layers-1:
			s += ', '
	return s

def generate_parameter_files(name, net):
	# Create a parameter file for each layer
	# The xml specifies the type of each layer, but
	# other parameters such as the learning rate can be adjusted
	# without having to re-generate any code
	for idx in range(len(net)):
		if net[idx].attrib['type'] == 'MAX_POOL':
			continue
		filename = name + '/layer_' + str(idx) + '_params.txt'
		if not os.path.exists(filename):
			param_file = open(filename, 'w')
			LR_description = 'Learning Rate'
			if net.get('lr_cmd_line_arg') == '1':
				LR_description = 'This is ignored (xml specified learning rate through command line)'
			param_file.write('''0.01 ; ''' + LR_description + '''
0.0001 ; L2 Regularization
0.9 ; momentum
0.1 ; initial weights
0.0 ; initial biases''')
			param_file.close()

	# Also create a file for global parameters
	filename = name + '/global_params.txt'
	if not os.path.exists(filename):
		param_file = open(filename, 'w')
		param_file.write('''1 ; Num Epochs
100 ; Mini-Batch size
0 ; Read model from file
0 ; Read momentum from file
10 ; Num epochs between saving model (0 to disable)
10 ; Num epochs between testing on training set (0 to disable)
1000 ; Test mini-batch size, training set (for final error report)
10 ; Num epochs between testing on validation set (0 to disable)
1000 ; Test mini-batch size, validation set (for final error report)
0 ; Test mode (0 disables testing, 1 disables training)
1000 ; Test set mini-batch size''')
		param_file.close()

def generate_weight_files(name, net):
	for i,l in enumerate(net):
		create_weights_file_for_layer(i,l,name)


def verify_net_architecture(net):

	# Only valid layers
	allowed_list = ['SOFTMAX', 'MAX_POOL', 'CONVOLUTION', 'FULLY_CONNECTED']
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

def print_weights_for_finite_difference(net, perturbation_index, type, weight_name):
	weights = []
	for i,l in enumerate(net):
		if l.attrib['type'] == 'MAX_POOL':
			continue
		if i == perturbation_index:
			if type == 'w':
				weights.append(weight_name)
			else:
				weights.append('w' + str(i))

			if type == 'b':
				weights.append(weight_name)
			else:
				weights.append('b' + str(i))
		else:
			weights.append('w' + str(i))
			weights.append('b' + str(i))
	return ','.join(weights)

# ------------------------------------------------------------------------------
# Writing OptiML boilerplate code
# ------------------------------------------------------------------------------

def write_optiml_header (f, fname, net):
	extra_arg = ''
	if net.get('lr_cmd_line_arg') == '1':
		extra_arg = ' <LearningRate, e.g. 0.01>'
	f.write('''
/* Use the following commands to run this file:
From /published/OptiML,
 
sbt compile
bin/delitec ''' + fname + '''Compiler --cuda
bin/delite ''' + fname + '''Compiler --cuda 1 ''' + extra_arg + '''

*/

import optiml.compiler._
import optiml.library._
import optiml.shared._

import java.util.Date // For writing weights back to file

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
		// Data''')
	f.write('''
		X: Rep[DenseMatrix[Double]],''')
	f.write('''
		// Weights''' + print_weight_vars_function_input(net) + '''
	) = {
''')

def write_fw_layers (f,net,blas,first_layer_num_feature_maps,input_image_size):

	# Keep track of # feature maps @ each layer
	prev_layer_num_fmaps = str(first_layer_num_feature_maps)
	current_fmap_size = input_image_size

	for i,l in enumerate(net):
		type = l.attrib['type']
		if type == 'MAX_POOL':
			f.write('''
		// Layer ''' + str(i) + ': ' + l.attrib['name'] + '''
		val (o''' + str(i) + ''', pool_indices_o''' + str(i) + ''') = ''')
		else:
			f.write('''
		// Layer ''' + str(i) + ': ' + l.attrib['name'] + '''
		val o''' + str(i) + ''' = (''')
		if type == 'SOFTMAX':
			if blas:
				f.write('softmax_fw_blas (')
			else:
				f.write('softmax_fw (')
		elif type == 'FULLY_CONNECTED':
			if blas:
				f.write('fullycon_fw_blas (')
			else:
				f.write('fullycon_fw (')
		elif type == 'CONVOLUTION':
			if net.get('gemm_conv') == '1':
				f.write('conv_fw_gemm (')
			else:
				f.write('conv_fw (')
		elif type == 'MAX_POOL':
			f.write('max_pool_indices (')
		else:
			err("Unsupported type in write_fw_layer")

		if i==0:
			f.write('X, ')
		else:
			f.write('o' + str(i-1) + ', ')

		# Extra arguments for other layers
		# 'Extra' because these aren't actually needed (the information is contained
		# in the already supplied arguments, e.g. by the matrix # rows/cols), but 
		# providing these arguments as constants rather than variables can provide
		# significant speedups to the compiled generated code (>30% speedup)
		if type == 'MAX_POOL':
			pool_size = l.attrib['pool_size']
			f.write(str(pool_size) + ', ' + prev_layer_num_fmaps)
			additional_params = []
			additional_params.append(str(current_fmap_size))
			additional_params.append(str(int(math.sqrt(current_fmap_size))))
			current_fmap_size = current_fmap_size / int(pool_size) / int(pool_size) 
			additional_params.append(str(current_fmap_size))
			additional_params.append(str(int(math.sqrt(current_fmap_size))))
			f.write(', ' + ', '.join(additional_params))
			f.write(')')
		else:
			if type == 'CONVOLUTION':
				f.write(prev_layer_num_fmaps + ', ')
				prev_layer_num_fmaps = l.attrib['num_hidden']
			f.write(print_weight_vars_for_layer(i,l))
			if type == 'CONVOLUTION':
				additional_params = []
				additional_params.append(prev_layer_num_fmaps)
				conv_size = int(l.attrib['kernel_size'])
				additional_params.append(str(conv_size*conv_size))
				additional_params.append(str(conv_size))
				additional_params.append(str(int(conv_size/2)))
				additional_params.append(str(current_fmap_size))
				additional_params.append(str(int(math.sqrt(current_fmap_size))))
				f.write(', ' + ', '.join(additional_params))
			f.write(')')
			if type == 'SOFTMAX':
				f.write(')')
			elif type == 'CONVOLUTION' or type == 'FULLY_CONNECTED':
				if l.attrib.get('activation') == 'LOGISTIC':
					f.write(').map(logistic)')
				elif l.attrib.get('activation') == 'LINEAR':
					f.write(')')
				else: # ReLU
					f.write(').map(ReLU)')
				if l.attrib.get('dropout'):
					if type == 'CONVOLUTION':
						err('Currently dropout is only supported in fully-connected layers' + 
							"\n" + '(but should be easy to add for conv layers as well)')
					f.write('.*(' + l.attrib['dropout'] + ')')
		f.write('''
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

# TODO: Should merge this with write_fw_layers, no need to duplicate all the code
# for 2 small changes.
def write_fw_layers_dropout (f,net,blas,first_layer_num_feature_maps,input_image_size):

	# Keep track of # feature maps @ each layer
	prev_layer_num_fmaps = str(first_layer_num_feature_maps)
	current_fmap_size = input_image_size

	for i,l in enumerate(net):
		type = l.attrib['type']
		if type == 'MAX_POOL':
			f.write('''
		// Layer ''' + str(i) + ': ' + l.attrib['name'] + '''
		val (o''' + str(i) + ''', pool_indices_o''' + str(i) + ''') = ''')
		else:
			if l.attrib.get('dropout'):
				f.write('''
		// Layer ''' + str(i) + ': ' + l.attrib['name'] + ''', with dropout
		val o''' + str(i) + '''_no_dropout = (''')
			else:
				f.write('''
		// Layer ''' + str(i) + ': ' + l.attrib['name'] + '''
		val o''' + str(i) + ''' = (''')
		if type == 'SOFTMAX':
			if blas:
				f.write('softmax_fw_blas (')
			else:
				f.write('softmax_fw (')
		elif type == 'FULLY_CONNECTED':
			if blas:
				f.write('fullycon_fw_blas (')
			else:
				f.write('fullycon_fw (')
		elif type == 'CONVOLUTION':
			if net.get('gemm_conv') == '1':
				f.write('conv_fw_gemm (')
			else:
				f.write('conv_fw (')
		elif type == 'MAX_POOL':
			f.write('max_pool_indices (')
		else:
			err("Unsupported type in write_fw_layer")

		if i==0:
			f.write('X, ')
		else:
			f.write('o' + str(i-1) + ', ')

		# Extra arguments for other layers
		if type == 'MAX_POOL':
			pool_size = l.attrib['pool_size']
			f.write(str(pool_size) + ', ' + prev_layer_num_fmaps)
			additional_params = []
			additional_params.append(str(current_fmap_size))
			additional_params.append(str(int(math.sqrt(current_fmap_size))))
			current_fmap_size = current_fmap_size / int(pool_size) / int(pool_size) 
			additional_params.append(str(current_fmap_size))
			additional_params.append(str(int(math.sqrt(current_fmap_size))))
			f.write(', ' + ', '.join(additional_params))
			f.write(')')
		else:
			if type == 'CONVOLUTION':
				f.write(prev_layer_num_fmaps + ', ')
				prev_layer_num_fmaps = l.attrib['num_hidden']
			f.write(print_weight_vars_for_layer(i,l))
			if type == 'CONVOLUTION':
				additional_params = []
				additional_params.append(prev_layer_num_fmaps)
				conv_size = int(l.attrib['kernel_size'])
				additional_params.append(str(conv_size*conv_size))
				additional_params.append(str(conv_size))
				additional_params.append(str(int(conv_size/2)))
				additional_params.append(str(current_fmap_size))
				additional_params.append(str(int(math.sqrt(current_fmap_size))))
				f.write(', ' + ', '.join(additional_params))
			f.write(')')
			if type == 'SOFTMAX':
				f.write(')')
			elif type == 'CONVOLUTION' or type == 'FULLY_CONNECTED':
				if l.attrib.get('activation') == 'LOGISTIC':
					f.write(').map(logistic)')
				elif l.attrib.get('activation') == 'LINEAR':
					f.write(')')
				else: # ReLU
					f.write(').map(ReLU)')
				if l.attrib.get('dropout'):
					f.write('''
		val o''' + str(i) + ''' = (0::o''' + str(i) + '''_no_dropout.numRows, 0::o''' + str(i) + '''_no_dropout.numCols) { (r,c) => 
			o''' + str(i) + '''_no_dropout(r,c) * layer''' + str(i) + '''_dropout(c)
		}''')
		f.write('''
			''')


# ------------------------------------------------------------------------------
# Writing training def
# ------------------------------------------------------------------------------

def write_training_header(f, net, name, use_feature_maps, first_layer_num_feature_maps, input_image_size):


	# Declare the def and some variables
	f.write('''
	def train(X: Rep[DenseMatrix[Double]], y: Rep[DenseVector[Double]],
		val_X: Rep[DenseMatrix[Double]], val_y: Rep[DenseVector[Double]]) = {''')
	f.write('''

		// Initialize parameters for each layer
		val m = X.numRows	// m = # examples
		val n = ''' + str(input_image_size) + ''' 	// n = # features
		val k = ''' + net[-1].attrib['num_hidden'] + ''' // k = # classes (# outputs of final layer)

		// Global settings
		val glob_params = readVector[Double]("apps/src/NeuralNetwork/''' + name +'''/global_params.txt", line => line(0).toDouble, ";" )
		val num_epochs = glob_params(0).toInt
		val minib_m = glob_params(1).toInt
		val read_model = glob_params(2).toInt
		val read_mtm = glob_params(3).toInt
		val save_freq = glob_params(4).toInt
		val test_freq_train_set = glob_params(5).toInt
		val testing_minib_size_train = glob_params(6).toInt
		val test_freq_val_set = glob_params(7).toInt
		val testing_minib_size_val = glob_params(8).toInt
	''')

	# Create parameters for each layer
	for i,l in enumerate(net):

		if l.attrib['type'] == 'MAX_POOL':
			continue

		layer_id = 'layer' + str(i)
		f.write('''
		// Parameters for layer ''' + str(i) + ', ' + l.attrib['name'] + '''
		val ''' + layer_id + '''_params = readVector[Double]("apps/src/NeuralNetwork/''' + name +'''/layer_''' + str(i) + '''_params.txt", line => line(0).toDouble, ";" )
		''')
		if net.get('lr_cmd_line_arg') == '1':
			f.write('''var ''' + layer_id + '''_lr = args(0).toDouble
		''')
		else:
			f.write('''var ''' + layer_id + '''_lr = ''' + layer_id + '''_params(0)
		''')
		f.write('''val ''' + layer_id + '''_L2reg = ''' + layer_id + '''_params(1) // L2 regularization
		val ''' + layer_id + '''_mtm = ''' + layer_id + '''_params(2) // momentum
		val ''' + layer_id + '''_init_w = ''' + layer_id + '''_params(3) // initial weights
		val ''' + layer_id + '''_init_b = ''' + layer_id + '''_params(4) // initial biases
		val ''' + layer_id + '''_num_hidden = ''' + l.attrib['num_hidden'] + '''
		''')

	current_feature_map_size = int(input_image_size)

	f.write('''

		// Parameters for decreasing the learning rate over time
		val recent_history_size = 2 // Can increase beyond 2 to allow small fluctuations in error rate
		var history_counter = 0
		val train_logfile = "apps/src/NeuralNetwork/''' + name + '''/train.log"
		val train_err_history = DenseVector.zeros(num_epochs / max(1,test_freq_train_set)).mutable
		val val_logfile = "apps/src/NeuralNetwork/''' + name + '''/val.log"
		val val_err_history = DenseVector.zeros(num_epochs / max(1,test_freq_val_set)).mutable
		val val_err_recent_history = (DenseVector.ones(recent_history_size)*(100)).mutable

		if (read_model == 1) {
			print("Reading models from file...")
		}
	''')

	# Define the weight matrices for each layer
	# These depend on the previous layer, with the first layer's size depending on the #input columns
	cols_of_prev_layer = 'n'
	first_nonconv_layer = True
	for i,l in enumerate(net):
		
		f.write("\n")
		cols_of_this_layer = 'layer' + str(i) + '_num_hidden'

		# Create the weight matrices
		type = l.attrib['type']
		if type == 'SOFTMAX':

			if first_nonconv_layer and use_feature_maps:
				first_nonconv_layer = False
				cols_of_prev_layer = '(' + cols_of_prev_layer + '*' + str(current_feature_map_size) + ')'

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
			if first_nonconv_layer and use_feature_maps:
				first_nonconv_layer = False
				cols_of_prev_layer = '(' + cols_of_prev_layer + '*' + str(current_feature_map_size) + ')'

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
			# Set up the # rows for the next matrix
			cols_of_prev_layer = cols_of_this_layer
		elif type == 'CONVOLUTION':

			kernel_dim = int( l.attrib['kernel_size'] )
			num_2D_kernel_elements = kernel_dim*kernel_dim

			# First layer
			if i==0:
				cols_of_prev_layer = first_layer_num_feature_maps

			# The number of feature maps of the previous layer is the number of columns of the prev layer output unit matrix
			# So actually we do still need this # cols of the prev layer, but not that by itself, also multiplied by k*k

			# Weight format: The convolutional weights are stored in a single matrix, where each column
			# corresponds to the weights of an output feature map.
			#
			# For example, consider the case where a convolutional layer has 32 input feature maps, 64 output 
			# feature maps, and 5x5 convolutional kernels. The number of columns will be 64, and the number of 
			# rows will be 5x5x32. So column 1 contains all the convolutional weights going from the 32 input feature 
			# maps to output feature map #1. Column 2 contains the weights from the 32 inputs to output map #2, etc.
			# The 5x5x32 weights in each column are grouped by input feature map, i.e. the first 25 rows of a column
			# correspond to the convolution weights for input feature map 1, the next 25 rows of a column correspond
			# to the convolution weights for input feature map 2, etc. Finally, within this group of 25 weights, the
			# convolution weights of each 5x5 kernel are stored in row-major order, i.e. first row 1, then row 2, etc.
			num_3D_kernel_elements = '(' + str(num_2D_kernel_elements) + '*' + cols_of_prev_layer + ')'
			f.write("\t\t// Weights for Convolutional Layer\n")
			f.write("\t\t" + 'var w' + str(i) + '  = DenseMatrix.randn(' + str(num_3D_kernel_elements) + ', ' + cols_of_this_layer + ')*(layer' + str(i) + '_init_w)' + "\n")
			f.write("\t\t" + 'var b' + str(i) + '  = DenseVector.ones (' + cols_of_this_layer + ')*(layer' + str(i) + '_init_b)' + "\n")
			f.write("\t\t" + 'var dw' + str(i) + ' = DenseMatrix.zeros(' + str(num_3D_kernel_elements) + ', ' + cols_of_this_layer + ')' + "\n")
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
			cols_of_prev_layer = cols_of_this_layer
		elif type == 'MAX_POOL':
			pool_size = int(l.attrib['pool_size'])
			current_feature_map_size = current_feature_map_size/(pool_size*pool_size)
			# First layer
			if i==0:
				cols_of_prev_layer = first_layer_num_feature_maps
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
	

def write_training_loop(f, net, use_dropout, first_layer_num_feature_maps,input_image_size):
	num_layers = len(net)
	weight_updates = '''
				// TODO: The operations below can be computed in parallel (task-level)

	'''
	blas = (net.get('blas')=='1')
	f.write('''
		var epoch = 0
		// Serial Iteration
		while(epoch < num_epochs) {

			epoch+=1
			print("Epoch ")
			println(epoch)

			var minib_epoch = 0
			// Serial iteration
			while (minib_epoch < m/minib_m) {

				val start_index = minib_epoch * minib_m
				val stop_index  = (minib_epoch+1) * minib_m
				val minib_X = (start_index::stop_index, 0::X.numCols) { (r,c) => X(r,c) }
				val minib_y = y.slice(start_index, stop_index)''')

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
				val (''' + print_output_vars(net) + ''') = ''')
	if use_dropout:
		f.write('''
					feed_forward_dropout(minib_X, ''')
	else:
		f.write('''
					feed_forward(minib_X, ''')
	f.write(print_weight_vars(net))
	for i,l in enumerate(net):
		if l.attrib.get('dropout'):
			f.write(', layer' + str(i) + '_dropout')
	f.write(''')

			    // Back Prop
	''')    

	# Do a quick iteration to figure out the final feature map size (after downsampling)
	downsampled_size = input_image_size
	for i,l in enumerate(net):
		if l.attrib['type'] == 'MAX_POOL':
			pool_size = int(l.attrib['pool_size'])
			downsampled_size = downsampled_size / pool_size / pool_size
	
	# Now do back-prop wrt each other layer
	found_conv_layer_already = False
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

		# Here prev_layer means down towards the softmax, i.e. last is input and prev
		# is conv, pool, fc, sm, etc.
		prev_layer = i+1
		while prev_layer < len(net) and net[prev_layer].attrib['type'] == 'MAX_POOL':
			prev_layer += 1
		prev_dJ_do_IN = 'dJ_do' + str(prev_layer) + 'IN' 
		dJ_doi_OUT = 'dJ_do' + str(i) + 'OUT' 
		dJ_doi_IN  = 'dJ_do' + str(i) + 'IN'
		dJ_dWi     = 'dJ_dw' + str(i)
		dJ_dbi     = 'dJ_db' + str(i)
		layer_id   = 'layer' + str(i)

	    # TODO: For now I hard-code the cost function to be cross Entropy
	    # and assume the output layer is a softmax. So there is a special-
	    # case for the final layer. In the future if outputs other than 
	    # softmax are supported, add them here

	    # Write layer derivative for softmax
		if i==num_layers-1:
			if type != 'SOFTMAX':
				err("Currently, only SOFTMAX is supported for the final layer")
			f.write('''
			    // ''' + str(i) + '''a) Derivative of J wrt input of softmax layer
				val ''' + dJ_doi_IN + ''' = (0::minib_m, 0::k) {
					(i,j) => {
	 					val prob = o''' + str(i) + '''(i,j)
						if (minib_y(i) == j) 1-prob
						else -1*prob
					}
				}
			''')

	    # Write layer derivative for convolution, if this is not the final convolution
		elif type == 'CONVOLUTION' and found_conv_layer_already:
			# Derivative wrt convolutional layer given that we already found a
			# convolutional layer

			conv_size = int(net[prev_layer].attrib['kernel_size'])

			if net[prev_layer+1].attrib['type'] == 'MAX_POOL':

				if net[prev_layer+2].attrib['type'] == 'MAX_POOL':
					err("Backprop not currently supported for consecutive pooling layers")
				pool_size = net[prev_layer+1].attrib['pool_size']

				# We need to undo the most recent layer of pooling, since the convolution which
				# we are back-propagating potentially took place after the most recent pooling layer
				# (after in terms of towards the output)
				most_recent_pooling = 1
				if net[i+1].attrib['type'] == 'MAX_POOL':
					most_recent_pooling = int(net[i+1].attrib['pool_size'])
				upsampled_size_previous_convolution = int(downsampled_size / most_recent_pooling / most_recent_pooling)
				sqrt_upsampled_size_previous_convolution = float(math.sqrt(upsampled_size_previous_convolution))

				# That was the upsampled version of the previous convolution. But that
				# convolution was also followed by a pooling layer
				downsampled_size_previous_convolution = \
					int(upsampled_size_previous_convolution / int(pool_size) / int(pool_size))
				sqrt_downsampled_size_previous_convolution = \
					int(math.sqrt(downsampled_size_previous_convolution))

				f.write('''
			    // ''' + str(i) + '''a) Derivative of J wrt output and input of layer ''' + str(i) + '''
			    val ''' + dJ_doi_OUT +''' =
			    	conv_bw_data(w''' + str(prev_layer) + ''', layer''' + str(i) + 
			    		'''_num_hidden, dJ_do''' + str(prev_layer) + '''IN, ''' + pool_size + 
			    		''', pool_indices_o''' + str(prev_layer+1) + ''', layer''' + str(prev_layer) + '_num_hidden, ' +
				    	str(conv_size*conv_size) + ', ' + 
				    	str(conv_size) + ', ' + 
						str(downsampled_size_previous_convolution) + ', ' +
						str(sqrt_downsampled_size_previous_convolution) + ', ' +
						str(upsampled_size_previous_convolution) + ', ' +
						str(int(sqrt_upsampled_size_previous_convolution)) + ', ' +
						str(int(sqrt_upsampled_size_previous_convolution/2)) + ', ' +
						str(float(conv_size-sqrt_upsampled_size_previous_convolution)/2.0) + 
			    		''')
				''')

			else:

				# We need to undo the most recent layer of pooling, since this derivative
				# took place after the pooling layer
				most_recent_pooling = 1
				if net[i+1].attrib['type'] == 'MAX_POOL':
					most_recent_pooling = int(net[i+1].attrib['pool_size'])
				upsampled_size_previous_convolution = int(downsampled_size / most_recent_pooling / most_recent_pooling)
				sqrt_upsampled_size_previous_convolution = float(math.sqrt(upsampled_size_previous_convolution))

				f.write('''
			    // ''' + str(i) + '''a) Derivative of J wrt output and input of layer ''' + str(i) + '''
			    val ''' + dJ_doi_OUT +''' =
			    	conv_bw_data_no_pool(w''' + str(prev_layer) + ''', layer''' + str(i) + 
			    		'''_num_hidden, dJ_do''' + str(prev_layer) + '''IN, layer''' + str(i) + '_num_hidden, ' +
			    	str(conv_size*conv_size) + ', ' + 
			    	str(conv_size) + ', ' + 
					str(upsampled_size_previous_convolution) + ', ' +
					str(sqrt_upsampled_size_previous_convolution) + ', ' +
					str(upsampled_size_previous_convolution) + ', ' +
					str(int(sqrt_upsampled_size_previous_convolution)) + ', ' +
					str(int(sqrt_upsampled_size_previous_convolution/2)) + ', ' +
					str(float(conv_size-sqrt_upsampled_size_previous_convolution)/2.0) + 
			    		''')
				''')

	    # Write layer derivative for fully connected or the final convolution (same as fully connected)
		elif type == 'FULLY_CONNECTED' or type == 'CONVOLUTION':
			f.write('''
			    // ''' + str(i) + '''a) Derivative of J wrt output and input of layer ''' + str(i) + '''
				val w''' + str(prev_layer) + '''_IMM = w''' + str(prev_layer) + '''.Clone // Make immutable copy to avoid GPU errors (or can use matrix mult)''')
			if blas:
				f.write('''
			    val ''' + dJ_doi_OUT +''' = (''' + prev_dJ_do_IN + ''' * w''' + str(prev_layer) + '''_IMM.t)''')
			else:
				f.write('''
			    val ''' + dJ_doi_OUT +''' = (0::''' + prev_dJ_do_IN + '''.numRows, 0::w''' + str(prev_layer) + '''_IMM.numRows) { (r,c) =>
					sum(0, ''' + prev_dJ_do_IN + '''.numCols) { i =>
						''' + prev_dJ_do_IN + '''(r,i) * w''' + str(prev_layer) + '''_IMM(c,i)
					}
				}''')

	    # Write layer derivative wrt layer INPUT
	    # This is now the same for any fully connected or convolution (final or not) since we
	    # just need to take the quantity above (deriv wrt layer output) and backprop through the activation function
	    # Note, this depends on what that activation is, whether we used pooling, and also whether we use dropout
		if type == 'FULLY_CONNECTED' or type == 'CONVOLUTION':
			f.write('''
			    val ''' + dJ_doi_IN +''' = (0::''' + dJ_doi_OUT + '''.numRows, 0::''' + dJ_doi_OUT + '''.numCols) { (r,c) => 
					''')

			# Check 1: Did we follow this layer by a pooling layer? (only valid for convolution)
			if net[i+1].attrib['type'] == 'MAX_POOL':
				
				pool_size = net[i+1].attrib['pool_size']
				if net[i+2].attrib['type'] == 'MAX_POOL':
					err("Backprop not currently supported for consecutive pooling layers")
				f.write('val upsampled_idx = get_upsampled_index_from_downsampled(' + dJ_doi_OUT + ', layer' +
					str(i) + '_num_hidden, ' + pool_size + ', c, pool_indices_o' + str(i+1) + '''(r,c), ''' + 
					str(downsampled_size/int(pool_size)/int(pool_size)) + ', ' + \
					str(int(math.sqrt(downsampled_size))/int(pool_size)) + ', ' + \
					str(downsampled_size) + ', ' + \
					str(int(math.sqrt(downsampled_size))) + ''')
					''')

				# Check 2: Are we using dropout?
				if l.attrib.get('dropout'):

					# Check 3: Activation unit type
					if l.attrib.get('activation') == 'LOGISTIC':
						f.write(dJ_doi_OUT + '''(r,c) * o''' + str(i) + '''(r,upsampled_idx) * (1-o''' + str(i) + '''(r,upsampled_idx)) * layer''' + str(i) + '''_dropout(c)''')
					elif l.attrib.get('activation') == 'LINEAR':
						f.write(dJ_doi_OUT + '''(r,c) * layer''' + str(i) + '''_dropout(c)''')
					else: # ReLU
						# Note: This is a bit faster: (1-2% or so, maybe noise)
						# 	 dJ_doiOUT(r,c) * ReLU_Deriv(oi(r,c))
						f.write('if (o' + str(i) + '(r,upsampled_idx) > 0) (' + dJ_doi_OUT + '(r,c) * layer''' + str(i) + '''_dropout(c)) else 0.0''')

				# Check 2: No dropout
				else:

					# Check 3: Activation unit type
					if l.attrib.get('activation') == 'LOGISTIC':
						f.write(dJ_doi_OUT + '''(r,c) * o''' + str(i) + '''(r,upsampled_idx) * (1-o''' + str(i) + '''(r,upsampled_idx))''')
					elif l.attrib.get('activation') == 'LINEAR':
						f.write(dJ_doi_OUT + '''(r,c) // TODO: Ensure this is not an allocation''')
					else: # ReLU
						# Note: This is a bit faster: (1-2% or so, maybe noise)
						# 	 dJ_doiOUT(r,c) * ReLU_Deriv(oi(r,c))
						f.write('if (o' + str(i) + '(r,upsampled_idx) > 0) ' + dJ_doi_OUT + '(r,c) else 0.0''')

			# Check 1: The next layer is NOT a pooling layer
			else:

				# Check 2: Are we using dropout?
				if l.attrib.get('dropout'):

					# Check 3: Activation unit type
					if l.attrib.get('activation') == 'LOGISTIC':
						f.write(dJ_doi_OUT + '''(r,c) * o''' + str(i) + '''(r,c) * (1-o''' + str(i) + '''(r,c)) * layer''' + str(i) + '''_dropout(c)''')
					elif l.attrib.get('activation') == 'LINEAR':
						f.write(dJ_doi_OUT + '''(r,c) * layer''' + str(i) + '''_dropout(c)''')
					else: # ReLU
						# Note: This is a bit faster: (1-2% or so, maybe noise)
						# 	 dJ_doiOUT(r,c) * ReLU_Deriv(oi(r,c))
						f.write('if (o' + str(i) + '(r,c) > 0) (' + dJ_doi_OUT + '(r,c) * layer''' + str(i) + '''_dropout(c)) else 0.0''')

				# Check 2: No dropout
				else:

					# Check 3: Activation unit type
					if l.attrib.get('activation') == 'LOGISTIC':
						f.write(dJ_doi_OUT + '''(r,c) * o''' + str(i) + '''(r,c) * (1-o''' + str(i) + '''(r,c))''')
					elif l.attrib.get('activation') == 'LINEAR':
						f.write(dJ_doi_OUT + '''(r,c) // TODO: Ensure this is not an allocation''')
					else: # ReLU
						# Note: This is a bit faster: (1-2% or so, maybe noise)
						# 	 dJ_doiOUT(r,c) * ReLU_Deriv(oi(r,c))
						f.write('if (o' + str(i) + '(r,c) > 0) ' + dJ_doi_OUT + '(r,c) else 0.0''')
			f.write('''
				}
			''')

		if type == 'MAX_POOL':
			f.write('''
			    // Max-Pooling layer, derivatives are propagated below for
			    // max terms only
			''')

		# Get the name of the previous output layer (or the input, if this is layer 0)
		# Note: So here previous means previous in terms of layer order (i.e. L0 < L1 < L2)
		# Above, previous meant previous in terms of backprop order (L2 < L1 < L0)

		prev_layer_output = 'o' + str(i-1)
		if i==0:
			prev_layer_output = 'minib_X'

		weight_updates += '''
			    // ''' + str(i) + '''b) Derivative of J wrt layer ''' + str(i) + ''' weights

				// Derivative before L2 regularization'''
		if type == 'FULLY_CONNECTED' or type == 'SOFTMAX':
			if blas:
				weight_updates += '''
				val ''' + dJ_dWi + '''_no_reg = (''' + prev_layer_output + '''.t * ''' + dJ_doi_IN + ''').*(-1.0/minib_m.toDouble)'''
			else:
				weight_updates += '''
				val ''' + dJ_dWi + '''_no_reg = (0::''' + prev_layer_output + '''.numCols, 0::''' + dJ_doi_IN + '''.numCols) { (r,c) =>
					(-1.0/minib_m.toDouble)*(sum(0, ''' + prev_layer_output + '''.numRows) { i =>
						''' + prev_layer_output + '''(i,r) * ''' + dJ_doi_IN + '''(i,c)
					})
				}'''
			weight_updates += '''
			    val ''' + dJ_dbi + '''_no_reg = (0::''' + dJ_doi_IN + '''.numCols) {
			    	c => (-1.0/minib_m.toDouble)*''' + dJ_doi_IN + '''.getCol(c).sum
			    }'''
		elif type == 'CONVOLUTION':

			kernel_size = int( l.attrib['kernel_size'] )
			kernel_total_size = str(kernel_size*kernel_size)

			# Check if this is the conv layer closest to the input
			L1_num_fmaps = first_layer_num_feature_maps
			prev_conv_layer_num = i-1
			while prev_conv_layer_num >= 0 and net[prev_conv_layer_num].attrib['type'] != 'CONVOLUTION':
				prev_conv_layer_num = prev_conv_layer_num-1
			if prev_conv_layer_num >= 0:
				L1_num_fmaps = 'layer' + str(prev_conv_layer_num) + '_num_hidden'

			# Keep track of this so we know how to handle future convolution
			# layer derivatives
			found_conv_layer_already = True

			# This next call has a lot of input parameters
			# First, determine whether this conv layer is followed by a pooling layer
			if net[i+1].attrib['type'] == 'MAX_POOL':
				
				pool_size = net[i+1].attrib['pool_size']
				if net[i+2].attrib['type'] == 'MAX_POOL':
					# We could support back-prop for consecutive pooling layers but 
					# need to modify conv_bw_weights
					err("Backprop not currently supported for consecutive pooling layers")

				weight_updates += '''
				val ''' + dJ_dWi + '''_no_reg =
					conv_bw_weights(''' + kernel_total_size + ', ' + L1_num_fmaps + ', layer' + str(i) + \
						'''_num_hidden, ''' + prev_layer_output + ''', ''' + dJ_doi_IN + ', ' + \
						pool_size + ''', pool_indices_o''' + str(i+1) + \
						', ' + str(kernel_size) + ', ' + str(int(kernel_size/2)) + ', ' + \
						str(downsampled_size) + ', ' + \
						str(int(math.sqrt(downsampled_size))) + ', ' + \
						str(downsampled_size/int(pool_size)/int(pool_size)) + ', ' + \
						str(int(math.sqrt(downsampled_size))/int(pool_size)) + \
						''').*(-1.0/minib_m.toDouble)
				'''

			else:
				weight_updates += '''
				val ''' + dJ_dWi + '''_no_reg =
					conv_bw_weights_no_pool(''' + kernel_total_size + ''', ''' + L1_num_fmaps + ''', layer''' + str(i) + \
						 '''_num_hidden, ''' + prev_layer_output + ''', ''' + dJ_doi_IN + ', ' + \
						str(downsampled_size) + ', ' + \
						str(int(math.sqrt(downsampled_size))) + ', ' + \
						str(downsampled_size) + ', ' + \
						str(int(math.sqrt(downsampled_size))) + \
						''').*(-1.0/minib_m.toDouble)
				'''

			weight_updates += '''
				val layer''' + str(i) + '''_fmap_size = ''' + dJ_doi_IN + '''.numCols / layer''' + str(i) + '''_num_hidden
			    val ''' + dJ_dbi + '''_no_reg = (0::layer''' + str(i) + '''_num_hidden) {
			    	b => (-1.0/minib_m.toDouble)*sum(0, layer''' + str(i) + '''_fmap_size) { c => 
			    		''' + dJ_doi_IN + '''.getCol(b*layer''' + str(i) + '''_fmap_size + c).sum
			    	}
			    }'''

		if type == 'MAX_POOL':
			pool_size = int(l.attrib['pool_size'])
			downsampled_size = downsampled_size * pool_size * pool_size
		else:
			weight_updates += '''
				// Derivative with L2 regularization term, and also momentum
				dw''' + str(i) + ''' = (''' + dJ_dWi + '''_no_reg + w''' + str(i) + '''.*(''' + layer_id + '''_L2reg))*(-1*''' + layer_id + '''_lr.toDouble) + dw''' + str(i) + '''.*(''' + layer_id + '''_mtm)
				db''' + str(i) + ''' = (''' + dJ_dbi + '''_no_reg                     )*(-1*''' + layer_id + '''_lr.toDouble) + db''' + str(i) + '''.*(''' + layer_id + '''_mtm)

		'''

	# The gradients have been back-propagated through each layer
	# Now, use the derivatives above to update each weight matrix
	# Note: These can all be updated in parallel
	f.write(weight_updates)

def write_training_ending(f, net, name):

	# Get L2 regularization weights
	l2_reg_terms = []
	for i,l in enumerate(net):
		type = l.attrib['type']
		if type != 'MAX_POOL':
			l2_reg_terms.append('layer' + str(i) + '_L2reg')

	f.write('''
/*
				///////////////////////////////////////////////////////////////////////////////////
				//
				// Gradient check: 
				//
				// To run the gradient check, uncomment this block of code.
				// However, to avoid very long compiles and runtimes, it is recommended to:
				//	- Use a smaller network or dataset (e.g. ~5 feature maps per layer)
				//	- Use a small mini-batch size (e.g. 5)
				// Gradient checking also does not currently support dropout, although adding
				// this is as simple as modifying J to call feed_forward_dropout instead of
				// feed_forward, and passing in the dropout masks used during training above.
				//
				// If the network is very large, this section may also be long enough that
				// compilation will fail. In that case, comment out the iteration over epochs
				// and mini-batches (the 2 while loops) so this file only does gradient checking.
				// You can also comment out additional code in this file which doesn't contribue
				// to gradient checking, e.g. validation within main()
				//
				///////////////////////////////////////////////////////////////////////////////////
				println("")
				println("**********************")
				println("RUNNING GRADIENT CHECK")
				println("**********************")
				if (epoch == 1 && minib_epoch == 1) {
					val epsilon = 0.0001

					''')

	l2_reg_str = ', '.join(l2_reg_terms)
	for i,l in enumerate(net):
		if l.attrib['type'] == 'MAX_POOL':
			continue
		f.write('println("Layer ' + str(i) + '''")

					print("  Weights:  ")
					val w''' + str(i) + '''_finite_differences = (0::w''' + str(i) + 
						'''.numRows, 0::w''' + str(i) + '''.numCols) { (r,c) =>
						val w''' + str(i) + '''_plus = w''' + str(i) + '''.mutable
						w''' + str(i) + '''_plus(r,c) = w''' + str(i) + '''_plus(r,c) + epsilon
						val w''' + str(i) + '''_minus = w''' + str(i) + '''.mutable
						w''' + str(i) + '''_minus(r,c) = w''' + str(i) + '''_minus(r,c) - epsilon
						( J(minib_X,minib_y,''' + 
						print_weights_for_finite_difference(net, i, 'w', 'w' + str(i) + '_plus') + 
						''',''' + l2_reg_str + ''', minib_m) 
							- J(minib_X,minib_y,''' + 
						print_weights_for_finite_difference(net, i, 'w', 'w' + str(i) + '_minus') + 
						''', ''' + l2_reg_str + ''', minib_m) ) / (2 * epsilon)
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
						''', ''' + l2_reg_str + ''', minib_m) 
							- J(minib_X,minib_y,''' + 
						print_weights_for_finite_difference(net, i, 'b', 'b' + str(i) + '_minus') + 
						''', ''' + l2_reg_str + ''', minib_m) ) / (2 * epsilon)
					}
					println( (db''' + str(i) + '''*(-1/layer''' + str(i) + '''_lr) - b''' + str(i) +
					'''_finite_differences).abs.max )
					writeVector(b''' + str(i) + '''_finite_differences, "apps/src/NeuralNetwork/''' + name + 
					'''/b''' + str(i) + '''_finite_differences.txt")
					writeVector(db''' + str(i) + '''*(-1/layer''' + str(i) + 
					'''_lr), "apps/src/NeuralNetwork/''' + name + '''/b''' + str(i) + '''_gradient.txt")
					''')
	f.write('''
				}
				///////////////////////////////////////////////////////////////////////////////////
				// END OF GRADIENT CHECK
				///////////////////////////////////////////////////////////////////////////////////
*/

				''')

	# Print weight updates
	for i,l in enumerate(net):
		type = l.attrib['type']
		if type != 'MAX_POOL':
			f.write('''
				// Update parameters
				w''' + str(i) + '''  = w''' + str(i) + ''' + dw''' + str(i) + '''
				b''' + str(i) + '''  = b''' + str(i) + ''' + db''' + str(i) + '''
			''')

	f.write('''
			} // Mini-Batch

			// End of epoch
			// Check if we should run on the training/validation sets
			if (test_freq_train_set > 0 && epoch % max(1,test_freq_train_set) == 0) {

				val (train_c_entropy, train_classification_error) = 
					get_cross_entropy_and_classification_error(X, y, ''' + print_weight_vars(net) + ''', 
						testing_minib_size_train)
				train_err_history(epoch/test_freq_train_set - 1) = train_classification_error
				writeVector(train_err_history, train_logfile)
				print("  Training set error (")
				print(X.numRows)
				print(" examples): ")
				print(train_classification_error)
				print("%, Cross Entropy ")
				println(train_c_entropy)
			}

			// For validation error, additionally use the returned error to adjust the learning rate
			if (test_freq_val_set > 0 && epoch % max(1,test_freq_val_set) == 0) {
				val (val_c_entropy, val_classification_error) = 
					get_cross_entropy_and_classification_error(val_X, val_y, ''' + print_weight_vars(net) + ''', 
						testing_minib_size_val)
				val_err_history(epoch/test_freq_val_set - 1) = val_classification_error
				writeVector(val_err_history, val_logfile)
				print("  Validation set error (")
				print(val_X.numRows)
				print(" examples): ")
				print(val_classification_error)
				print("%, Cross Entropy ")
				println(val_c_entropy)

				// Check whether the learning rates should be decreased:

				// First add the newest error to the history, removing the oldest one
				val_err_recent_history(history_counter % recent_history_size) = val_classification_error
				history_counter += 1
				val val_err_recent_history_IMM = val_err_recent_history.Clone

				// Now check if there has been no improvement in the history period
				if ( (history_counter >= recent_history_size) &&
						(max(val_err_recent_history_IMM) - val_classification_error) <= 0.0 ) {
					println("  Decreasing all learning rates by a factor of 10")
					''')

	for i,l in enumerate(net):
		if l.attrib['type'] == 'MAX_POOL':
			continue
		f.write('''layer''' + str(i) + '''_lr += (layer''' + str(i) + '''_lr * -0.9) // TODO: Diving by 10 causes cuda errors, only += works...
					''')

	f.write('''
					// Clear the history for the new LR
					history_counter += (history_counter * -1) // Setting to zero causes errors..
				}
			}
			// Check if weights should be saved or validation should be done.
			if (save_freq > 0 && epoch % max(1,save_freq) == 0) {
				val current_datetime = new java.util.Date().toString.replaceAll(" ", "_").replaceAll(":", "_")
				val prefix = "checkpoints/" + current_datetime + "_e" + readVar(epoch).makeStr + "_"

				print("  Created checkpoint with prefix \\"")
				print(prefix)
				println("\\"")
				''')

	for i,l in enumerate(net):
		f.write(print_write_weights_to_file_for_layer(i,l,'apps/src/NeuralNetwork/' + name + '/" + prefix + "'))
	f.write("\n")
	for i,l in enumerate(net):
		f.write(print_write_weights_to_file_for_layer(i,l,'apps/src/NeuralNetwork/' + name + '/'))

	f.write('''
			}
		} // Epoch
	}

	''')

def write_train_def(f, net, name, use_feature_maps, first_layer_num_feature_maps, use_dropout, input_image_size):
	write_training_header(f, net, name, use_feature_maps, first_layer_num_feature_maps, input_image_size)
	write_training_loop(f, net, use_dropout, first_layer_num_feature_maps, input_image_size)
	write_training_ending(f, net, name)

# ------------------------------------------------------------------------------
# Writing main def
# ------------------------------------------------------------------------------

def write_main(f, net, first_layer_num_feature_maps, input_image_size, dataset_path):
	expected_num_cols = int(first_layer_num_feature_maps) * int(input_image_size)
	weight_vars = print_weight_vars(net)
	f.write('''

	def main() = {
	''')
	if net.get('lr_cmd_line_arg') == '1':
		f.write('''
		if (args.length < 1) {
			println("\\nError, please specify learning rate as an input argument, or change the XML file\\n")
			exit(0)
		}
		''')
	f.write('''
		val glob_params = readVector[Double]("apps/src/NeuralNetwork/''' + name + '''/global_params.txt", line => line(0).toDouble, ";" )
		val test = glob_params(9).toInt

		if (test == 1) {
			run_test_set(glob_params(10).toInt)
		} 
		else {

			// Read training data	
			val X = readMatrix[Double]("'''+ dataset_path + '''/train_data.txt", s => s.toDouble)
			if ( X.numCols != ''' + str(expected_num_cols) + ''' ) {
				print("\\nError: According to the XML file, train_data.txt should have ")
				print(''' + str(expected_num_cols) + ''')
				println(" columns per row.")
				println("Please update the XML or data to match.")
				exit(1)
			}
			val y = readVector("'''+ dataset_path +'''/train_labels.txt")

			// Also read validation data, used for early-stopping
			val val_X = readMatrix[Double]("''' + dataset_path + '''/val_data.txt", s => s.toDouble)
			if ( val_X.numCols != ''' + str(expected_num_cols) + ''' ) {
				print("\\nError: According to the XML file, val_data.txt should have ")
				print(''' + str(expected_num_cols) + ''')
				println(" columns per row.")
				println("Please update the XML or data to match.")
				exit(1)
			}
			val val_y = readVector("''' + dataset_path + '''/val_labels.txt")

			// Train the network
			train(X, y, val_X, val_y)
		}
	}
''')

def write_test_def(f, net, dataset_path, name, first_layer_num_feature_maps, input_image_size):

	expected_num_cols = int(first_layer_num_feature_maps) * int(input_image_size)

	f.write('''
	def get_cross_entropy_and_classification_error
	(
		// Data''')
	f.write('''
		X: Rep[DenseMatrix[Double]], y: Rep[DenseVector[Double]],''')
	f.write('''
		// Weights''' + print_weight_vars_function_input(net) + ''',

		minib_size: Rep[Int] // Can pass in 1 for online testing
	) = {
		var classification_error = 0.0
		var c_entropy = 0.0 // Cross entropy
		val num_test = y.length
		val num_minibatches = num_test / minib_size
		var idx = 0
		while (idx < num_minibatches) {
			val (''' + print_output_vars(net) + ''') = 
				feed_forward(X.sliceRows(idx*minib_size, (idx+1)*minib_size), ''' + print_weight_vars(net) + ''')
			classification_error += get_classification_error(o''' + str(len(net)-1) + ''', y.slice(idx*minib_size, (idx+1)*minib_size))
			c_entropy += cross_entropy(o''' + str(len(net)-1) + ''', y.slice(idx*minib_size, (idx+1)*minib_size))
			idx += 1
		}
		(c_entropy, classification_error/num_minibatches)
	}

	// Testing
	def run_test_set(batch_size: Rep[Int]) = {

		val X = readMatrix[Double]("''' + dataset_path + '''/test_data.txt", s => s.toDouble)
		if ( X.numCols != ''' + str(expected_num_cols) + ''' ) {
			print("\\nError: According to the XML file, test_data.txt should have ")
			print(''' + str(expected_num_cols) + ''')
			println(" columns per row.")
			println("Please update the XML or data to match.")
			exit(1)
		}
		val y = readVector("''' + dataset_path + '''/test_labels.txt")

		// Load weights''')

	for i,l in enumerate(net):
		if l.attrib['type'] == 'MAX_POOL':
			continue
		f.write('''
		val w''' + str(i) + ''' = readMatrix[Double]("apps/src/NeuralNetwork/''' + name + '''/w''' + str(i) + '''.txt", s => s.toDouble)
		val b''' + str(i) + ''' = readVector("apps/src/NeuralNetwork/''' + name + '''/b''' + str(i) + '''.txt")''')

	f.write('''

		// Test
		val (c_entropy, classification_error) = 
			get_cross_entropy_and_classification_error(X, y, ''' + print_weight_vars(net) + ''', batch_size)
		val num_test = y.length
		print("Test Set Classification Error (")
		print(num_test)
		print(" examples):  ")
		print(classification_error)
		print("%, Cross Entropy (")
		print(num_test)
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
		X: Rep[DenseMatrix[Double]], y: Rep[DenseVector[Double]],''')
	f.write('''
		// Weights''' + print_weight_vars_function_input(net) + ''',''')
	for i,l in enumerate(net):
		type = l.attrib['type']
		if type != 'MAX_POOL':
			f.write('''
		layer''' + str(i) + '''_L2reg: Rep[Double],''')
	f.write('''
		minib_size: Rep[Int] // Can pass in 1 for online testing
	) = {

		val (c_entropy, classification_error) = get_cross_entropy_and_classification_error(X, y, ''' + print_weight_vars(net) + ''', minib_size)
		''')

	f.write('''
		val L2_term = ''')
	for i,l in enumerate(net):
		type = l.attrib['type']
		if type != 'MAX_POOL':
			f.write('''
				w''' + str(i) + '''.map(e => e*e).sum * layer''' + str(i) + '''_L2reg''')
			if i != len(net)-1:
				f.write(' +')
	f.write('''
		c_entropy*(-1.0/X.numRows.toDouble) + ( L2_term / 2 )
	}
	''')

# ------------------------------------------------------------------------------
# Read and parse xml file
# ------------------------------------------------------------------------------
if len(sys.argv) != 2:
	err('Usage: python generate_cnn.py net.xml')
filename = sys.argv[1]
if not os.path.isfile(filename):
	err(filename + ' does not exist')
print "\n" + 'Generating neural network from ' + filename + '...'
tree = ET.parse(filename)
# Get the name of the app
net = tree.getroot()
name = net.get('name')
if not name:
	err('net tag needs a \"name\" attribute')

# ------------------------------------------------------------------------------
# Verify that the network is correct
# ------------------------------------------------------------------------------
verify_net_architecture(net)

# NOTE: For RGB, the input data file still has 1 row per example: It's just
# a row that is 3x as long, and it's stored as R R R R G G G G B B B B
# (for a 2z2 RGB image)

use_feature_maps = False
if net[0].attrib['type'] != 'SOFTMAX' and net[0].attrib['type'] != 'FULLY_CONNECTED':
	# We have at least one convolution/pooling layer, so interpret the input X
	# as a matrix of matrices, vs just a matrix
	# (Note that either way X is stored the same on disk, it's just a matter of 
	# how we interpret it)
	use_feature_maps = True

use_dropout = False
for l in net:
	if l.attrib.get('dropout'):
		use_dropout = True
		break

# Note: Now I assme grasyscale rages from 0-1, and so is RGB
# (i.e. the python to make the data does the scaling. I could also
# also specify the scaling factor in XML like other tools, e.g. 
# http://deeplearning.net/tutorial/lenet.html divides by 256)
first_layer_num_feature_maps = '1'
num_input_channels = net.get('num_input_channels')
if num_input_channels:
	first_layer_num_feature_maps = num_input_channels
elif net.get('colormap') == 'RGB':
	first_layer_num_feature_maps = '3'


# Get the input size
input_image_size = 0
# If this is a convnet, then ensure the input image is square
if use_feature_maps:
	input_img_size_str = net.get('img_size')
	if not input_img_size_str:
		err("Please specify img_size as an attribute of the XML's <net> tag, e.g. img_size=\"32x32\"")
	input_image_dims = input_img_size_str.split('x')
	if not len(input_image_dims) == 2:
		err("Please specify img_size in the XML using the format (e.g.) img_size=\"32x32\"")
	if int(input_image_dims[0]) != int(input_image_dims[1]):
		err('Only square image sizes are currently supported. Please resize the images to make them square.')
	input_image_size = int(input_image_dims[0]) * int(input_image_dims[1])

# This isn't a convnet, so see if the user specified the size a different way
else:
	# First see if they still use img_size. For example this may not be a convnet (e.g. just a
	# softmax), but the inputs are still images. Now however they don't need to be square.
	input_img_size_str = net.get('img_size')
	if input_img_size_str:
		input_image_dims = input_img_size_str.split('x')
		if not len(input_image_dims) == 2:
			err("Please specify img_size in the XML using the format (e.g.) img_size=\"32x32\"")
		input_image_size = int(input_image_dims[0]) * int(input_image_dims[1])

	# No img_size specified, but that's ok since this is not a convolutional network and so
	# the inputs may not be images. But we still need the user to specify the input data size:
	else:
		input_size_str = net.get('input_size')
		if not input_size_str:
			err('''Please specify the input size as an attribute of the XML's <net> tag.

Since this network is not doing colvolution/pooling, input size can be specified in 1 of 2 ways:

1. Using the img_size attribute, e.g.: img_size=\"32x32\"
   (E.g., you can use this if the inputs are images)

2. Using the input_size attribute, e.g.: input_size=\"400\"
   Here 400 is the number of elements of an input vector. 
   (E.g., you can use this if the inputs are not images)

''')
		if 'x' in input_size_str:
			err('input_size should be specified as a single integer')
		input_image_size = int(input_size_str)

# Get the path to the dataset
dataset_path = net.get('dataset_path')
if not dataset_path:
	err('''Please add a dataset_path attribute to the <net> tag,
either as a relative path to /published/OptiML/, or an absolute path.
E.g.: dataset_path="apps/src/NeuralNetwork/examples/mnist/"''')

# ------------------------------------------------------------------------------
# Generate parameter files
# ------------------------------------------------------------------------------

# Create an output directory
if not os.path.exists(name):
    os.makedirs(name)
if not os.path.exists(name + '/checkpoints'):
    os.makedirs(name + '/checkpoints')

generate_parameter_files(name, net)
generate_weight_files(name, net)

# ------------------------------------------------------------------------------
# Generate OptiML file
# ------------------------------------------------------------------------------
optiml_file = open(name + '/' + name + '.scala', 'w')

# Write the file header
write_optiml_header(optiml_file, name, net)

# Write the feed-forward def
num_layers = len(net)
if use_dropout:
	write_fw_header_dropout(optiml_file, net)
	write_fw_layers_dropout(optiml_file, net, net.get('blas')=='1',first_layer_num_feature_maps,input_image_size)
	write_fw_ending(optiml_file, num_layers, net)

write_fw_header(optiml_file, net)
write_fw_layers(optiml_file, net, net.get('blas')=='1',first_layer_num_feature_maps,input_image_size)
write_fw_ending(optiml_file, num_layers, net)

# Write train def (SGD/back-prop)
write_train_def(optiml_file,net,name,use_feature_maps,first_layer_num_feature_maps, use_dropout, input_image_size)

# Write validation def
write_test_def(optiml_file,net,dataset_path, name, first_layer_num_feature_maps, input_image_size)

# Write the main
write_main(optiml_file, net, first_layer_num_feature_maps, input_image_size, dataset_path)

# Close the file
write_optiml_ending(optiml_file, name)
optiml_file.close()

# ------------------------------------------------------------------------------
# Finish
# ------------------------------------------------------------------------------
extra_arg = ''
if net.get('lr_cmd_line_arg') == '1':
	extra_arg = ' <LearningRate, e.g. 0.01>'
print 'Generated ' + name + '/' + name + '''.scala. To train and run the network:
From /published/OptiML,
	sbt compile
	bin/delitec ''' + name + '''Compiler --cuda
	bin/delite ''' + name + '''Compiler --cuda 1''' + extra_arg
example = 'learning rate'
if net.get('lr_cmd_line_arg') == '1':
	example = 'momentum'
print '''
To modify parameters (e.g. ''' + example + ''', # epochs) modify the files:
	''' + name + '''/layer_*_params.txt
	''' + name + '''/global_params.txt

For example you can modify the parameters to do testing instead of training.
	'''
