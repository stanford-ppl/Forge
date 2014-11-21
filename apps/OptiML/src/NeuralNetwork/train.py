#  
#  Stanford Pervasive Parallelism Laboratory
#  http://ppl.stanford.edu/
#

import subprocess
import sys
import time
from subprocess import call
import os.path

if len(sys.argv) != 3:
	print 'Usage: python train.py APP_DIR_NAME PATH_TO_APP'
	print 'Example:'
	print '>>> python train.py cifar /home/user/hyperdsl/published/OptiML/apps/src/NeuralNetwork/'
	exit(0)

name = sys.argv[1]
path_to_app = sys.argv[2]
lr = 0.01
train_err_history = []
val_err_history = []
HISTORY = 4

path = path_to_app + "/" + name + "/"
val_file = open(path + "val.txt", "a", 0)
train_file = open(path + "train.txt", "a", 0)
log_file = open(path + "log.txt", "a", 0)

checkpoint_frequency = 5
checkpoint_counter = checkpoint_frequency

while True:

	# Run a new experiment
	print "bin/delite " + name + "Compiler --cuda 1 " + str(lr)
	output = subprocess.check_output(["bin/delite", name + "Compiler", "--cuda", "1", str(lr)])
	#print output
	lines = output.split('\n')
	print lines[-5]
	print lines[-4]
	log_file.write(lines[-5])
	log_file.write(lines[-4])
	train_err = float((((lines[-5]).split(':')[1]).split('%')[0]).strip())
	val_err   = float((((lines[-4]).split(':')[1]).split('%')[0]).strip())

	train_err_history.append(train_err)
	val_err_history.append(val_err)

	#print("Training error: " + str(train_err))
	#print("Training error: " + str(val_err))

	val_file.write(str(val_err) + "\n")
	train_file.write(str(train_err) + "\n")

	if checkpoint_counter == 0:
		#checkpoint_counter = checkpoint_frequency
		#now = (time.strftime("%c")).replace (" ", "_")
		#checkpoint_path = path + 'CHECKPOINT_' + now
		#if not os.path.exists(checkpoint_path):
		#    os.makedirs(checkpoint_path)
		#call(["cp", path + "b*txt", checkpoint_path + '/'])
		#call(["cp", path + "db*txt", checkpoint_path + '/'])
		#call(["cp", path + "w*txt", checkpoint_path + '/'])
		#call(["cp", path + "dw*txt", checkpoint_path + '/'])
		#print 'Created checkpoint in: ' + checkpoint_path
		#log_file.write('Created checkpoint in: ' + checkpoint_path)
	else:
		checkpoint_counter -= 1

	# E.g. train error is 34 and best of last few was 34.2
	if len(val_err_history) >= HISTORY:
		if (max(val_err_history) - min(val_err_history)) < 0.2:
			lr = lr / 10
			val_err_history = []
			train_err_history = []
			if lr < 0.00001:
				exit(0)
	if len(val_err_history) > HISTORY:
		train_err_history.pop(0)
		val_err_history.pop(0)

val_file.close()
train_file.close()
log_file.close()
