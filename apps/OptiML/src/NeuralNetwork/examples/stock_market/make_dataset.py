#  
#  Stanford Pervasive Parallelism Laboratory
#  http://ppl.stanford.edu/
#
# Recurrent Neural Network example
#
# Make up some stock market data, and determine if we should buy, sell, or do nothing
# at each time interval.
#
# This dataset can be used either with a classifier output layer (e.g. softmax) or a 
# regression output layer (e.g. to predict the future price of the stock)
#
# Usage:
# >>> python make_dataset.py
#
# To also display the data,
# >>> python make_dataset.py show_data plot_data
#


import numpy as np
import random
from subprocess import call
import sys

num_intervals = 5
interval_size = 400
window_size = 25

# Generate "num_intervals" noisy sine waves, each with "interval_size" points, and concatenate them
def get_random_stock():
	stock_price = []
	last_endpoint = 0.0
	pi = 3.14
	for s in range(num_intervals):
		# Pick a frequency and amplitude for the sine wave
		freq = random.random()/2
		amplitude = random.random()*100 # Cents
		phase = random.random()*2*pi
		data = np.sin(np.linspace(phase, phase + 2*pi*freq, num=interval_size))*amplitude
		data = data - data[0] + last_endpoint
		# Multiply the sine by a line
		bias_slope = (random.random()-0.5)*5
		biases = np.linspace(0.0, 5.0, num=interval_size)*bias_slope
		# Also add some noise
		data = data + biases + np.random.randn(interval_size)
		last_endpoint = data[-1] # for continuity
		stock_price = np.append(stock_price, data)

	return stock_price


# Create labels for each point: 
#   0 - sell a lot
#   1 - sell a bit
#   2 - do nothing
#   3 - buy a bit
#   4 - buy a lot
# These are determined by looking forwards in time, which
# the network can't do (not using a bidirectional net for
# this experiment, since this would be cheating)
# 
# In this example the stock prices are based on 
# concatenated sin waves.
# The net needs to therefore look at the previous steps
# as well as the current step to get a sense for the 
# "concavity", to determine if the stock is going to
# continue growing or flatten.
def get_action(diff):
	action = 2
	if (diff > 10):
		action = 4
	elif (diff > 2):
		action = 3
	elif (diff < -10):
		action = 0
	elif (diff < -2):
		action = 1
	return action

def get_labels(stock):
	labels = []
	for i in range(num_intervals * interval_size / window_size ):
		# Look into the future one window and average the 
		# last half of the window
		avg_period = window_size/4
		future_lookahead = 2*window_size-1
		future_idx = min(i*window_size+future_lookahead, len(stock)-1)
		future_mean = np.mean(stock[future_idx-avg_period : future_idx])
		# Also look at the closing mean for this window
		present_idx = i*window_size+window_size-1
		present_mean = np.mean(stock[present_idx-avg_period : present_idx])
		# Use the diff of the 2 to determine the action
		diff = future_mean - present_mean
		labels.append(get_action(diff))
	return labels

# Slight preprocessing for the net, rather than feed the net the prices
# directly, feed the net the change in price (e.g. instead of a stock with
# price [$1, $2, $4, $3, $8] across 5 days, feed the net [0,+1,+2,-1,+5])
def fluctuations(l):
	new_l = []
	for i in range(len(l)):
		if i==0:
			new_l.append(0.0)
		else:
			new_l.append(l[i]-l[i-1])
	return new_l

# Make stocks for training
num_stocks = 1000
data_f = open("train_data.txt", "w")
labels_f = open("train_labels.txt", "w")
for s in range(num_stocks):
	train_data = get_random_stock()
	train_labels = get_labels(train_data)
	#print("\n\nTraining:")
	#for i in range(len(train_labels)/2):
	#	print str((i+1)*window_size) + ': ' + str(train_labels[i]) + '\t\t\t' + \
	#		str(((i+1) + len(train_labels)/2) * window_size) + ': ' + str(train_labels[i + len(train_labels)/2])
	#print train_labels
	#plt.plot(train_data)
	#plt.show()
	np.savetxt(data_f, fluctuations(train_data), delimiter='\t', newline='\t', fmt='%f')
	np.savetxt(labels_f, train_labels, delimiter='\t', newline='\t', fmt='%d')
	data_f.write("\n")
	labels_f.write("\n")
data_f.close()
labels_f.close()

# Validation
val_data = get_random_stock()
val_labels = get_labels(val_data)

if len(sys.argv) > 1 and sys.argv[1] == 'show_data':
	print("\n\nValidation data:")
	for i in range(len(val_labels)/2):
		print str((i+1)*window_size) + ': ' + str(val_labels[i]) + '\t\t\t' + \
			str(((i+1) + len(val_labels)/2) * window_size) + ': ' + str(val_labels[i + len(val_labels)/2])
	print val_labels

if len(sys.argv) > 2 and sys.argv[2] == 'plot_data':
	import matplotlib.pyplot as plt
	plt.plot(val_data)
	plt.show()

data_f = open("val_data.txt", "w")
labels_f = open("val_labels.txt", "w")
np.savetxt(data_f, fluctuations(val_data), delimiter='\t', newline='\t', fmt='%f')
np.savetxt(labels_f, val_labels, delimiter='\t', newline='\t', fmt='%d')
data_f.close()
labels_f.close()


