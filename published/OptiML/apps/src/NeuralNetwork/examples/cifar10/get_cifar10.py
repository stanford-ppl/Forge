import cPickle, gzip
import numpy as np
from subprocess import call
import os


# Load the dataset
# http://www.cs.utoronto.ca/~kriz/cifar.html
print "\nDownloading CIFAR-10 dataset from utoronto.ca...\n"
call(['wget', 'http://www.cs.utoronto.ca/~kriz/cifar-10-python.tar.gz'])
call(['tar', 'xvfz', 'cifar-10-python.tar.gz'])
os.system('mv cifar-10-batches-py/* .')
call(['rmdir', 'cifar-10-batches-py'])

print "Creating train_data.txt and train_labels.txt..."
for i in range(4):

	flag = 'a'
	if i==0:
		flag = 'w'

	print 'Writing data_batch_' + str(i+1) + '...'
	f = open('data_batch_' + str(i+1), 'rb')
	d = cPickle.load(f)
	f.close()
	
	f = open("train_data.txt", flag)
	np.savetxt(f, d['data']/256.0, delimiter='\t', fmt='%f')
	f.close
	
	f = open("train_labels.txt", flag)
	f.write("\n".join(str(l) for l in d['labels']))
	f.write("\n")
	f.close

print "Creating val_data.txt and val_labels.txt..."
print 'Writing data_batch_5...'
f = open('data_batch_5', 'rb')
d = cPickle.load(f)
f.close()

f = open("val_data.txt", "w")
np.savetxt(f, d['data']/256.0, delimiter='\t', fmt='%f')
f.close

f = open("val_labels.txt", "w")
f.write("\n".join(str(l) for l in d['labels']))
f.write("\n")
f.close

print "Creating test_data.txt and test_labels.txt..."
print 'Writing test_batch...'
f = open('test_batch', 'rb')
d = cPickle.load(f)
f.close()

f = open("test_data.txt", "w")
np.savetxt(f, d['data']/256.0, delimiter='\t', fmt='%f')
f.close

f = open("test_labels.txt", "w")
f.write("\n".join(str(l) for l in d['labels']))
f.write("\n")
f.close

print("Done\n")
