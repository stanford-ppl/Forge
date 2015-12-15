import cPickle, gzip
from subprocess import call

# Load the dataset
# http://deeplearning.net/tutorial/gettingstarted.html
print "Downloading MNIST dataset from deeplearning.net...\n"
call(['wget', 'http://deeplearning.net/data/mnist/mnist.pkl.gz'])
f = gzip.open('mnist.pkl.gz', 'rb')
train_set, valid_set, test_set = cPickle.load(f)
f.close()

# Write to files
# Format: each set above is 2 lists: a list of images
# and a list of labels. Each image is a 1D vector,
# of length (28x28)=784. Write as a tsv for OptiML.

# TODO: Use join instead to write to file as string

# Training set
print "Creating train_data.txt..."
f = open("train_data.txt", "w")
for img in train_set[0]:
	cache = ""
	for i in img:
		cache += (str(i) + "\t")
	f.write(cache+"\n")
f.close
print "Creating train_labels.txt..."
f = open("train_labels.txt", "w")
for label in train_set[1]:
	f.write(str(label)+"\n")
f.close

# Val set
print "Creating val_data.txt..."
f = open("val_data.txt", "w")
for img in valid_set[0]:
	cache = ""
	for i in img:
		cache += (str(i) + "\t")
	f.write(cache+"\n")
f.close
print "Creating val_labels.txt..."
f = open("val_labels.txt", "w")
for label in valid_set[1]:
	f.write(str(label)+"\n")
f.close

# Test set
print "Creating test_data.txt..."
f = open("test_data.txt", "w")
for img in test_set[0]:
	cache = ""
	for i in img:
		cache += (str(i) + "\t")
	f.write(cache+"\n")
f.close
print "Creating test_labels.txt..."
f = open("test_labels.txt", "w")
for label in test_set[1]:
	f.write(str(label)+"\n")
f.close

print "Done!\n"
