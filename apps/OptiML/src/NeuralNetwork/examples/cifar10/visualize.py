import numpy
import scipy.misc
import sys

# Get the image number to display
if len(sys.argv) < 2:
	print 'Specify the training image number to show as an input argument'
	print 'Requires that train_data.txt be generated.'
	print 'Example:  >>> python visualize.py 100'
	sys.exit(0)
img_num = int(sys.argv[1])

# Read the image from file
print 'Loading training_data.txt...'
train_data_file = open('train_data.txt')
img_str = ''
for i,l in enumerate(train_data_file):
	if (i-1) == img_num:
		img_str = l
		break

# Write the image to a file
img_1D = numpy.fromstring(img_str, dtype=float, sep="\t") * 256
img = numpy.reshape(img_1D, (3,32,32))
name = 'img_' + str(img_num) + '.png'
scipy.misc.imsave(name, img)
print 'Saved ' + name 
