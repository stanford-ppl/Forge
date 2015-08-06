# Visualize convolution weights written to file by the network,
# or more generally visualize any Grayscale image.
#
# Usage:
#
# 1. Put weights into weights.txt. Format is 1 number per line.
# E.g. for a 2x2 Grayscale image:
#
# If the image is:
#	Px0	Px1
# 	Px2	Px3
#
# Weights.txt should contain 12 numbers, 1 per line (this is the
# same format that the convolution weights are written to file).
# (Intensities are between 0 and 1)
#
#	Px0
#	Px1
#	Px2
#	Px3
#
# 2. Run the script and display the image:
# >>> python visualize_weights_grayscale.py
# >>> display outfile.png

import numpy
import scipy.misc
import math

mat = numpy.loadtxt('weights.txt')
side = math.sqrt(mat.shape[0])
img = numpy.reshape(mat, (side,side))
scipy.misc.imsave('outfile.png', img)
