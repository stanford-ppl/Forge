import numpy
import scipy.misc
'''
l = numpy.empty([12])
l[0 ] = 0
l[1 ] = 1
l[2 ] = 2
l[3 ] = 3
l[4 ] = 4
l[5 ] = 5
l[6 ] = 6
l[7 ] = 7
l[8 ] = 8
l[9 ] = 9
l[10] = 10
l[11] = 11
print(numpy.reshape(l, (3,2,2)))
'''
mat = numpy.loadtxt('train_data.txt') * 256
row1 = mat[23,:] # Pick some image, e.g. #23
img = numpy.reshape(row1, (3,32,32))
scipy.misc.imsave('outfile.png', img)
