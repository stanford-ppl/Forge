#! /usr/bin/python
from __future__ import division

import argparse
import numpy
import scipy
import scipy.sparse
import scipy.sparse.linalg
import math

def main():
  parser = argparse.ArgumentParser(
    description="Generate a random sparse COO matrix in a format that can be parsed by OptiML.")
  parser.add_argument('n', type=int, help="matrix size")
  parser.add_argument('r', type=int, help="matrix rank")
  parser.add_argument('m', type=int, help="search rank")
  parser.add_argument('lmin', type=float, help="matrix min eval")
  parser.add_argument('lmax', type=float, help="matrix max eval")
  args = parser.parse_args()

  # initialize c
  c = scipy.sparse.rand(args.n, args.n, 0.05)
  c = (c + c.T)/2
  (l, e) = scipy.sparse.linalg.eigsh(c, args.r+1, which="LA")
  l0 = (l[0] + l[1])/2;
  c = c - l0 * scipy.sparse.eye(args.n)
  c = c.tocoo()
  # lp = args.lmin + (args.lmax - args.lmin) * numpy.random.rand(args.r)
  # ln = -(args.lmin + (args.lmax - args.lmin) * numpy.random.rand(args.n-args.r))
  # l = numpy.concatenate((lp, ln))
  # c = scipy.sparse.diags((l,), (0,), format='coo')

  # initialize soln
  (lc, ec) = scipy.sparse.linalg.eigsh(c, args.r, which="LA")
  ec = numpy.matrix(ec)
  soln = numpy.array(ec * numpy.diag(lc) * ec.T)
  # soln = numpy.diag(numpy.concatenate((lp, numpy.zeros(args.n - args.r))))

  # initialize v0
  v0 = numpy.random.randn(args.n, args.m);
  v0 = v0 / numpy.linalg.norm(v0, 2)
  # v0 = numpy.concatenate((numpy.eye(args.m), numpy.zeros((args.n - args.m, args.m)))) / math.sqrt(args.m)

  with open("c.dat", "w") as f:
    for (i,j,v) in zip(c.row, c.col, c.data):
      if(i <= j):
        print >>f, str(i) + " " + str(j) + " " + str(v)
      elif(i == j):
        print >>f, str(i) + " " + str(j) + " " + str(v/2)

  with open("v0.dat", "w") as f:
    for i in range(v0.shape[0]):
      print >>f, " ".join([str(x) for x in v0[i,:]])

  with open("soln.dat", "w") as f:
    for i in range(soln.shape[0]):
      print >>f, " ".join([str(x) for x in soln[i,:]])

  print numpy.linalg.norm(soln, 'fro')
  print numpy.linalg.norm(numpy.dot(v0,v0.T), 'fro')

if __name__ == "__main__":
  main()

