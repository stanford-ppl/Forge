#! /usr/bin/python

import argparse
import random
import numpy

def main():
  parser = argparse.ArgumentParser(
    description="Generate a random linear program in a format that can be parsed by OptiML.")
  parser.add_argument('n', type=int, help="problem variable size")
  parser.add_argument('m', type=int, help="inequality constraint size")
  parser.add_argument('p', type=int, help="equality constraint size")
  args = parser.parse_args()

  # initialize c, g, and a
  c = numpy.random.randn(args.n, 1)
  g = numpy.random.randn(args.m, args.n)
  a = numpy.random.randn(args.p, args.n)
  # pick a feasible point
  x = numpy.random.randn(args.n, 1)
  # initialize b so that x is feasible
  b = numpy.dot(a, x)
  # initialize h so that x is feasible
  h = numpy.dot(g, x) - numpy.random.rand(args.m, 1)


  makeMatrixDataFile("c.dat", c)
  makeMatrixDataFile("g.dat", g)
  makeMatrixDataFile("h.dat", h)
  makeMatrixDataFile("a.dat", a)
  makeMatrixDataFile("b.dat", b)

  makeMatrixDataFile("x0.dat", numpy.zeros((args.n, 1)))
  makeMatrixDataFile("y0.dat", numpy.ones((args.p, 1)))
  makeMatrixDataFile("s0.dat", numpy.zeros((args.m, 1)))
  makeMatrixDataFile("z0.dat", numpy.ones((args.m, 1)))


def makeMatrixDataFile(name, A):
  with open(name, "w") as f:
    for i in range(A.shape[0]):
      print >>f, " ".join([str(x) for x in A[i,:]])

if __name__ == "__main__":
  main()

