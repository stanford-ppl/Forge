import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object BitSetTriangleCountingCompiler extends OptiGraphApplicationCompiler with BitSetTriangleCounting

// This object lets us run the Scala library version of the code
object BitSetTriangleCountingInterpreter extends OptiGraphApplicationInterpreter with BitSetTriangleCounting

trait BitSetTriangleCounting extends OptiGraphApplication {
  def main() = {
    println("BitSetTriangleCounting")
  
    if (args.length < 1) printUsage
    tic("total",args(0))
    //Works for both directed and undirected, performance 
    val t = countOverEdges(args(0))
    println("Num triangles: " + t)
    toc("total",t)
  }
  def printUsage = {
    println("Usage: BitSetTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
