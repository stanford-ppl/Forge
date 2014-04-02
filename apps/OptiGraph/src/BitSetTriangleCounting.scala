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
    //tic("input",args(0))
    //Works for both directed and undirected, performance 
    //val g = undirectedGraphFromDirectedAdjList(args(0),false,args(1).toInt)
    val g = bitSetGraphFromEdgeList(args(0))
    println("Input finished.  NumNodes: " + g.numNodes)
    
    tic("Count Time",g)
    val count = g.countTriangles
    toc("Count Time",count)
    println("Count: " + count)
  }
  def printUsage = {
    println("Usage: BitSetTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
