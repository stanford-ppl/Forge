import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object SetTriangleCountingCompiler extends OptiGraphApplicationCompiler with SetTriangleCounting

// This object lets us run the Scala library version of the code
object SetTriangleCountingInterpreter extends OptiGraphApplicationInterpreter with SetTriangleCounting

trait SetTriangleCounting extends OptiGraphApplication {
  def main() = {
    println("SetTriangleCounting")
  
    if (args.length < 1) printUsage
    tic("input",args(0))
    val g = aoaGraphFromEdgeList(args(0))
    toc("input",g)

    println("Number of Nodes: " + g.numNodes)    
    println("performing Traingle Counting")
    tic("Set Triangle Counting",g)

    //Set intersection
    val t1 = g.sumOverNodes{ n =>
      g.intersectSets(n)
    }
    
    toc("Set Triangle Counting",t1)
    println("# Set of triangles: " + t1)
  }
  def printUsage = {
    println("Usage: SetTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
