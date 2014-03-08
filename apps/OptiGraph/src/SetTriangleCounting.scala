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
    //Works for both directed and undirected, performance 
    //val g = undirectedGraphFromDirectedAdjList(args(0),false,args(1).toInt)
    //val t = countOverEdges(args(0))
    //println("Num triangles: " + t)
    //toc("total",t)
    
    val g = aoaGraphFromEdgeList(args(0))

    toc("input",g)
    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    
    println("performing Traingle Counting")

    tic("Set Triangle Counting",g)

    //Set intersection
    val t1 = g.sumOverNodes{ n =>
      g.intersectHybrid(n)
    }
    
    toc("Set Triangle Counting",t1)
    println("# Set of triangles: " + t1)
  }
  def printUsage = {
    println("Usage: SetTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
