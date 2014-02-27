import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object HashTriangleCountingCompiler extends OptiGraphApplicationCompiler with HashTriangleCounting

// This object lets us run the Scala library version of the code
object HashTriangleCountingInterpreter extends OptiGraphApplicationInterpreter with HashTriangleCounting

trait HashTriangleCounting extends OptiGraphApplication {
  def main() = {
    println("HashTriangleCounting")
  
    if (args.length < 1) printUsage
    tic("input",args(0))
    //Works for both directed and undirected, performance 
    //val g = undirectedGraphFromDirectedAdjList(args(0),false,args(1).toInt)
    val g = specundirectedGraphFromEdgeList(args(0),true,0)

    toc("input",g)
    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    
    println("performing Traingle Counting")

    tic("Hash Triangle Counting",g)
        //Set intersection
    val t2 = g.sumOverNodes{ n =>
      g.twoLevelHash(n)
    }
    toc("Hash Triangle Counting",t2)
    println("# Hash of triangles: " + t2)
    println("Number of heavy nodes: " + g.numHeavy)
  }
  def printUsage = {
    println("Usage: HashTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
