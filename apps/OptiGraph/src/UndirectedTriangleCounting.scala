import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object UndirectedTriangleCountingCompiler extends OptiGraphApplicationCompiler with UndirectedTriangleCounting

// This object lets us run the Scala library version of the code
object UndirectedTriangleCountingInterpreter extends OptiGraphApplicationInterpreter with UndirectedTriangleCounting

trait UndirectedTriangleCounting extends OptiGraphApplication {
  def main() = {
    println("UndirectedTriangleCounting")
  
    if (args.length < 1) printUsage
    tic("input",args(0))
    //Works for both directed and undirected, performance 
    val g = undirectedGraphFromEdgeList(args(0),true)

    toc("input",g)
    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    
    println("performing Traingle Counting")
    tic("Tcounting",g)
    
    val t = g.mapNodes{ n =>
      val nbrHash = g.neighborHash(n)
      g.neighbors(n).mapreduce[Int]({ smallNbr =>
        g.neighbors(n).mapreduce[Int]({ bigNbr =>
          if(nbrHash.hasEdgeWith(smallNbr)) 1
          else 0
        },{(a,b) => a+b},{bigNbr => bigNbr>smallNbr})
      },{(a,b) => a+b},{smallNbr => smallNbr > n.id})
    }.reduce{(a,b) => a+b}

    toc("Tcounting",t)
    println("Number of triangles " + t)
  }
  def printUsage = {
    println("Usage: UndirectedTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
