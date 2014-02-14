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

    //Works for both directed and undirected, performance 
    val g = undirectedGraphFromEdgeList(args(0),true)

    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    
    println("performing Traingle Counting")
    tic(g)
    
    val t = g.mapNodes{ n =>
      val nbrHash = g.neighborHash(n)
      g.neighbors(n).mapreduce[Int]({ nbr =>
        g.neighbors(nbr).mapreduce[Int]({ nbrOfNbr =>
          if(nbrHash.hasEdgeWith(nbrOfNbr)) 1
          else 0
        },{(a,b) => a+b},{nbrOfNbr => nbrOfNbr>nbr})
      },{(a,b) => a+b},{nbr => nbr > n.id})
    }.reduce{(a,b) => a+b}

    toc(t)
    println("Number of trianges " + t)
  }
  def printUsage = {
    println("Usage: UndirectedTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
