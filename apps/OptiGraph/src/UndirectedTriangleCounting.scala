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
    val g = csrPrunedUndirectedGraphFromEdgeList(args(0))
    
    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    
    println("performing Traingle Counting")
    tic(g)
    
    val t = g.sumOverNodes{ n =>
      val nbrs = g.neighbors(n)
      sumOverCollection(nbrs){ nbr =>
        nbrs.intersect(g.neighbors(nbr))
      }{e => true}
    }

    toc(t)
    println("Number of trianges " + t)
  }
  def printUsage = {
    println("Usage: UndirectedTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
