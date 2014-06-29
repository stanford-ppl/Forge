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

    tic("input loading")
    val edgeList = loadUndirectedEdgeList(args(0))
    toc("input loading",edgeList)

    tic("creating graph",edgeList)
    val g = undirectedGraphFromEdgeList(edgeList)
    toc("creating graph",g)
    
    println("performing Traingle Counting")
    tic(g)
    
    val t = sumOverNodes(g.nodes){ n =>
      sumOverNbrs(g.neighbors(n)){ nbr =>
        if(nbr > n) g.commonNeighbors(n,nbr)
        else 0l
      }
    }

    toc(t)
    println("Number of triangles " + t)
  }
  def printUsage = {
    println("Usage: UndirectedTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
