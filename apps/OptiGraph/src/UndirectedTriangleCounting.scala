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
    val g = undirectedGraphFromEdgeList(args(0),true,0)
    //val g = undirectedGraphFromDirectedAdjList(args(0),true,args(1).toInt)

    toc("input",g)
    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    
    println("performing Traingle Counting")
    tic("Triangle Counting",g)
    
    val t = g.sumOverNodes{ n =>
      //could probably clean up syntax to move hash inside DSL
      val nbrHash = g.neighborHash(n)
      g.sumOverNbrs(n){ nbr =>
        g.sumOverNbrs(nbr){ nbrOfNbr =>
          if(nbrHash.hasEdgeWith(nbr)) 1.toLong
          else 0.toLong
        }{nbrOfNbr => nbrOfNbr>nbr}
      }{nbr => nbr > n.id}
    }

    toc("Triangle Counting",t)
    println("Number of triangles " + t)
  }
  def printUsage = {
    println("Usage: UndirectedTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
