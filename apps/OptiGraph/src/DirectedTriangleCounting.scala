import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object DirectedTriangleCountingCompiler extends OptiGraphApplicationCompiler with DirectedTriangleCounting

// This object lets us run the Scala library version of the code
object DirectedTriangleCountingInterpreter extends OptiGraphApplicationInterpreter with DirectedTriangleCounting

trait DirectedTriangleCounting extends OptiGraphApplication {
  def main() = {
    println("DirectedTriangleCounting (counts through triangles)")
  
    if (args.length < 1) printUsage
    tic("input",args(0))
    //Works for both directed and undirected, performance 
    val g = directedGraphFromEdgeList(args(0))
    
    toc("input",g)
    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    
    println("performing Traingle Counting")
    tic("Triangle Counting",g)
    
    val t = g.mapNodes{ n =>
      val inHash = g.inNeighborHash(n)
      g.outNbrs(n).mapreduce[Int]({ nbr =>
        g.outNbrs(nbr).mapreduce[Int]({ nbrOfNbr =>
          if(inHash.hasEdgeWith(nbrOfNbr)) 1
          else 0
        },{(a,b) => a+b},{e => true})
      },{(a,b) => a+b},{e => true})
    }.reduce{(a,b) => a+b}

    toc("Triangle Counting", t)
    println("Number through triangles (x3) : " + t)
  }
  def printUsage = {
    println("Usage: DirectedTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}
