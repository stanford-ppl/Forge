import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object Skew2TriangleCountingCompiler extends OptiGraphApplicationCompiler with Skew2TriangleCounting

// This object lets us run the Scala library version of the code
object Skew2TriangleCountingInterpreter extends OptiGraphApplicationInterpreter with Skew2TriangleCounting

trait Skew2TriangleCounting extends OptiGraphApplication {
  def main() = {
    println("Skew2TriangleCounting")
  
    if (args.length < 2) printUsage
    tic("input",args(0))
    //Works for both directed and undirected, performance 
    //val g = undirectedGraphFromDirectedAdjList(args(0),false,args(1).toInt)
    val g = undirectedGraphFromEdgeList(args(0),true,0)

    toc("input",g)
    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    
    println("performing Traingle Counting")
    tic("Triangle Counting",g)
    
    val t = g.sumOverNodes{ n =>
      val nbrHash = g.neighborHash(n)
      if(g.isHeavy(n)){
        g.sumOverEdges{ e =>
          if(nbrHash.hasEdgeWith(e.fromNode.id) && nbrHash.hasEdgeWith(e.toNode.id)
            && e.fromNode.id < e.toNode.id){ 
            1
          }
          else 0
        }
      }
      else{
        g.sumOverNbrs(n){ nbr =>
          g.sumOverNbrs(nbr){ nbrOfNbr =>
            if(nbrHash.hasEdgeWith(nbrOfNbr)) 1
            else 0
          }{nbrOfNbr => nbrOfNbr > nbr}
        }{e => true}
      }
    }
    toc("Triangle Counting",t)
    println("Number of triangles: " + t)
    println("Number of heavy nodes: " + g.numHeavy)
  }
  def printUsage = {
    println("Usage: Skew2TriangleCounting <path to input edge list file> <split # for heavy>")
    exit(-1)
  }
}
