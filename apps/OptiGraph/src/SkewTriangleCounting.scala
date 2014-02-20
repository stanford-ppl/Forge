import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object SkewTriangleCountingCompiler extends OptiGraphApplicationCompiler with SkewTriangleCounting

// This object lets us run the Scala library version of the code
object SkewTriangleCountingInterpreter extends OptiGraphApplicationInterpreter with SkewTriangleCounting

trait SkewTriangleCounting extends OptiGraphApplication {
  def main() = {
    println("SkewTriangleCounting")
  
    if (args.length < 2) printUsage
    tic("input",args(0))
    //Works for both directed and undirected, performance 
    //val g = undirectedGraphFromDirectedAdjList(args(0),true,args(1).toInt)
    val g = undirectedGraphFromEdgeList(args(0),true,0)

    toc("input",g)
    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    
    println("performing Traingle Counting")
    tic("Triangle Counting",g)
    
    val t = g.sumOverNodes{ n =>
      //could probably clean up syntax to move hash inside DSL
      if(g.isHeavy(n)){
        val nbrHash = g.neighborHash(n)
        g.sumOverEdges{ e =>
            if(nbrHash.hasEdgeWith(e.fromNode.id) && nbrHash.hasEdgeWith(e.toNode.id)) 1.toLong
            else 0.toLong
        }/2
      }
      else{
        /*
        val nbrHash = g.neighborHash(n)
        g.sumOverNbrs(n){ nbr =>
          g.sumOverNbrs(nbr){ nbrOfNbr =>
            if(nbrHash.hasEdgeWith(nbr)) 1.toLong
            else 0.toLong
          }{nbrOfNbr => nbrOfNbr>nbr}
        }{nbr => nbr > n.id}
        */
        g.sumOverNbrs(n){ nbr =>
          val nbrHash = g.neighborHash(Node(nbr))
          g.sumOverNbrs(n){ nbrClone =>
            if(nbrHash.hasEdgeWith(nbrClone)) 1.toLong
            else 0.toLong
          }{nbrClone => nbrClone > nbr}
        }{e => true}

      }
    }
    toc("Triangle Counting",t)
    println("Number of triangles " + t/3)
  }
  def printUsage = {
    println("Usage: SkewTriangleCounting <path to input edge list file> <split # for heavy>")
    exit(-1)
  }
}
