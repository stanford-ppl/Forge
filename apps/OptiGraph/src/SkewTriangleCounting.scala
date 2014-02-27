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
  
    if (args.length < 1) printUsage
    tic("input",args(0))
    //Works for both directed and undirected, performance 
    //val g = undirectedGraphFromDirectedAdjList(args(0),false,args(1).toInt)
    val g = specundirectedGraphFromEdgeList(args(0),true,0)

    toc("input",g)
    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    
    println("performing Traingle Counting")

    /* Heavy / Light
    val t = g.sumOverNodes{ n =>
      if(true){//g.isHeavy(n)){
        g.intersectSets(n)
      }
      else{
        g.sumOverNbrs(n){ nbr =>
          g.sumOverNbrs(n){ nbrClone =>
            if(g.hasEdge(nbrClone,nbr)) 1
            else 0
          }{nbrClone => nbr>nbrClone}
        }{nbr => true}
      }
    }
    */
    tic("Set Triangle Counting",g)

    //Set intersection
    val t1 = g.sumOverNodes{ n =>
      g.intersectSets(n)
    }
    
    toc("Set Triangle Counting",t1)
    println("# Set of triangles: " + t1)

    tic("Hash Triangle Counting",t1)
        //Set intersection
    val t2 = g.sumOverNodes{ n =>
      g.twoLevelHash(n)
    }
    toc("Hash Triangle Counting",t2)
    println("# Hash of triangles: " + t2)
    println("Number of heavy nodes: " + g.numHeavy)
  }
  def printUsage = {
    println("Usage: SkewTriangleCounting <path to input edge list file>")
    exit(-1)
  }
}