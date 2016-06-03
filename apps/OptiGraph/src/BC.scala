import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object BCCompiler extends OptiGraphApplicationCompiler with BC

// This object lets us run the Scala library version of the code
object BCInterpreter extends OptiGraphApplicationInterpreter with BC

trait BC extends OptiGraphApplication {
  def main() = {
    println("OptiGraph Test 1")
    if (args.length < 2) printUsage

    tic("input loading")
    val edgeList = loadEdgeList(args(0), "\t")
    toc("input loading",edgeList)

    tic("creating graph",edgeList)
    val g = directedGraphFromEdgeList(edgeList)
    toc("creating graph",g)
    
    println("performing Betweeness Centrality")
    tic(g)
  
    val bc = sum( g.mapNodes( { n =>
      g.inBFOrder(n){ (bfsNode:Rep[Node],sigma:Rep[NodeData[Double]],levelArray:Rep[NodeData[Int]]) =>
        if(bfsNode.id==n.id){1.0}
        else{g.sumUpNeighbors(bfsNode,levelArray){w => sigma(w)}}
      }
      {(rbfsNode:Rep[Node],sigma:Rep[NodeData[Double]],delta:Rep[NodeData[Double]],levelArray:Rep[NodeData[Int]]) => 
        if(levelArray(rbfsNode.id)!=1){ g.sumDownNeighbors(rbfsNode,levelArray){w => 
          (sigma(rbfsNode.id)/ sigma(w))*(1.0+delta(w))}
        }
        else{0.0}
      }
    }))
    
    toc(bc)
    writeResults(args(1),g.getExternalIDs,bc)
    println("done")
  }
  def printUsage = {
    println("Usage: BC <path to input edge list file> <path to output file (to be created)>")
    exit(-1)
  }
}