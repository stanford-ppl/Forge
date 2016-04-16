import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object PageRankCompiler extends OptiGraphApplicationCompiler with PageRank

// This object lets us run the Scala library version of the code
object PageRankInterpreter extends OptiGraphApplicationInterpreter with PageRank

trait PageRank extends OptiGraphApplication {
  def main() = {
    println("PageRank")
  
    if (args.length < 2) printUsage

    tic("input loading")
    val edgeList = loadEdgeList(args(0), "\t")
    toc("input loading",edgeList)

    tic("creating graph",edgeList)
    val g = directedGraphFromEdgeList(edgeList)
    toc("creating graph",g)

    println("performing Page Rank")
    tic(g)

    //matches parameters from snap
    //initalize array to 1/numNodes
    val prInit = NodeData.fromFunction[Double](g.numNodes,{e => 1.0/g.numNodes})
    val threshold = 0.01 
    val damp = 0.15
    val maxItr = if(args.length == 3) args(2).toInt else 100
    
    val pr =
     untilconverged(prInit, tol=threshold,maxIter=maxItr){ oldPr =>
      g.mapNodes{ n =>
        damp + (1.0-damp)*sumOverNeighbors(g.inNeighbors(n)){ w =>
          oldPr(w) / g.outDegree(w)}
      }
    }{(curPr,oldPr) => sum(abs(curPr-oldPr))}
    
    toc(pr)
    writeResults(args(1),g.getExternalIDs,pr)
    println("wrote output to: " + args(1))
  }
  def printUsage = {
    println("Usage: PageRank <path to input edge list file> <path to output file (to be created)> <OPTIONAL: max # of iterations>")
    exit(-1)
  }
}
