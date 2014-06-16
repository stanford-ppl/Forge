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

    //Works for both directed and undirected, performance 
    tic("input loading")
    val edgeList = loadDirectedEdgeList(args(0))
    toc("input loading",edgeList)

    tic("creating graph",edgeList)
    val g = directedGraphFromEdgeList(edgeList)
    toc("creating graph",g)

    println("performing Page Rank")
    tic(g)

    //matches parameters from snap
    //initalize array to 1/numNodes
    val prInit = NodeData.fromFunction[Double](g.numNodes,{e => 1.0/g.numNodes})
    val threshold = 0.0001 
    val damp = 0.85
    val maxItr = 100
    
    val pr =
     untilconverged(prInit, tol=threshold,maxIter=maxItr){ oldPr =>
      g.mapNodes{ n =>
        ((1.0 - damp) / g.numNodes) + damp * sum(g.inNbrs(n)){ w =>
          oldPr(w) / g.outDegree(Node(w))}{n => true}
      }
    }{(curPr,oldPr) => sum(abs(curPr-oldPr))}
    
    toc(pr)
    writeResults(args(1),g.getExternalIDs,pr)
    println("wrote output to: " + args(1))
  }
  def printUsage = {
    println("Usage: PageRank <path to input edge list file> <path to output file (to be created)>")
    exit(-1)
  }
}
