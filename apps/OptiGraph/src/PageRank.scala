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
    val g = directedGraphFromEdgeList(args(0))
    
    println("Directed: " + g.isDirected)
    println("Number of Nodes: " + g.numNodes)
    
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
      g.nodes{ n =>
        ((1.0 - damp) / g.numNodes) + damp * sum(g.inNbrs(n)){ w =>
          oldPr(w) / g.outDegree(Node(w))}{n => true}
      }
    }{(curPr,oldPr) => sum(abs(curPr-oldPr))}
    
    toc(pr)
    writeResults(args(1),g.getExternalIDs,pr)
    println("wrote output to: " + args(1))
  }
  def printUsage = {
    println("Usage: BC <path to input edge list file> <path to output file (to be created)>")
    exit(-1)
  }
}
