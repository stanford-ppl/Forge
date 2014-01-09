import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object PageRankCompiler extends OptiGraphApplicationCompiler with PageRank

// This object lets us run the Scala library version of the code
object PageRankInterpreter extends OptiGraphApplicationInterpreter with PageRank

trait PageRank extends OptiGraphApplication {
	def main() = {
		println("OptiGraph Test 1")
		if (args.length < 1) printUsage
		val g = graphFromEdgeList(args(0)) 
		
		println("Directed: " + g.isDirected)
		println("Number of Nodes: " + g.numNodes)
		
		//you can't input 0 as a start node ID
		//there is an issue with directed versus undirected right now
		println("performing Betweeness Centrality")
		tic("PageRank")

    //matches parameters from snap
    //initalize array to 1/numNodes
    val prInit = (NodeData[Double](g.numNodes)).map(e => 1.0/g.numNodes)
    val threshold = 0.0001 
    val damp = 0.85
    val maxItr = 100 

    //FIXME: take out condition on sum, overloading doesn't seem to work
    val pr =
     untilconverged(prInit, tol=threshold,maxIter=maxItr){ oldPr =>
      g.nodes({ n =>
        ((1.0 - damp) / g.numNodes) + damp * sum(g.inNbrs(n)){w => 
          oldPr(w) / g.outDegree(Node(w))}{n =>true} 
        })
    }{(curPr,oldPr) => sum(abs(curPr-oldPr))}
    
		toc("PageRank")
		writeResults("pageRank.txt",g,pr)
	}
	def printUsage = {
    println("Usage: Q1 <path to input edge list file>")
    exit(-1)
  }
}
