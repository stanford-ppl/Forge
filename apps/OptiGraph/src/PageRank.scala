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

    
    val prInit = NodeData[Double](g.numNodes)
    val threshold = 0.0
    val damp = 0.0 

    val pr =
     untilconverged(prInit, tol=threshold,minIter=1,maxIter=1000){  oldPr:Rep[NodeData[Double]] =>
        g.nodes({ n =>
           ((1.0 - damp) / g.numNodes) + damp * sum(g.inNbrs(n),{w => oldPr(w) / 3},{n =>true}) //need method for outDegree
        })
    }{ (curPr:Rep[NodeData[Double]],oldPr:Rep[NodeData[Double]]) => damp-threshold } //need to add in sum and abs
    
		toc("PageRank")
		//writePageRankResults("PageRank.txt",g,pr)
	}
	def printUsage = {
    println("Usage: Q1 <path to input edge list file>")
    exit(-1)
  }
}
