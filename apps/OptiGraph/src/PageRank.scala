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
	
		if (args.length < 1) printUsage
		val g = graphFromEdgeList(args(0))
		
		println("Directed: " + g.isDirected)
		println("Number of Nodes: " + g.numNodes)
		
		//you can't input 0 as a start node ID
		//there is an issue with directed versus undirected right now
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
			val tmp = g.nodes({ n =>
				  damp * sum(g.inNbrs(n)){w => oldPr(w) / g.outDegree(Node(w))}{n =>true}
				})
			val leaked = (1.0 - sum(tmp)) / g.numNodes
			tmp.map(e => e + leaked)
		}{(curPr,oldPr) => sum(abs(curPr-oldPr))}
		
		toc(pr)
		writeResults("pageRank.txt",g,pr)
		println("done float")
	}
	def printUsage = {
		println("Usage: Q1 <path to input edge list file> <delimeter in edgelist>")
		exit(-1)
	}
}
