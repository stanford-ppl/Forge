import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object SnapPageRankCompiler extends OptiGraphApplicationCompiler with SnapPageRank

// This object lets us run the Scala library version of the code
object SnapPageRankInterpreter extends OptiGraphApplicationInterpreter with SnapPageRank

trait SnapPageRank extends OptiGraphApplication {
	def main() = {
		println("SnapPageRank")
	
		if (args.length < 2) printUsage

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
			val tmp = g.mapNodes({ n =>
				  damp * sum(g.inNbrs(n)){w => oldPr(w) / g.outDegree(Node(w))}{n =>true}
				})
			val leaked = (1.0 - sum(tmp)) / g.numNodes
			tmp.map(e => e + leaked)
		}{(curPr,oldPr) => sum(abs(curPr-oldPr))}
		
		toc(pr)
		writeResults(args(1),g.getExternalIDs,pr)
		println("done float")
	}
	def printUsage = {
		println("Usage: SnapPageRank <'directed' or 'undirected'> <path to input edge list file> <path to output file (to be created)>")
		exit(-1)
	}
}
