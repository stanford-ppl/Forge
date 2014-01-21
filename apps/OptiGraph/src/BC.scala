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
		if (args.length < 1) printUsage
		
		val g = graphFromEdgeList(args(0)) 
		
		println("Directed: " + g.isDirected)
		println("Number of Nodes: " + g.numNodes)

		println("performing Betweeness Centrality")
		tic(g)
	
		val bc = sum( g.nodes(
			{ n =>
  			g.inBFOrder(n){ (bfsNode:Rep[Node],sigma:Rep[NodeData[Double]],levelArray:Rep[NodeData[Int]]) =>
    				if(bfsNode.id==n.id){1.0}
    				else{g.sumUpNbrs(bfsNode,levelArray){w => sigma(w)}}
          }
  			{(rbfsNode:Rep[Node],sigma:Rep[NodeData[Double]],delta:Rep[NodeData[Double]],levelArray:Rep[NodeData[Int]]) => 
            if(levelArray(rbfsNode.id)!=1){ g.sumDownNbrs(rbfsNode,levelArray){w => 
            	(sigma(rbfsNode.id)/ sigma(w))*(1.0+delta(w))}
          	}
          	else{0.0}
          }
		  }
    ))
		
		toc(bc)
		writeResults("bc.txt",g,bc)
		println("done")
	}
	def printUsage = {
    println("Usage: Q1 <path to input edge list file>")
    exit(-1)
  }
}