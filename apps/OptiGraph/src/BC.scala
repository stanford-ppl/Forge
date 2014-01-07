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
		
		//you can't input 0 as a start node ID
		//there is an issue with directed versus undirected right now
		println("performing Betweeness Centrality")
		tic("bc")
    //FIXME: throws an error if you take out explicit type, why?
		val bc = sum( g.nodes(
			{ n =>
  			g.inBFOrder(n,{ (bfs_node:Rep[Node],sigma:Rep[NodeData[Double]],levelArray:Rep[NodeData[Int]]) =>
      				if(bfs_node.id==n.id){1.0}
      				else{g.sumUpNbrs(bfs_node,levelArray,{w => sigma(w)})}
          },
  				{(rbfs_node:Rep[Node],sigma:Rep[NodeData[Double]],delta:Rep[NodeData[Double]],levelArray:Rep[NodeData[Int]]) => 
              g.sumDownNbrs(rbfs_node,levelArray, {w => (sigma(rbfs_node.id)/ sigma(w))*(1.0+delta(w)) })
          })
		  }
    ))

		toc("bc")
		writeBCResults("bc.txt",g,bc)
	}
	def printUsage = {
    println("Usage: Q1 <path to input edge list file>")
    exit(-1)
  }
}
