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
		
		/*
		graphFromEdgeList(args(0))
		//val g = graphFromEdgeList(args(0)) 
		
		println("Directed: " + g.isDirected)
		println("Number of Nodes: " + g.numNodes)
 
		//you can't input 0 as a start node ID
		//patch up undirected versus directed
		//rbfs flag?
		//val n = g.getNodeFromID(78)
		println("performing Betweeness Centrality")
		tic(g)
	*/

    //FIXME: throws an error if you take out explicit type, why?

    //val nd = NodeData(array_buffer_result(map))
    //nd.print
    //val nd = NodeData[Int](10)
    //val to = nd.map(e => nd)
		//nd.print
    /*
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
		*/
		//toc(bc)
		//writeResults("bc.txt",g,bc)
		println("done")
	}
	def printUsage = {
    println("Usage: Q1 <path to input edge list file>")
    exit(-1)
  }
}