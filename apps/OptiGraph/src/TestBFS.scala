import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object TestBFSCompiler extends OptiGraphApplicationCompiler with TestBFS

// This object lets us run the Scala library version of the code
object TestBFSInterpreter extends OptiGraphApplicationInterpreter with TestBFS

trait TestBFS extends OptiGraphApplication {
	def main() = {
	println("OptiGraph Test 1")
	if (args.length < 1) printUsage
	val g = Graph.fromFile(args(0)) 
	//val g = Graph.fromFile("n1.txt","e1.txt", fromLine) 
	
	println("Directed: " + g.is_directed)
	println("Number of Nodes: " + g.get_num_nodes)
	
	//you can't input 0 as a start node ID
	//there is an issue with directed versus undirected right now
	
	//val n1 = g.get_node_from_id(3)
	//println("n1 ID: " + n1.id)

	println("performing Betweeness Centrality")
	var bc = NodeData[Double](g.get_num_nodes)
	//needs to be fixed so that in_neighbors and out_neighbors are hidden.  Should be
	//up neighbors and down neighbors. external code needs to be generated.
	bc = g.nodes(
		{(bc_new:Rep[NodeData[Double]],bc_old:Rep[NodeData[Double]]) => bc_old.zip(bc_new)},
		{ n =>
		g.inBFS(n,{ (bfs_node:Rep[Node],sigma:Rep[NodeData[Double]],levelArray:Rep[GraphCollection[Int]]) =>
			if(bfs_node.id==n.id){1.0}
			else{
				sum(g.in_neighbors(bfs_node),{w => sigma(w)},{ e:Rep[Int] => 
					levelArray(e)==levelArray(bfs_node.id)-1})
			}},
			{(rbfs_node:Rep[Node],sigma:Rep[NodeData[Double]],delta:Rep[NodeData[Double]],levelArray:Rep[GraphCollection[Int]]) => 
				sum(g.out_neighbors(rbfs_node), {w => 
						(sigma(rbfs_node.id)/ sigma(w))*(1.0+delta(w)) },
					{e => 
						(( levelArray(e)==(levelArray(rbfs_node.id)+1) ) && (levelArray(rbfs_node.id) != 1))})
		})
	})
	println("bc")
	bc.nd_print

	}
	def printUsage = {
    println("Usage: Q1 <input edge list file>")
    exit(-1)
  }
}
