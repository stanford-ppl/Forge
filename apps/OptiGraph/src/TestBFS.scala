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
    val g = Graph.fromFile("nodeOffsets.txt","edgeList.txt", fromLine) 
    //val g = Graph.fromFile("n1.txt","e1.txt", fromLine) 
    println("Directed: " + g.is_directed)
    println("Number of Nodes: " + g.get_num_nodes)
    val n1 = g.get_node_from_id(0)
    println("node id1: " + n1.id)
    val n2 = g.get_node_from_id(10)

    println("performing BFS")
    var bc = NodeData[Double](g.get_num_nodes)
  
    bc = g.nodes(
          {(bc_new:Rep[NodeData[Double]],bc_old:Rep[NodeData[Double]]) => bc_old.zip(bc_new)},
          { n =>
            g.inBFS(n,{ (bfs_node:Rep[Node],sigma:Rep[NodeData[Double]],levelArray:Rep[GraphCollection[Int]]) =>
                if(bfs_node.id==n.id){1.0}
                else{
                    sum(g.out_neighbors(bfs_node),{w => sigma(w)},{ e:Rep[Int] => 
                            levelArray(e)==levelArray(bfs_node.id)-1})
                }},
              {(rbfs_node:Rep[Node],sigma:Rep[NodeData[Double]],delta:Rep[NodeData[Double]],levelArray:Rep[GraphCollection[Int]]) => 
                sum(g.out_neighbors(rbfs_node), {w => 
                    (sigma(rbfs_node.id)/ sigma(w))*(1.0+delta(w))},
                    {e => 
                        ((levelArray(e)==levelArray(rbfs_node.id)-1) && (levelArray(rbfs_node.id) != 1))})
            })
        })
    println("bc")
    bc.nd_print
  }
def fromLine(line: Rep[String]): Rep[Int] = {line.toInt}
}
