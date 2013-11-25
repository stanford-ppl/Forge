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

    /*
    val collection = g.node_neighbors(4)
    val len = collection.length()
    println("printing edge stuff4")
    println("edge length: " + collection.length())
    collection.pprint

    collection = g.node_neighbors(0)
    len = collection.length()
    println("printing edge stuff0")
    println("edge length: " + collection.length())
    collection.pprint

    collection = g.node_neighbors(5)
    len = collection.length()
    println("printing edge stuff5")
    println("edge length: " + collection.length())
    collection.pprint

    collection = g.node_neighbors(9)
    len = collection.length()
    println("printing edge stuff")
    println("edge length: " + collection.length())
    collection.pprint
*/
    println("performing BFS")
    var bc = NodeData[Double](g.get_num_nodes)
    //var bcD = NodeData[NodeData[Double]](g.get_num_nodes)
    //val nd = g.inBFS(n1,nodeComputation)
    (g.nodes).foreach{n =>
        bc = bc.zip(
         //bcD(n) =
          g.inBFS(Node(n),{  
              (node:Rep[Node],nodeData:Rep[NodeData[Double]],levelArray:Rep[GraphCollection[Int]]) => 
                //println("asdf n: " + node() + " base: " + n1())
                if(node.id==n){1.0}
                else{
                    // sum(upNeighbors)
                    // sum(n.upNeighbors(levelArray))
                    g.sum(g.out_neighbors(node),nodeData,{
                            (e:Rep[Int]) => levelArray(e)==levelArray(node.id)-1
                        })
                }
            },{
            (node:Rep[Node],sigma:Rep[NodeData[Double]],delta:Rep[NodeData[Double]],levelArray:Rep[GraphCollection[Int]]) => 
                (g.out_neighbors(node)).mapreduce[Double]( {w => 
                                     //println("map : s(" + w +")= " + sigma(w) + " s("+node()+")= " + sigma(node()) + " delta(w)=" + delta(w))
                                     ( ( sigma(node.id)/ sigma(w) ) * (1.0+delta(w)) )
                                    },
                                { (a,b) => a+b}, 
                                { e => ( (levelArray(e)==levelArray(node.id)-1) && (levelArray(node.id) != 1) ) })
            })
            )//bc = bc.zip(delta)
        
    }

    println("bc")
    bc.nd_print

    //bc = bcD.hashreduce[NodeData,Double](e => e, e => e*10, (a,b) => a+b)
    //println("")
    //println("hash filter reduce")
    //bc.nd_print

    /*
    val n3 = Node(3)
    println("performing BFS")
    g.inBFS(n3)

    val n0 = Node(1)
    println("performing BFS")
    g.inBFS(n0)  
    */ 
  }
def fromLine(line: Rep[String]): Rep[Int] = {line.toInt}
//def nodeComputation(node: Rep[Node], nodeData: Rep[NodeData[Float]], levelArray:Rep[GraphCollection[Int]], level:Rep[Float]) : Rep[Int] = {
//    g.sum( g.out_neighbors(node),nodeData ){ levelArray==level }
//}
//def nodeComputation(node: Rep[Node], nd: Rep[NodeData[Int]], gc:Rep[GraphCollection[Int]], level:Rep[Int]) : Rep[Int] = {nd(node())+5}
}
