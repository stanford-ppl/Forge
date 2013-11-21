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
    println("Directed: " + g.is_directed())
    println("Number of Nodes: " + g.get_num_nodes)
    val n1 = g.get_node_from_id(9)
    println("node id1: " + n1())
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
    //val nd = g.inBFS(n1,nodeComputation)
    
    val nd = g.inBFS(n1,{
        (node:Rep[Node],nodeData:Rep[NodeData[Int]],levelArray:Rep[GraphCollection[Int]]) => 
            //println("asdf n: " + node() + " base: " + n1())
            if(node()==n1()){1}
            else{
                g.sum(g.out_neighbors(node),nodeData,{
                        e => levelArray(e)==levelArray(node())-1
                    })
            }
    })

    nd.nd_print

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
