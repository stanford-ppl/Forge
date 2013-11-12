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
    println("Number of Nodes: " + g.get_num_nodes)
    val n1 = g.get_node_from_id(9)
    println("node id1: " + n1())
    //val n2 = g.get_node_from_id(10)

    /*ß
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
    val nd = g.inBFS(n1,nodeComputation)
    nd.pprint

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
def nodeComputation(node: Rep[Node], nd: Rep[NodeData[Int]]) : Rep[Int] = {nd(node())+5}
}
