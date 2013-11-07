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
    val g = loadLineItems("test.txt")
    println("Number of Nodes: " + g.get_num_nodes)
    val n1 = Node(0)
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

    println("performing BFS")
    g.inBFS(n1,n1)

    val n3 = Node(3)
    println("performing BFS")
    g.inBFS(n3,n3)

    val n0 = Node(1)
    println("performing BFS")
    g.inBFS(n0,n0)   
  }
    def fromLine(line: Rep[String]): Rep[Int] = {
        line.toInt
    }
     def loadLineItems(filePath: Rep[String]) = Graph.fromFile(filePath,"test1.txt",fromLine)

}
