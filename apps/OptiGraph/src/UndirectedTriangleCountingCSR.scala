import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object UndirectedTriangleCountingCSRCompiler extends OptiGraphApplicationCompiler with UndirectedTriangleCountingCSR

// This object lets us run the Scala library version of the code
object UndirectedTriangleCountingCSRInterpreter extends OptiGraphApplicationInterpreter with UndirectedTriangleCountingCSR

trait UndirectedTriangleCountingCSR extends OptiGraphApplication {
  def main() = {
    if (args.length < 2) printUsage

    val nodesArray = ForgeFileReader.readLines(args(0)){ _.toInt }
    val edgesArray = ForgeFileReader.readLines(args(1)){ _.toInt }
    val g = undirectedGraphFromCSR(nodesArray, edgesArray)
    tic(g)
    
    val t = sumOverNodes(g.nodes){ n =>
      sumOverNeighbors(g.neighbors(n)){ nbr =>
        if(nbr > n) g.commonNeighbors(n,nbr)
        else 0l
      }
    }

    toc(t)
    println("Number of triangles " + t)
  }
  def printUsage = {
    println("Usage: UndirectedTriangleCounting <path to nodes> <path to edges>")
    exit(-1)
  }
}
