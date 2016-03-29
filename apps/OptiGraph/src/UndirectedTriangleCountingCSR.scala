import optigraph.compiler._
import optigraph.library._
import optigraph.shared._


object UndirectedTriangleCountingCSRCompiler extends OptiGraphApplicationCompiler with UndirectedTriangleCountingCSR
object UndirectedTriangleCountingCSRInterpreter extends OptiGraphApplicationInterpreter with UndirectedTriangleCountingCSR

trait UndirectedTriangleCountingCSR extends OptiGraphApplication {
  def main() = {
    if (args.length < 2) printUsage

    val nodesArray = ForgeFileReader.readLines(args(0)){ _.toInt }
    val nl = array_length(nodesArray)
    println("nodes: " + nl)

    val edgesArray = ForgeFileReader.readLinesFlattened(args(1)){ s => s.split("\\s+").map(_.toInt) }
    val el = array_length(edgesArray)
    println("edges: " + el)

    val g = undirectedGraphFromCSR(nodesArray, edgesArray)
    tic(nl,el)
    
    val t = sumOverNodes(g.nodes){ n =>
      sumOverNeighbors(g.neighbors(n)){ nbr =>
        if(nbr > n) g.numCommonNeighbors(n,nbr)
        else 0l
      }
    }

    toc(t)
    println("Number of triangles: " + t)
  }
  def printUsage = {
    println("Usage: UndirectedTriangleCounting <path to nodes> <path to edges>")
    exit(-1)
  }
}
