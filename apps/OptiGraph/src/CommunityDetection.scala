import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object CommunityDetectionCompiler extends OptiGraphApplicationCompiler with CommunityDetection

// This object lets us run the Scala library version of the code
object CommunityDetectionInterpreter extends OptiGraphApplicationInterpreter with CommunityDetection

trait CommunityDetection extends OptiGraphApplication {
  def main() = {
    println("CommunityDetection")
  
    if (args.length < 1) printUsage

    tic("input loading")
    val edgeList = loadUndirectedEdgeList(args(0))
    toc("input loading",edgeList)

    tic("creating graph",edgeList)
    var g = undirectedGraphFromEdgeList(edgeList)
    toc("creating graph",g)
    
    println("performing Community Detection")
    tic(g)
    
    var c = Community(g)
    var mod = c.modularity

    println("Modularity: " + mod)
    
    var level = 0
    var improvement = true
    while(improvement){ //can be an until converged.
      //Move nodes to different communities until we converge
      c = c.louvain //heart of the algorithm
      val newMod = c.storedModularity
      improvement = c.canImprove
      //Generate a new graph from community structure
      //Generate a new comm structure from graph so we can go again
      g = c.generateNewGraph
      c = Community(g)

      //purely for debug
      println("Level: " + level + " Modularity improved from: " + mod + " to: " + newMod)
      println("\tnumNodes: " + g.numNodes + " numEdges: " + g.numEdges + " weight: " + g.totalWeight)

      //Keep track of some stats between iterations
      level += 1
      mod = newMod
    }
    toc(mod)
  }
  def printUsage = {
    println("Usage: CommunityDetection <path to input edge list file>")
    exit(-1)
  }
}