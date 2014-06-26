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
    val g = undirectedGraphFromEdgeList(edgeList)
    toc("creating graph",g)
    
    val k = 0.01
    val result = CommunityDetection(g.getNodes,g.getEdges,k)

  }
  def CommunityDetection(nodes:Rep[ForgeArray[Int]], edges:Rep[ForgeArray[Int]], k:Rep[Double]) : Tuple2[Rep[ForgeArray[Int]],Rep[ForgeArray[Int]]] = {
    var g = undirectedGraphFromCSR(nodes,edges)

    println("performing Community Detection")
    tic(g)
    
    val precision = k
    var c = Community(g,precision)
    var mod,newMod = c.modularity

    println("Modularity: " + mod)
    
    var level = 0
    var improvement = true
    while(improvement){
      ///////////////////PHASE 1/////////////////////////////////////
      //Move nodes to different communities until we converge
      c = c.louvain //heart of the algorithm
      newMod = c.storedModularity //to avoid recomputation, store it
      improvement = c.canImprove //are we done yet?
      ///////////////////////////////////////////////////////////////
      
      ///////////////////PHASE 2/////////////////////////////////////
      //Generate a new graph from community structure
      //Generate a new comm structure from graph so we can go again
      g = c.generateNewGraph
      c = Community(g,precision)  //to be used in the next round
      ///////////////////////////////////////////////////////////////

      //////////////////////DEBUG/////////////////////////////////////
      println("Level: " + level + " Modularity improved from: " + mod + " to: " + newMod)
      println("\tnumNodes: " + g.numNodes + " numEdges: " + g.numEdges + " weight: " + g.totalWeight)
      level += 1
      mod = newMod
      ////////////////////////////////////////////////////////////////
    }
    toc(mod)

    (g.getNodes,g.getEdges)
  }
  def printUsage = {
    println("Usage: CommunityDetection <path to input edge list file>")
    exit(-1)
  }
}