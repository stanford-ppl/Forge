import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

object CommunityDetectionJar extends OptiGraphApplicationCompiler with CommDetection {
  registerFunction(CommunityDetection _)  
  override def functionName = "CommunityDetection"
}

object CommunityDetectionCompiler extends OptiGraphApplicationCompiler with CommDetection
object CommunityDetectionInterpreter extends OptiGraphApplicationInterpreter with CommDetection

trait CommDetection extends OptiGraphApplication {
  def main() = {
    println("CommunityDetection")
  
    if (args.length < 2) printUsage

    tic("input loading")
    val edgeList = loadEdgeList(args(0), "\t")
    toc("input loading",edgeList)

    tic("creating graph",edgeList)
    val g = undirectedGraphFromEdgeList(edgeList)
    toc("creating graph",g)
    
    val k = args(1).toDouble
    val result = CommunityDetection(g.getCSRNodes,g.getCSREdges,k)

    val newNumNodes = array_length(result._1)
    println(newNumNodes)
    var i = 0
    while(i < array_length(result._3)){
      if(array_apply(result._3,i) < 0 || array_apply(result._3,i) >= newNumNodes)
        println("ERROR i: " + i + " data: " + array_apply(result._3,i))
      i += 1
    }
  }
  def CommunityDetection(nodes:Rep[ForgeArray[Int]], edges:Rep[ForgeArray[Int]], k:Rep[Double]) : Rep[Tup3[ForgeArray[Int],ForgeArray[Int],ForgeArray[Int]]] = {
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
      val result = c.generateNewGraph
      g = result._1
      val n2o = result._2
      c = Community(g,precision,n2o)  //to be used in the next round
      ///////////////////////////////////////////////////////////////

      //////////////////////DEBUG/////////////////////////////////////
      println("Level: " + level + " Modularity improved from: " + mod + " to: " + newMod)
      println("\tnumNodes: " + g.numNodes + " numEdges: " + g.numEdges + " weight: " + g.totalWeight)
      //printGraph(path,level,mod,newMod,g.numNodes,g.numEdges,g.getCSRNodes,g.getCSREdges,g.getCSREdgeWeights)
      level += 1
      mod = newMod
      ////////////////////////////////////////////////////////////////
    }
    toc(mod)

    pack(g.getCSRNodes,g.getCSREdges,c.n2o)
  }
  def printUsage = {
    println("Usage: CommunityDetection <path to input edge list file> <convergance factor>")
    exit(-1)
  }
}
