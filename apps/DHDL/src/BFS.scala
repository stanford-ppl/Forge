import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object BFSCompiler extends DHDLApplicationCompiler with BFS
object BFSInterpreter extends DHDLApplicationInterpreter with BFS
trait BFS extends DHDLApplication {
  override def stageArgNames = List("tileSize")
  lazy val tileSize = param("tileSize", 2)
  lazy val maxNumEdge = 8.as[Index]
  lazy val diameter = ArgIn[SInt]("diameter")
  lazy val numVert = ArgIn[SInt]("numVert")

  // Limitation:
  // Not using bitMask due to filter is not supported
  def main() {
    //val NV = args(unit(0)).to[SInt]
    ////genRandDirEdgeList("/Users/Yaqi/Documents/hyperdsl/published/DHDL/graph.dot", NV, NE, true)
    val maps = loadDirEdgeList("/Users/Yaqi/Documents/hyperdsl/forge/apps/DHDL/graph/testBFS.dot", true)
    val map = maps(0)
    val vl = getVertList(map, false, true) // NV by 2 Array(pointer, numEdges)
    val el = getEdgeList(map) // Flat edge list
    val NV = vl.length/2 // Actual number of vertices in graph
    val NE = el.length
    val DM = 10.as[SInt] // Diameter

    println("vl:" + vl.mkString(","))
    println("el:" + el.mkString(","))

    val vertList = OffChipMem[Index]("VertList", NV, 2) // [pointer, size]
    val edgeList = OffChipMem[Index]("EdgeList", NE) // srcs of edges
    val distList = OffChipMem[Index]("DistList", NV) // srcs of edges

    // Initialize dist to -1 except the source
    val gold = Array.empty[SInt](NV)
    for (i <- 0 until NV) {
      gold(i) = -1.as[SInt]
    }
    gold(0) = 0.as[SInt]

    setMem(vertList, vl)
    setMem(edgeList, el)
    setMem(distList, gold)

    setArg(diameter, DM)
    setArg(numVert, NV)

    Accel {
      //val bitMap = BRAM[Bit]("bitMap", NV) // Assume reset to 0
      Sequential (diameter.value by 1) { level =>
        Pipe (numVert.value by tileSize) { ivt =>
          val vB = BRAM[Index]("vB", tileSize, 2) 
          val distB = BRAM[Index]("distB", tileSize) 
          vB := vertList(ivt::ivt+tileSize, 0::2)
          distB := distList(ivt::ivt+tileSize)
          Pipe (tileSize by 1) {iv =>
            val eB = BRAM[Index]("edgeTile", maxNumEdge) // edge list of v
            val eDistB = BRAM[Index]("edgeDistTile", maxNumEdge) // dists of edge list of v
            val vpt = Reg[Index]("vpt") // ptr to v's edgelist
            val vsize = Reg[Index]("vsize") // number of edges of v
            Pipe {
              vpt := vB(iv,0)
            //  vsize := vB(iv,1)
            }
            Parallel { 
            //  //TODO: doesn't support using function of register value as range of tile load
              eB := edgeList(vpt.value::vpt.value+tileSize)
              //eB := edgeList(vpt.value::vpt.value+vsize.value)
            //  //eDistB := distList(eB, vpt.value+vsize.value)
            }
            //Pipe { //TODO: has to wrap the innerpipe inside another pipe to make use of vsize not in parent of Pipe where it's created
            //  Pipe (vsize.value by 1) { ie =>
            //    val nbr = eB(ie)
            //    val isCurLevel = (distB(iv) == level)
            //    val isNbrNotSet = (distB(nbr) < 0.as[SInt])
            //    eDistB(ie) = mux(isCurLevel && isNbrNotSet, level+1, eDistB(ie))
            //  }
            //}
            //distList(eB) := eDistB
          }
        }
      }
      ()
    }

    /* Scala Version */

    for (level <- 0 until DM) {
      println("level: " + level)
      for (iv <- 0 until NV) {
        val pt = vl(iv * 2)
        val size = vl(iv * 2 + 1)
        //println("iv: " + iv)
        if (gold(iv)==level) {
          for (ie <- 0 until size) {
            val e = el(pt + ie)
            if (gold(e)>0)
              gold(e) = level + 1
            //println("ie: " + ie + " e:" + e)
            //println("gold(e):" + gold(e))
            //println("gold:" + gold.mkString(","))
          }
        }
      }
    }
    
    println("expected: " + gold.mkString(","))
    //println("result: " + result)
    //assert(result == gold)

  }
}
