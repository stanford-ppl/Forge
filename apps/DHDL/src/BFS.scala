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
  lazy val maxNumEdgeX2 = 16.as[Index]

  // Limitation:
  def main() {
    //val NV = args(unit(0)).to[SInt]
    ////genRandDirEdgeList("/Users/Yaqi/Documents/hyperdsl/published/DHDL/graph.dot", NV, NE, true)
    val maps = loadDirEdgeList("/Users/Yaqi/Documents/hyperdsl/forge/apps/DHDL/graph/testBFS.dot", true)
    val map = maps(0)
    val vl = getVertList(map, false, true) // NV by 2 Array(pointer, numEdges)
    val el = getEdgeList(map) // Flat edge list
    val NV = vl.length/2 // Actual number of vertices in graph
    val NE = el.length

    println("vl:" + vl.mkString(","))
    println("el:" + el.mkString(","))

    val vertList = OffChipMem[Index]("VertList", NV, 2) // [pointer, size]
    val edgeList = OffChipMem[Index]("EdgeList", NE * 2) // srcs of edges
    val count = ArgOut[Index]("count")

    setMem(vertList, vl)
    setMem(edgeList, el)

    Accel {
      //Pipe.fold(NV by tileSize)(count){ ivt =>
      //  val vB = BRAM[Index]("vertTile", tileSize, 2)
      //  vB := vertList(ivt::ivt+tileSize, 0::2)
      //  val sumTile = Reg[Index]("sumTile")
      //  Pipe.fold(tileSize by 1)(sumTile){ iv =>
      //    val eB = BRAM[Index]("edgeTile", maxNumEdge) // edge list of v
      //    val nvIdxB = BRAM[Index]("neighborVertIdxTile", maxNumEdgeX2) // Index to gather vertice list of neighbors of v
      //    val nvB = BRAM[Index]("neighborVertTile", maxNumEdgeX2) // vertice list of v's neighbors
      //    val vpt = Reg[Index]("vpt") // ptr to v's edgelist
      //    val vsize = Reg[Index]("vsize") // number of edges of v
      //    Pipe {
      //      vpt := vB(iv,0)
      //      vsize := vB(iv,1)
      //    }
      //    eB := edgeList(vpt::vpt+vsize)
      //    Pipe (vsize by 1) { ie =>
      //      val nbr = eB(ie)
      //      nvIdxB(ie*2) = nbr*2
      //      nvIdxB(ie*2+1) = nbr*2+1
      //    }
      //    nvB := vertList(nvIdxB)
      //    val sumNbr = Reg[Index]("sumNbr")
      //    Pipe.fold(vsize by 1)(sumNbr){ ie =>
      //      val neB = BRAM[Index]("neighborEdgeTile", maxNumEdgeX2) // edgeList of v's neighbors
      //      val nbrpt = Reg[Index]("nbrpt") // ptr to neighbor's edgelist
      //      val nbrsize = Reg[Index]("nbrsize") // number of edges of neighbor
      //      val cntCommon = Reg[Index]("cntCommon")
      //      Pipe {
      //        nbrpt := nvB(ie*2)
      //        nbrsize := nvB(ie*2+1)
      //      }
      //      neB := edgeList(nbrpt::nbrpt+nbrsize)
      //      Pipe.reduce(vsize by 1, nbrsize by 1)(cntCommon){ (ixv, ixn) =>
      //        mux(eB(ixv)==neB(ixn), 1.as[Index], 0.as[Index])
      //      }{_+_} // Sum for one neighbor
      //    }{_+_} // Sum across all node's neighbors
      //  }{_+_} // Sum across a tile of nodes
      //}{_+_} // Sum across all tiles
      Pipe {val a = 3}
      ()
    }

    val result = getArg(count) / 6

    /* OptiGraph Version */
    //val t = sumOverNodes(g.nodes){ n =>
    //  sumOverNeighbors(g.neighbors(n)){ nbr =>
    //    if(nbr > n) g.commonNeighbors(n,nbr)
    //    else 0l
    //  }
    //}

    /* Scala Version */
    
    val dists = Array.tabulate(NV) { iv => -1.as[SInt] }
    dists(0) = 0

    val diameter = 10.as[SInt]

    for (level <- 0 until diameter) {
      println("level: " + level)
      for (iv <- 0 until NV) {
        val pt = vl(iv * 2)
        val size = vl(iv * 2 + 1)
        println("iv: " + iv)
        if (dists(iv)==level) {
          for (ie <- 0 until size) {
            val e = el(pt + ie)
            dists(e) = level + 1
            println("ie: " + ie + " e:" + e)
            println("dists(e):" + dists(e))
            println("dists:" + dists.mkString(","))
          }
        }
      }
    }
    
    println("dists:" + dists.mkString(","))

    //val gold = Array.tabulate(NV){ iv=>
    //  val pt = vl(iv*2)
    //  val size = vl(iv*2+1)
    //  Array.tabulate(size){ i =>
    //    val ie = pt + i
    //    val nbr = el(ie)
    //    //val cm = if (nbr > iv) commonNeighbors(iv, nbr, vl, el)
    //    //         else 0.as[Index]
    //    val cm = commonNeighbors(iv, nbr, vl, el)
    //    cm
    //  }.reduce{_+_}
    //}.reduce{_+_} / 6.as[Index]

    //println("expected: " + gold)
    //println("result: " + result)
    //assert(result == gold)

  }
}
