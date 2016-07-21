import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TriangleCountingCompiler extends DHDLApplicationCompiler with TriangleCounting
object TriangleCountingInterpreter extends DHDLApplicationInterpreter with TriangleCounting
trait TriangleCounting extends DHDLApplication {

  lazy val tileSize = param(2)
  lazy val maxNumEdge = 8.as[Index]
  lazy val maxNumEdgeX2 = 16.as[Index]

  // Limitation:
  //  1. Number of vertices must be a multiple of tileSize
  //  2. Assume number of edges of 1 vertex fits on chip
  //  3. Intersection in DHDL of two sets is O(M*N) where M and N are sizes of two sets
  def main() {
    //val NV = args(0).to[SInt]
    ////genRandDirEdgeList("/Users/Yaqi/Documents/hyperdsl/published/DHDL/graph.dot", NV, NE, true)
    val map = loadUnDirEdgeList("/Users/Yaqi/Documents/hyperdsl/forge/apps/DHDL/graph/testTc2.dot", true)
    val vl = getVertList(map, false, true) // NV by 2 Array(pointer, numEdges)
    val el = getEdgeList(map) // Flat edge list
    val NV = vl.length/2 // Actual number of vertices in graph
    val NE = el.length/2 // Divided by 2 b/c bi-directional edge

    val vertList = OffChipMem[Index](NV, 2) // [pointer, size]
    val edgeList = OffChipMem[Index](NE*2) // srcs of edges
    val count = ArgOut[Index]

    setMem(vertList, vl)
    setMem(edgeList, el)

    Accel {
      Pipe.fold(NV by tileSize)(count){ ivt =>
        val vB = BRAM[Index](tileSize, 2)
        vB := vertList(ivt::ivt+tileSize, 0::2)
        val sumTile = Reg[Index]
        Pipe.fold(tileSize by 1)(sumTile){ iv =>
          val eB = BRAM[Index](maxNumEdge)       // edge list of v
          val nvIdxB = BRAM[Index](maxNumEdgeX2) // Index to gather vertice list of neighbors of v
          val nvB = BRAM[Index](maxNumEdgeX2)    // vertice list of v's neighbors
          val vpt = Reg[Index]                   // ptr to v's edgelist
          val vsize = Reg[Index]                 // number of edges of v
          Pipe {
            vpt := vB(iv,0)
            vsize := vB(iv,1)
          }
          eB := edgeList(vpt::vpt+vsize)
          Pipe (vsize by 1) { ie =>
            val nbr = eB(ie)
            nvIdxB(ie*2) = nbr*2
            nvIdxB(ie*2+1) = nbr*2+1
          }
          nvB := vertList(nvIdxB)
          val sumNbr = Reg[Index]
          Pipe.fold(vsize by 1)(sumNbr){ ie =>
            val neB = BRAM[Index](maxNumEdgeX2)   // edgeList of v's neighbors
            val nbrpt = Reg[Index]                // ptr to neighbor's edgelist
            val nbrsize = Reg[Index]              // number of edges of neighbor
            val cntCommon = Reg[Index]
            Pipe {
              nbrpt := nvB(ie*2)
              nbrsize := nvB(ie*2+1)
            }
            neB := edgeList(nbrpt::nbrpt+nbrsize)
            Pipe.reduce(vsize by 1, nbrsize by 1)(cntCommon){ (ixv, ixn) =>
              mux(eB(ixv)==neB(ixn), 1.as[Index], 0.as[Index])
            }{_+_} // Sum for one neighbor
          }{_+_} // Sum across all node's neighbors
        }{_+_} // Sum across a tile of nodes
      }{_+_} // Sum across all tiles
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
    def commonNeighbors(n1:Rep[Index], n2:Rep[Index], vl:Rep[ForgeArray[Index]], el:Rep[ForgeArray[Index]]):Rep[Index] = {
      val n1pt = vl(n1*2)
      val n1size = vl(n1*2+1)
      val n2pt = vl(n2*2)
      val n2size = vl(n2*2+1)
      if (n1size==0.as[Index] || n2size==0.as[Index]) {
        0.as[Index]
      } else {
        var count = 0.as[Index]
        var n1cur = n1pt
        var n2cur = n2pt
        //Can use a while loop here but try to be consistent with DHDL
        for (i <- 0 until (n1size + n2size)) {
          if (n1cur < (n1pt+n1size) && n2cur < n2pt + n2size) {
            if (el(n1cur) < el(n2cur)) {
                n1cur += 1.as[Index]
            } else if (el(n1cur) > el(n2cur)) {
              if (n2cur < (n2pt+n2size))
                n2cur +=1.as[Index]
            } else { //n1cur == n2cur
              count +=1.as[Index]
                n1cur += 1.as[Index]
            }
          }
        }
        count
      }
    }
    val gold = Array.tabulate(NV){ iv=>
      val pt = vl(iv*2)
      val size = vl(iv*2+1)
      Array.tabulate(size){ i =>
        val ie = pt + i
        val nbr = el(ie)
        //val cm = if (nbr > iv) commonNeighbors(iv, nbr, vl, el)
        //         else 0.as[Index]
        val cm = commonNeighbors(iv, nbr, vl, el)
        cm
      }.reduce{_+_}
    }.reduce{_+_} / 6.as[Index]

    println("expected: " + gold)
    println("result: " + result)
    assert(result == gold)

  }
}
