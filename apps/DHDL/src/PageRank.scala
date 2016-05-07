import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object PageRankCompiler extends DHDLApplicationCompiler with PageRank
object PageRankInterpreter extends DHDLApplicationInterpreter with PageRank
trait PageRank extends DHDLApplication {
  type Elem = Flt //FixPt[Signed, B16, B16]

  override def stageArgNames = List("tileSize")
  lazy val tileSize = param("tileSize", 2)
  lazy val maxNumEdge = param("maxNumEdge", 60)
  lazy val dataSize = ArgIn[SInt]("dataSize")

  def main() {
    /*
    println("expected: " + gold.mkString)
    println("result: " + result.mkString)
    assert(result == gold)
    */
    //val edges = loadDirEdgeList("/Users/Yaqi/Documents/hyperdsl/published/DHDL/data.txt", unit(false))
    //println(edges.map{case (s,d) => s + "->" + d}.mkString(","))
    val NV = args(unit(0)).to[SInt]
    val NE = args(unit(1)).to[SInt]
    genRandDirEdgeList("/Users/Yaqi/Documents/hyperdsl/published/DHDL/graph.dot", fix_to_int(NV), fix_to_int(NE), true)
    val veArr = loadDirEdgeList("/Users/Yaqi/Documents/hyperdsl/published/DHDL/graph.dot", fix_to_int(NV), true)
    //val NV = veArr(0).length/3 // Actual number of vertices in graph
    //val NE = veArr(1).length // Actual number of vertices in graph
    val vertList = OffChipMem[Index]("VertList", NV, 2)
    val edgeList = OffChipMem[Index]("EdgeList", NE, 2)
    val pageRank = OffChipMem[Elem]("PageRank", NV, 2)
    val sVertList = veArr(1).map(i => int_to_fix[Signed,B32](i)) 
    val sEdgeList = veArr(1).map(i => int_to_fix[Signed,B32](i))
    setMem(vertList, sVertList)
    setMem(edgeList, sEdgeList)
    println("vertList: " + getMem(vertList).mkString(","))
    println("edgeList: " + getMem(edgeList).mkString(","))

    val init = Array.tabulate(NV*2) { i => 1.as[Elem]/NV.to[Elem] }
    setMem(pageRank, init)
    println("initial page rank: " + getMem(pageRank).mkString(","))
    Accel {
      /*
      Sequential(NumIter by 1) { iter =>
        val oldPrIdx = iter % 2
        val newPrIdx = (iter + 1) % 2
        MetaPipe(NV by tileSize) { ivt =>
          val prOldB = BRAM[Elem]("prTile", tileSize)
          val prNewB = BRAM[Elem]("prTile", tileSize)
          val vB = BRAM[Elem]("vertTile", tileSize, 2)
          prOldB := pageRank(ivt::ivt+tileSize, oldPrIdx)
          vB := vertList(ivt::ivt+tileSize, 0::2)
          MetaPipe (tileSize by 1) { iv =>
            val eB = BRAM[Elem]("edgeTile", maxNumEdge, 2)
            eB := edgeList(pt::pt+numEdge, 0::2)
            val eprB = BRAM[Elem]("edgePageRank", tileSize, 2)
            val pt = vB(iv,0)
            val numEdge = vB(iv,1)
            // Flatten address
            Pipe (numEdge by 1, 2 by 1) { (ie, is) =>
              eBaddr(ie) = eB(ie) * 2 + is
            }
            eprB := pageRank(eB)
            val sum = Reg[Elem]("sum")
            Pipe (numEdge by 1, sum) { ie =>
              eprB(ie, iter % 2) / oB(ie) 
            } {_+_}
            val pr = sum.value * DF + (1-DF) / NV
            prNewB(iv, newPrIdx) := pr
          } 
          pageRank(ivt::ivt+tileSize, (iter + 1) % 2) = prNewB
        }
      }
      */
      Pipe {val a = 3}
    }

  }
}
