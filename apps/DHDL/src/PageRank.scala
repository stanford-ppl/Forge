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
  lazy val maxNumEdge = param("maxNumEdge", 64)
  lazy val numIter = ArgIn[SInt]("numIter")

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
    val NI = args(unit(2)).to[SInt]
    //genRandDirEdgeList("/Users/Yaqi/Documents/hyperdsl/published/DHDL/graph.dot", NV, NE, true)
    val maps = loadDirEdgeList("/Users/Yaqi/Documents/hyperdsl/published/DHDL/graph.dot", NV, true)
    val smap = maps(0)
    val dmap = maps(1)
    val svl:Rep[ForgeArray[SInt]] = getVertList(smap, false)
    val dvl:Rep[ForgeArray[SInt]] = getVertList(dmap, false)
    val del:Rep[ForgeArray[SInt]] = getEdgeList(dmap) 
    val sob = Array.tabulate(NE) { i => 
      svl(del(i)*2+1)
    }
    println("svl: " + svl.mkString(","))
    println("dvl: " + dvl.mkString(","))
    println("del: " + del.mkString(","))
    println("sob: " + sob.mkString(","))
    //val NV = veArr(0).length/3 // Actual number of vertices in graph
    //val NE = veArr(1).length // Actual number of vertices in graph
    val vertList = OffChipMem[Index]("VertList", NV, 2) // [pointer, size]
    val edgeList = OffChipMem[Index]("EdgeList", NE) // srcs of edges 
    val outBounds = OffChipMem[Index]("outBounds", NE) // number of outbound links for each src in edgeList 
    val pageRank = OffChipMem[Elem]("PageRank", NV, 2) // [PR iter even, PR iter odd]
    
    setArg(numIter, NI)
    setMem(vertList, dvl)
    setMem(edgeList, del)
    setMem(outBounds, sob)
    println("vertList: " + getMem(vertList).mkString(","))
    println("edgeList: " + getMem(edgeList).mkString(","))

    val init = Array.tabulate(NV*2) { i => 1.as[Elem]/NV.to[Elem] }
    setMem(pageRank, init)
    println("initial page rank: " + getMem(pageRank).mkString(","))

    Accel {
      Sequential(numIter by 1) { iter =>
        println("iter:" + iter)
        val oldPrIdx = iter % 2
        val newPrIdx = (iter + 1) % 2
        MetaPipe(NV by tileSize) { ivt =>
          val prOldB = BRAM[Elem]("prTile", tileSize)
          val prNewB = BRAM[Elem]("prTile", tileSize)
          val vB = BRAM[Index]("vertTile", tileSize, 2)
          prOldB := pageRank(ivt::ivt+tileSize, oldPrIdx)
          vB := vertList(ivt::ivt+tileSize, 0::2)
          printBram(vB)
          //printBram(prOldB)
          println(vB(1,0))
          MetaPipe (tileSize by 1) { iv =>
            val eB = BRAM[Index]("edgeTile", maxNumEdge)
            val oB = BRAM[Index]("outTile", maxNumEdge)
            val eprB = BRAM[Elem]("edgePageRank", tileSize, 2)
            //printBram(vB)
            //printBram(prOldB)
            println(vB(1,0))
            //val pt = mux(vB(iv,0) < 0.as[SInt], 0, vB(iv,0)) //If pointer is -1, set to 0
            //printBram(vB)
            //val numEdge = vB(iv,1.as[SInt])
            //println("pt:" + pt)
            //println("numEdge:" + numEdge)
            //Parallel {
            //  eB := edgeList(pt::pt+numEdge)
            //  oB := outBounds(pt::pt+numEdge)
            //}
            //printBram(eB)
            //printBram(oB)
            //eprB := pageRank(eB)
      /*
            val sum = Reg[Elem]("sum")
            Pipe (numEdge by 1, sum) { ie =>
              eprB(ie, iter % 2) / oB(ie) 
            } {_+_}
            val pr = sum.value * DF + (1-DF) / NV
            prNewB(iv, newPrIdx) := pr
      */
          } 
          println(vB(1,0))
          //pageRank(ivt::ivt+tileSize, (iter + 1) % 2) = prNewB
        }
      }
      Pipe {val a = 3}
    }

  }
}
