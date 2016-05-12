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
  lazy val maxNumEdge = param("maxNumEdge", 8)
  lazy val numIter = ArgIn[SInt]("numIter")
  lazy val damp = ArgIn[Elem]("damping")

  def main() {
    val NV = args(unit(0)).to[SInt]
    val NE = args(unit(1)).to[SInt]
    val NI = args(unit(2)).to[SInt]
    val DF = args(unit(3)).to[Elem]
    //genRandDirEdgeList("/Users/Yaqi/Documents/hyperdsl/published/DHDL/graph.dot", NV, NE, true)
    //Verified with graph from http://www.cs.princeton.edu/~chazelle/courses/BIB/pagerank.htm
    //NV = 4, NE = 5, DF = 0.85
    val maps = loadDirEdgeList("/Users/Yaqi/Documents/hyperdsl/forge/apps/DHDL/graph/testPr.dot", NV, true)
    val smap = maps(0)
    val dmap = maps(1)
    val svl:Rep[ForgeArray[SInt]] = getVertList(smap, false)
    val dvl:Rep[ForgeArray[SInt]] = getVertList(dmap, false)
    val del:Rep[ForgeArray[SInt]] = getEdgeList(dmap) 
    val sob = Array.tabulate(NE) { i => 
      svl(del(i)*2+1)
    }
    //println("svl: " + svl.mkString(","))
    //println("dvl: " + dvl.mkString(","))
    //println("del: " + del.mkString(","))
    //println("sob: " + sob.mkString(","))
    //val NV = veArr(0).length/3 // Actual number of vertices in graph
    //val NE = veArr(1).length // Actual number of vertices in graph
    val vertList = OffChipMem[Index]("VertList", NV, 2) // [pointer, size]
    val edgeList = OffChipMem[Index]("EdgeList", NE) // srcs of edges 
    val outBounds = OffChipMem[Index]("outBounds", NE) // number of outbound links for each src in edgeList 
    val pageRank = OffChipMem[Elem]("PageRank", NV, 2) // [PR iter even, PR iter odd]
    
    setArg(numIter, NI)
    setArg(damp, DF)
    setMem(vertList, dvl)
    setMem(edgeList, del)
    setMem(outBounds, sob)

    val init = Array.fill(NV*2) (1.as[Elem]/NV.to[Elem])
    setMem(pageRank, init)
    //println("initial page rank: " + getMem(pageRank).mkString(","))

    Accel {
      Sequential(numIter by 1) { iter =>
        //println("iter:" + iter + " ---------------------")
        val oldPrIdx = iter % 2.as[Index]
        val newPrIdx = (iter + 1.as[Index]) % 2.as[Index]
        MetaPipe(NV by tileSize) { ivt =>
          val prOldB = BRAM[Elem]("prOldTile", tileSize)
          val prNewB = BRAM[Elem]("prNewTile", tileSize)
          val vB = BRAM[Index]("vertTile", tileSize, 2)
          prOldB := pageRank(ivt::ivt+tileSize, oldPrIdx::oldPrIdx+1.as[SInt])
          vB := vertList(ivt::ivt+tileSize, 0::2)
          MetaPipe (tileSize by 1) { iv =>
            val eB = BRAM[Index]("edgeTile", maxNumEdge)
            val oB = BRAM[Index]("outTile", maxNumEdge)
            val eprB = BRAM[Elem]("edgePageRank", maxNumEdge)
            val idxB = BRAM[Index]("idxTile", maxNumEdge)
            val pt = mux(vB(iv,0) < 0.as[SInt], 0, vB(iv,0)) //If pointer is -1, set to 0
            val numEdge = vB(iv,1.as[SInt])
            //println("iv:" + iv)
            //println("pt:" + pt)
            //println("numEdge:" + numEdge)
            Parallel {
              eB := edgeList(pt::pt+numEdge)
              //printBram(eB)
              oB := outBounds(pt::pt+numEdge)
              //printBram(oB)
            }
            //TODO: Flatten idx in app, move into dhdl
            Pipe (numEdge by 1) { ie =>
              idxB(ie) = eB(ie) * 2.as[SInt] + oldPrIdx 
            }
            //printBram(idxB)
            eprB := pageRank(idxB, numEdge)
            //printBram(eprB)
            val sum = Reg[Elem]("sum")
            Pipe (numEdge by 1, sum) { ie =>
              eprB(ie) / oB(ie).to[Elem] 
            } {_+_}
            //println("sum:" + sum.value)
            Pipe {
              val pr = sum.value * damp + (1.as[Elem]-damp)
              prNewB(iv) = pr
            }
          } 
          //printBram(prNewB)
          pageRank(ivt::ivt+tileSize, newPrIdx::newPrIdx+1.as[SInt]) := prNewB
        }
      }
    }
    
    val result = getMem(pageRank)

    /* Scala Version */ 
    val gold = Array.fill (NV*2)(1.as[Elem]/NV.to[Elem])
    for (iter <- 0.as[Index] until NI) {
      val oldPrIdx = iter % 2.as[Index]
      val newPrIdx = (iter + 1.as[Index]) % 2.as[Index]
      def getPr(iv:Rep[Index]) = gold(iv*2.as[Index] + oldPrIdx)
      def setPr(iv:Rep[Index], p:Rep[Elem]) = gold(iv*2.as[Index] + newPrIdx) = p
      for (iv <- 0.as[Index] until NV) {
        val pt = dvl(iv*2)
        val numEdge = dvl(iv*2+1)
        val sum = if (numEdge == 0.as[Index]) {
          0.as[Elem]  
        } else {
          Array.tabulate(numEdge){ ie => 
            val e = del(pt + ie)
            getPr(e) / sob(pt + ie).to[Elem] 
          }.reduce(_+_)
        }
        setPr(iv, sum * DF + (1.as[Elem] - DF))
      }
    }
    println("expected: " + gold.mkString(","))
    println("result: " + result.mkString(","))
    assert(result == gold)

  }
}
