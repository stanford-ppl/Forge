import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object TrangleCountingCompiler extends DHDLApplicationCompiler with TrangleCounting
object TrangleCountingInterpreter extends DHDLApplicationInterpreter with TrangleCounting
trait TrangleCounting extends DHDLApplication {
  type Elem = Flt //FixPt[Signed, B16, B16]

  override def stageArgNames = List("tileSize")
  lazy val tileSize = param("tileSize", 2)
  lazy val numIter = ArgIn[SInt]("numIter")

  def main() {
    //val NV = args(unit(0)).to[SInt]
    ////genRandDirEdgeList("/Users/Yaqi/Documents/hyperdsl/published/DHDL/graph.dot", NV, NE, true)
    val map = loadUnDirEdgeList("/Users/Yaqi/Documents/hyperdsl/forge/apps/DHDL/graph/testTc.dot", true)
    val vl:Rep[ForgeArray[SInt]] = getVertList(map, false, true)
    val el:Rep[ForgeArray[SInt]] = getEdgeList(map) 
    println("vl: " + vl.mkString(","))
    println("el: " + el.mkString(","))
    val NV = vl.length/2 // Actual number of vertices in graph
    val NE = el.length/2 // Divided by 2 b/c bi-directional edge

    val vertList = OffChipMem[Index]("VertList", NV, 2) // [pointer, size]
    val edgeList = OffChipMem[Index]("EdgeList", NE*2) // srcs of edges 
    val count = ArgOut[Elem]("count")
    
    setMem(vertList, vl)
    setMem(edgeList, el)

    Accel {
      MetaPipe(NV by tileSize) { ivt =>
        //val vB = BRAM[Index]("vertTile", tileSize, 2)
        //vB := vertList(ivt::ivt+tileSize, 0::2)
        MetaPipe (tileSize by 1) { iv =>
      //    val eB = BRAM[Index]("edgeTile", maxNumEdge)
      //    val oB = BRAM[Index]("outTile", maxNumEdge)
      //    val eprB = BRAM[Elem]("edgeTrangleCounting", maxNumEdge)
      //    val idxB = BRAM[Index]("idxTile", maxNumEdge)
      //    val pt = mux(vB(iv,0) < 0.as[SInt], 0, vB(iv,0)) //If pointer is -1, set to 0
      //    val numEdge = vB(iv,1.as[SInt])
      //    //println("iv:" + iv)
      //    //println("pt:" + pt)
      //    //println("numEdge:" + numEdge)
      //    Parallel {
      //      eB := edgeList(pt::pt+numEdge)
      //      //printBram(eB)
      //      oB := outBounds(pt::pt+numEdge)
      //      //printBram(oB)
      //    }
      //    Pipe (numEdge by 1) { ie =>
      //      idxB(ie) = eB(ie) * 2.as[SInt] + oldPrIdx 
      //    }
      //    //printBram(idxB)
      //    eprB := pageRank(idxB, numEdge)
      //    //printBram(eprB)
      //    val sum = Reg[Elem]("sum")
      //    Pipe (numEdge by 1, sum) { ie =>
      //      eprB(ie) / oB(ie).to[Elem] 
      //    } {_+_}
      //    //println("sum:" + sum.value)
      //    Pipe {
      //      val pr = sum.value * damp + (1.as[Elem]-damp)
      //      prNewB(iv) = pr
      //    }
        } 
      //  //printBram(prNewB)
      //  pageRank(ivt::ivt+tileSize, newPrIdx::newPrIdx+1.as[SInt]) := prNewB
      }
      Pipe {val a = 3}

    }
    
    //val result = getMem(pageRank)

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
      //TODO: adding this line causes error in lms code motion. But without this line, the entire
      //function seems code motioned away?
      //println("size=0:" + n1size==0.as[Index] + " " + n2size==0.as[Index])
      if (n1size==0.as[Index] || n2size==0.as[Index])
        return 0.as[Index]
      println("n1pt:" + n1pt + " n1size:" + n1size + " n2pt:" + n2pt + " n2size:" + n2size)
      //var count = 0.as[Index]
      //var n1cur = n1pt
      //var n2cur = n2pt
      //for (i <- 0 until (n1size + n2size)) {
      //  print("n1cur:" + n1cur + " n2cur:" + n2cur + " count:" + count)
      //  if (n1cur == (n1pt+n1size) || n2cur == (n2pt+n2size))
      //    return count
      //  if (el(n1cur) < el(n2cur))
      //    n1cur += 1
      //  else if (el(n1cur) > el(n2cur))
      //    n2cur +=1
      //  else { //n1cur == n2cur
      //    count +=1
      //    if (n1cur < n1pt + n1size)
      //      n1cur += 1
      //  }
      //}
      -1.as[Index]
    }

    val gold = Array.tabulate(NV){ iv=>
      val pt = vl(iv*2)
      val size = vl(iv*2+1) 
      Array.tabulate(size){ i =>
        val ie = pt + i
        val nbr = el(ie)
        val cm = if (nbr > iv) commonNeighbors(iv, nbr, vl, el)
                 else 0.as[Index]
        println("iv:" + iv + " nbr:" + nbr + " cm:" + cm)
        cm
      }.reduce{_+_}
    }.reduce{_+_}

    val result = count.value
    println("expected: " + gold)
    println("result: " + result)
    //assert(result == gold)

  }
}
