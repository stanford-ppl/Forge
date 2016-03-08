import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object OuterProductCompiler extends DHDLApplicationCompiler with OuterProduct
object OuterProductInterpreter extends DHDLApplicationInterpreter with OuterProduct
trait OuterProduct extends DHDLApplication {
  type Elem = Fix

  override def stageArgNames = List("tileSize")
  lazy val tileSize = stageArgOrElse[Int](0, 2)
  lazy val dataSize = ArgIn[Fix]("dataSize")

  def outerProduct(vec1: Rep[OffChipMem[Elem]], vec2: Rep[OffChipMem[Elem]], out: Rep[OffChipMem[Elem]]) = {
    MetaPipe(dataSize by tileSize, dataSize by tileSize) { (i,j) =>
      val b1 = BRAM[Elem]("b1", tileSize)
      val b2 = BRAM[Elem]("b2", tileSize)
      val outTile = BRAM[Elem]("outTile", tileSize*tileSize)
      Parallel {
        vec1.ld(b1, i, tileSize)
        vec2.ld(b2, j, tileSize)
      }
      Pipe(tileSize by 1, tileSize by 1) { (ii,jj) => outTile(ii, jj) = b1(ii) * b2(jj) }
      out.st(outTile, i, j, tileSize, tileSize)
    }
  }

  def main() = {
    val v1 = OffChipMem[Elem]("vec1", dataSize)
    val v2 = OffChipMem[Elem]("vec2", dataSize)
    val out = OffChipMem[Elem]("out", dataSize, dataSize)
    outerProduct(v1, v2, out)
	}
}

object OuterProductTestCompiler extends DHDLApplicationCompiler with OuterProductTest
object OuterProductTestInterpreter extends DHDLApplicationInterpreter with OuterProductTest
trait OuterProductTest extends OuterProduct {

  override def stageArgNames = List("tileSize", "dataSize")
  lazy val sdataSize = stageArgOrElse[Int](1, 32)

  override def main() {
    val svec1 = Seq.fill(sdataSize)(Random.nextInt(100))
    val svec2 = Seq.fill(sdataSize)(Random.nextInt(100))
    val gold = Seq.tabulate(sdataSize){i =>
        Seq.tabulate(sdataSize){j =>
        svec1(i)*svec2(j)
      }
    }

    val vec1 = OffChipMem.withInit1D("vec1", svec1.map(i => i.toFixPt))
    val vec2 = OffChipMem.withInit1D("vec2", svec2.map(i => i.toFixPt))
    val out  = OffChipMem[Elem]("result", sdataSize, sdataSize)
    outerProduct(vec1, vec2, out)

    gold.flatten.zipWithIndex.foreach{case (g, i) => assert(out.ld(i) == g) }
  }
}