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
    val N = 32

    val v1 = OffChipMem[Elem]("vec1", dataSize)
    val v2 = OffChipMem[Elem]("vec2", dataSize)
    val out = OffChipMem[Elem]("out", dataSize, dataSize)

    val vec1 = Array.fill(N)(random[Elem])
    val vec2 = Array.fill(N)(random[Elem])

    // Transfer data and start accelerator
    setArg(dataSize, N)
    setMem(v1, vec1)
    setMem(v2, vec2)
    Accel{ outerProduct(v1, v2, out) }

    val gold = Array.tabulate(N){i => Array.tabulate(N){j => vec1(i) * vec2(j) }}.flatten

    val result = Array.empty[Fix](N)
    getMem(out, result)

    assert( result == gold )
	}
}
