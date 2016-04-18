import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object OuterProductCompiler extends DHDLApplicationCompiler with OuterProduct
object OuterProductInterpreter extends DHDLApplicationInterpreter with OuterProduct
trait OuterProduct extends DHDLApplication {
  type Elem = UInt //FixPt[Signed,B16,B16]

  override def stageArgNames = List("tileSize")
  lazy val dataSize = ArgIn[SInt]("dataSize")

  lazy val tileSize = param("tileSize", 384)
  lazy val outerPar = param("outerPar", 1)
  lazy val innerPar = param("innerPar", 16)

  def outerProduct(vec1: Rep[OffChipMem[Elem]], vec2: Rep[OffChipMem[Elem]], out: Rep[OffChipMem[Elem]]) {
    MetaPipe(dataSize by tileSize, (dataSize by tileSize) par outerPar) { (i,j) =>
      val b1 = BRAM[Elem]("b1", tileSize)
      val b2 = BRAM[Elem]("b2", tileSize)
      val outTile = BRAM[Elem]("outTile", tileSize, tileSize)
      Parallel {
        b1 := vec1(i::i+tileSize, innerPar)
        b2 := vec2(j::j+tileSize, innerPar)
      }
      Pipe(tileSize by 1, (tileSize by 1) par innerPar) { (ii,jj) => outTile(ii, jj) = b1(ii) * b2(jj) } // 2

      out(i::i+tileSize, j::j+tileSize, param(1)) := outTile
    }
  }

  def main() = {
    val N = args(unit(0)).to[SInt]

    bound(N) = 3840
    domainOf(tileSize) = (96,960,96)  // 10
    domainOf(outerPar) = (1,4,1)      // 4
    domainOf(innerPar) = (1,192,6)    // 32

    val v1 = OffChipMem[Elem]("vec1", N)
    val v2 = OffChipMem[Elem]("vec2", N)
    val out = OffChipMem[Elem]("out", N, N)

    val vec1 = Array.fill(N)(random[Elem](100))
    val vec2 = Array.fill(N)(random[Elem](100))

    // Transfer data and start accelerator
    setArg(dataSize, N)
    setMem(v1, vec1)
    setMem(v2, vec2)
    Accel{ outerProduct(v1, v2, out) }

    val gold = Array.tabulate(N){i => Array.tabulate(N){j => vec1(i) * vec2(j) }}.flatten

    val result = getMem(out)

    println("expected: " + gold.mkString(", "))
    println("result:   " + result.mkString(", "))
    assert( result == gold )
  }
}
