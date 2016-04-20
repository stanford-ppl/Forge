import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object TileStoreCompiler extends DHDLApplicationCompiler with TileStoreTest
trait TileStoreTest extends DHDLApplication {

  lazy val c = 1
  lazy val p = unit(24)
  lazy val rows = 1000.as[SInt]
  lazy val cols = 1920.as[SInt]

  def stores(v1: Rep[OffChipMem[Flt]]) {
    val b1 = List.tabulate(c){i => BRAM[Flt]("b1", rows, cols) }

    Sequential(0 until 5000) { i =>
      Parallel {
        b1.foreach{b => v1(0::rows, 0::cols, p) := b }
      }
    }
  }

  def main() {
    val N = args(unit(0)).to[SInt]
    val C = args(unit(1)).to[SInt]

    bound(N) = 9993600
    bound(C) = 9993600
    val v1 = OffChipMem[Flt]("v1", N, C)

    val vec1 = Array.fill(N)(random[Flt](10))

    setMem(v1, vec1)

    Accel {
      stores(v1)
    }
  }
}
