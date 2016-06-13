import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object TileLoadCompiler extends DHDLApplicationCompiler with TileLoadTest
trait TileLoadTest extends DHDLApplication {

  lazy val n = 1
  lazy val p = unit(48)
  lazy val rows = 1000.as[SInt]
  lazy val cols = 4800.as[SInt]

  def loads(v1: Rep[OffChipMem[Flt]]) {
    val b1 = List.tabulate(n){i => BRAM[Flt]("b1", rows, cols) }

//    Sequential {
//      Parallel {
//        b1.foreach{b => b := v1(0::rows, 0::cols, p) }
//      }
//    }
    b1(0) := v1(0::rows, 0::cols)
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
      loads(v1)
    }
  }
}
