import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TestCompiler extends DHDLApplicationCompiler with Test
object TestInterpreter extends DHDLApplicationInterpreter with Test
trait Test extends DHDLApplication {

  def main() {
    type Q16 = FixPt[Signed, B16, B16]
    type A = SInt

    val N = 10
    val T = 5
    val PN = param(2)
    val PT = param(2)
    val v1 = OffChipMem[A]("v1", N)
    val v2 = OffChipMem[A]("v2", N)
    val out = OffChipMem[A]("out", N, N)

    val vec1 = Array.fill(N)(random[A](10))
    val vec2 = Array.fill(N)(random[A](10))
    setMem(v1, vec1)
    setMem(v2, vec2)

    Accel {
      Pipe(N by T, N by T par PN) { (i,j) =>
        val b1 = BRAM[A]("b1", T)
        val b2 = BRAM[A]("b2", T)
        val outTile = BRAM[A]("outTile", T, T)
        Parallel {
          b1 := v1(i::i+T)
          b2 := v2(j::j+T)
        }
        Pipe(T by 1, T by 1 par PT) { (ii,jj) => outTile(ii, jj) = b1(ii) * b2(jj) } // 2

        out(i::i+T, j::j+T) := outTile
      }
      ()
    }
    val result = getMem(out)
    println( result.mkString(", ") )
  }
}
