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
    val v1 = OffChipMem[A](N)
    val v2 = OffChipMem[A](N)
    val out = OffChipMem[A](N, N)

    val vec1 = Array.fill(N)(random[A](10))
    val vec2 = Array.fill(N)(random[A](10))
    setMem(v1, vec1)
    setMem(v2, vec2)

    Accel {
      Pipe(N by T, N by T par PN) { (i,j) =>
        val b1 = BRAM[A](T)
        val b2 = BRAM[A](T)
        val outTile = BRAM[A](T, T)
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

// Previously reported that hwblock pointer was stale after unrolling
object Test2Compiler extends DHDLApplicationCompiler with Test2
trait Test2 extends DHDLApplication {
  def main() {
    val T = param(4)
    val P = param(2)

    val x = ArgIn[SInt]
    Accel {
      Reduce(T par P)(0){ii => x.value * ii }{_+_}
      ()
    }
  }
}

// Reported bug where zero wasn't being propagated for registers
object Test3Compiler extends DHDLApplicationCompiler with Test3
trait Test3 extends DHDLApplication {
  def main() {
    val xin = 5
    val T = param(4)
    val P = param(2)
    val x = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(x, xin)

    Accel {
      Sequential(T by T){ i =>
        val b1 = BRAM[SInt](T)
        Pipe(T par P){ ii =>
          b1(ii) = x.value * ii
        }
        out := Reduce(T par P)(0){ii => b1(ii) }{_+_}
      }
      ()
    }
    getArg(out)
  }
}
