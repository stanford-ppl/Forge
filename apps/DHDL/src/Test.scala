import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TestCompiler extends DHDLApplicationCompiler with Test
object TestInterpreter extends DHDLApplicationInterpreter with Test
trait Test extends DHDLApplication {

  def main() {
    type Q16 = FixPt[Signed, B16, B16]
    type A = SInt

    val N = 24
    val T = 6
    val PN = param(2)
    val PT = param(1)
    val v1 = OffChipMem[A]("v1", N)
    val out = ArgOut[A]("out")

    val vec1 = Array.fill(N)(random[A](10))
    setMem(v1, vec1)

    Accel {
      Pipe.fold(N by T par PN)(out){i =>
        val b1 = BRAM[A]("b1", N)
        b1 := v1(i::i+T)
        Pipe.reduce(T par PT)(Reg[A]){ii =>
          println(i + ", " + ii + ": " + b1(ii))
          b1(ii)
        }{_+_}
      }{_+_}
      ()
    }

    println("vec1: " + vec1.mkString(", "))

    val result = getArg(out)
    println("result: " + result.mkString)
    val gold = vec1.reduce{_+_}
    println("gold: " + gold.mkString)
  }
}
