import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TestCompiler extends DHDLApplicationCompiler with Test
object TestInterpreter extends DHDLApplicationInterpreter with Test
trait Test extends DHDLApplication {

  def main() {
    type Q16 = FixPt[Signed, B16, B16]

    val v1    = OffChipMem[Q16]("v1", 10)
    val outer = ArgOut[Q16]

    val vec1 = Array.fill(10)(random[Q16](10))
    setMem(v1, vec1)

    Accel {
      val b1 = BRAM[Q16]("b1", 5)

      MetaPipe(10 by 5, outer){i =>
        b1 := v1(i::i+5)

        val inner = Reg[Q16]
        Pipe(0 until 5, inner){ii => b1(ii) ** 2 }{_+_}
        inner.value
      }{_+_}
    }

    println("vec1: " + vec1.mkString(", "))

    val gold = vec1.map{_**2}.reduce{_+_}

    println("outer: " + getArg(outer).mkString + " (should be " + gold.mkString + ")")
  }
}
