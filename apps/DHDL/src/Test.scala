import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TestCompiler extends DHDLApplicationCompiler with Test
object TestInterpreter extends DHDLApplicationInterpreter with Test
trait Test extends DHDLApplication {

  def main() {
    type Q16 = FixPt[Signed, B16, B16]

    val N = 24
    val T = 6
    val PN = param(2)
    val PT = param(2)
    val v1 = OffChipMem[Q16]("v1", N)
    val out = ArgOut[Q16]("out")
    //val vec1 = Array.fill(N)(random[Q16](10))
    //setMem(v1, vec1)

    Accel {
      val b1 = BRAM[Q16]("b1", N)

      Pipe.fold(N by T par PN)(out){i =>
        b1 := v1(i::i+T)
        Pipe.reduce(T par PT)(Reg[Q16]){ii => b1(ii) }{_+_}
      }{_+_}
      ()
    }
    /*println("vec1: " + vec1.mkString(", "))
    val result = getArg(out)
    println("result: " + result.mkString)

    val gold = vec1.reduce{_+_}
    println("gold: " + gold.mkString)*/
  }
}
