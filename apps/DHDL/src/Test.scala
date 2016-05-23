import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TestCompiler extends DHDLApplicationCompiler with Test
object TestInterpreter extends DHDLApplicationInterpreter with Test
trait Test extends DHDLApplication {

  def main() {
    type Q16 = FixPt[Signed, B16, B16]

    val N = 6
    val v1 = OffChipMem[Q16]("v1", N)
    val out = ArgOut[Q16]("out")
    val vec1 = Array.fill(N)(random[Q16](10))
    setMem(v1, vec1)

    Accel {
      val b1 = BRAM[Q16]("b1", N)
      b1 := v1(0::N, unit(6))
      Pipe.reduce(N par unit(6))(out){ii =>
        val elem = b1(ii)
        println(elem.mkString)
        elem
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
