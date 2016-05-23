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
    val v2 = OffChipMem[Q16]("v2", N)
    val out = ArgOut[Q16]("out")
    val vec1 = Array.fill(N)(random[Q16](10))
    val vec2 = Array.fill(N)(random[Q16](10))
    setMem(v1, vec1)
    setMem(v2, vec2)

    Accel {
      val b1 = BRAM[Q16]("b1", N)
      val b2 = BRAM[Q16]("b2", N)
      b1 := v1(0::N)
      b2 := v2(0::N)
      Pipe.reduce(N par unit(2))(out){ii => b1(ii) * b2(ii) }{_+_}
      ()
    }

    println("vec1: " + vec1.mkString(", "))
    println("vec2: " + vec2.mkString(", "))
    val result = getArg(out)
    println("result: " + result.mkString)

    val gold = vec1.zip(vec2){_*_}.reduce{_+_}
    println("gold: " + gold.mkString)
  }
}
