import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object BankingTestCompiler extends DHDLApplicationCompiler with BankingTest
trait BankingTest extends DHDLApplication {

  def main() {

  type Elem = SInt
    //println("vec1: " + vec1.mkString(", "))
    //println("vec2: " + vec2.mkString(", "))

    val N = 8;
    val tileSize = 4;

    val out = ArgOut[Elem]("out")

    val v1 = OffChipMem[Elem]("v1", N)
    val v2 = OffChipMem[Elem]("v2", N)
    Accel {
      val b1 = BRAM[Elem]("b1", tileSize)
      val b2 = BRAM[Elem]("b2", tileSize)
      Parallel {
        b1 := v1(0::tileSize)
        b2 := v2(0::tileSize)
      }
      Pipe.reduce(0 until N)(out){ ii => b1(ii) * b2(ii) }{_+_}
    }
    val result = getArg(out)
    println(result)
  }
}
