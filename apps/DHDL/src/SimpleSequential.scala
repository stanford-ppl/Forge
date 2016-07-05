import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object SimpleSequentialCompiler extends DHDLApplicationCompiler with SimpleSequential
object SimpleSequentialInterpreter extends DHDLApplicationInterpreter with SimpleSequential
trait SimpleSequential extends DHDLApplication {
  type Array[T] = ForgeArray[T]

  def simplemap(xin: Rep[SInt]) = {
    val innerPar = param("innerPar", 1); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)
//    bound(N) = 187200000

    val x = ArgIn[SInt]("x")
    val out = ArgOut[SInt]("out")
    setArg(x, xin)

    Accel {
      val accumOut = Reg[SInt]("accumOut")
      Sequential.reduce (tileSize by tileSize)(accumOut) { i =>
        val b1 = BRAM[SInt]("b1", tileSize)
        Pipe.foreach(tileSize par innerPar) { ii =>
          b1(ii) = x.value * ii
        }
        val accumIn = Reg[SInt]("accumIn")
        Pipe.reduce(tileSize par innerPar)(accumIn) { ii =>  b1(ii) } {_+_}
      } {_+_}
      out := accumOut
      ()
    }
    getArg(out)
  }

  def main() {
    val x = args(unit(0)).to[SInt]
//    val N = args(unit(1)).to[SInt]

    val result = simplemap(x)

    val b1 = Array.tabulate[SInt](96) { i => x * i }
    val gold = b1.reduce {_+_}
    println("expected: " + gold)
    println("result:   " + result)
    assert(result == gold)
  }
}
