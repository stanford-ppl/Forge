import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object SimpleMapCompiler extends DHDLApplicationCompiler with SimpleMap
object SimpleMapInterpreter extends DHDLApplicationInterpreter with SimpleMap
trait SimpleMap extends DHDLApplication {
  type Array[T] = ForgeArray[T]

  def simplemap(xin: Rep[SInt]) = {
    val innerPar = param("innerPar", 1); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)
//    bound(N) = 187200000

    val x = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(x, xin)

    Accel {
      Sequential.fold (tileSize by tileSize)(out) { i =>
        val b1 = BRAM[SInt]("b1", tileSize)
        Pipe.foreach(tileSize par innerPar) { ii =>
          b1(ii) = x.value * ii
        }
        Pipe.reduce(tileSize par innerPar)(Reg[SInt]) { ii =>  b1(ii) } {_+_}
      } {_+_}
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
    println("expected: " + gold.mkString)
    println("result:   " + result.mkString)
    assert(result == gold)
  }
}
