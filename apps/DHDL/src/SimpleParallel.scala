import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object SimpleParallelCompiler extends DHDLApplicationCompiler with SimpleParallel
object SimpleParallelInterpreter extends DHDLApplicationInterpreter with SimpleParallel
trait SimpleParallel extends DHDLApplication {
  type Array[T] = ForgeArray[T]

  def simpleseq(xin: Rep[SInt], yin: Rep[SInt]) = {
    val innerPar = param("innerPar", 1); domainOf(innerPar) = (1, 1, 1)
    val tileSize1 = param("tileSize", 96); domainOf(tileSize1) = (96, 96, 96)
    val tileSize2 = param("tileSize", 192); domainOf(tileSize2) = (192, 192, 192)

//    val x = ArgIn[SInt]("x")
//    val y = ArgIn[SInt]("y")
//    val out = ArgOut[SInt]("out")
    val x = ArgIn[SInt]
    val y = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(x, xin)
    setArg(y, yin)

    Accel {
//      val b1 = BRAM[SInt]("b1", tileSize1)
//      val b2 = BRAM[SInt]("b2", tileSize2)
      val b1 = BRAM[SInt](tileSize1)
      val b2 = BRAM[SInt](tileSize2)
      Sequential(1 by 1) { i =>
        Parallel {
          Pipe.foreach(tileSize1 par innerPar) { ii =>
            b1(ii) = x.value * ii
          }
          Pipe.foreach(tileSize2 par innerPar) { ii =>
            b2(ii) = x.value * ii
          }
        }
//        val b3 = BRAM[SInt]("b3", tileSize1, tileSize2)
        val b3 = BRAM[SInt](tileSize1, tileSize2)
        Pipe (tileSize1 by 1, tileSize2 by 1) { (ii, jj) =>
          b3(ii, jj) = b1(ii) * b2(jj)
        }
        Pipe { out := b3(0, y.value) }
      }
      ()
    }
    getArg(out)
  }

  def main() {
    val x = args(unit(0)).to[SInt]
    val y = args(unit(1)).to[SInt]

    val result = simpleseq(x, y)

    val b1 = Array.tabulate[SInt](96) { i => x * i }
    val b2 = Array.tabulate[SInt](192) { i => x * i }
    val b3 = b1 map { i => b2.map { j => i * j } }
//    val gold = b3(0)(0)
//    println("expected: " + gold)
    println("result:   " + result)
//    assert(result == gold)
  }
}
