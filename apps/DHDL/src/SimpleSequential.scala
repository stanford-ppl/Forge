import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

//object SimpleSequentialCompiler extends DHDLApplicationCompiler with SimpleSequential
//object SimpleSequentialInterpreter extends DHDLApplicationInterpreter with SimpleSequential
//trait SimpleSequential extends DHDLApplication {
//  type Array[T] = ForgeArray[T]
//
//  def simpleseq(xin: Rep[SInt], yin: Rep[SInt]) = {
//    val innerPar = param("innerPar", 1); domainOf(innerPar) = (1, 1, 1)
//    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)
//
//    val x = ArgIn[SInt]
//    val y = ArgIn[SInt]
//    val out = ArgOut[SInt]
//    setArg(x, xin)
//    setArg(y, yin)
//
//    Accel {
//      val b1 = BRAM[SInt](tileSize)
////      Sequential (tileSize by tileSize) { i =>
//      Sequential {
//        Pipe.foreach(tileSize par innerPar) { ii =>
//          b1(ii) = x.value * ii
//        }
//        Pipe { out := b1(y) }
//      }
//      ()
//    }
//    getArg(out)
//  }
//
//  def main() {
//    val x = args(unit(0)).to[SInt]
//    val y = args(unit(1)).to[SInt]
//
//    val result = simpleseq(x, y)
//
//    val b1 = Array.tabulate[SInt](96) { i => x * i }
//    val gold = b1(y)
//    println("expected: " + gold)
//    println("result:   " + result)
//    assert(result == gold)
//  }
//}
