import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object SimpleAccumMemCompiler extends DHDLApplicationCompiler with SimpleAccumMem
object SimpleAccumMemInterpreter extends DHDLApplicationInterpreter with SimpleAccumMem
trait SimpleAccumMem extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]

  val Cmax = 16 //96

  def SimpleAccumMem(xin: Rep[SInt], yin: Rep[SInt]) = {
    val rTileSize     = param("tileSize", 96);      domainOf(rTileSize) = (96, 19200, 1)
    val outerAccumPar      = param("outerAccumPar", 1);      domainOf(outerAccumPar)  = (1, 1, 1)
    val innerPar      = param("innerPar", 1);      domainOf(innerPar)  = (1, 1, 1)

    val out = ArgOut[SInt]
    val x = ArgIn[SInt]
    setArg(x, xin)
    val y = ArgIn[SInt]
    setArg(y, yin)


    Accel {
      val accumMem = BRAM[T](rTileSize)

      Pipe.fold(rTileSize par innerPar, outerAccumPar)(accumMem){ r =>
        val xTile = BRAM[T](rTileSize)
        Pipe.foreach(rTileSize par innerPar) { ii =>
          xTile(ii) = x.value * ii
        }
        xTile
      }{_+_}

      //Pipe { out := accumMem(y) }
    }

  }

  def main() {
    val x = args(unit(0)).to[SInt]
    val y = args(unit(1)).to[SInt]

    val result = SimpleAccumMem(x,y)
  }
}
