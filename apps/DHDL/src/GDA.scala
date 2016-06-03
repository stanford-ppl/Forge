import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object GDACompiler extends DHDLApplicationCompiler with GDA
object GDAInterpreter extends DHDLApplicationInterpreter with GDA
trait GDA extends DHDLApplication {
  type T = Flt
  type Array[T] = ForgeArray[T]

  val Cmax = 16 //96

  def gda(xCPU: Rep[Array[T]], yCPU: Rep[Array[Bit]], mu0CPU: Rep[Array[T]], mu1CPU: Rep[Array[T]]) = {
    val rTileSize     = param("tileSize", 4);      domainOf(rTileSize) = (96, 19200, 1)
    val outerPar      = param("outerPar", 2);      domainOf(outerPar)  = (1, 4, 1)
    val innerPar      = param("innerPar", 2);      domainOf(innerPar)  = (1, 12, 1)
    val subLoopPar    = param("subLoopPar", 1);    domainOf(subLoopPar)    = (1, 16, 1)
    val prodLoopPar   = param("prodLoopPar", 2);   domainOf(prodLoopPar)   = (1, 96, 1)
    val outerAccumPar = param("outerAccumPar", 1); domainOf(outerAccumPar) = (1, 1, 1)

    val R = yCPU.length;   bound(R) = 360000
    val C = mu0CPU.length; bound(C) = Cmax

    assert(C == Cmax) // TODO: Shouldn't be necessary, but addressing requires it at the moment

    val x     = OffChipMem[T]("x", R, C)
    val y     = OffChipMem[Bit]("y", R)
    val mu0   = OffChipMem[T]("mu0", C)
    val mu1   = OffChipMem[T]("mu1", C)
    val sigma = OffChipMem[T]("sigma", C, C)
    val rows  = ArgIn[SInt]("rows")
    val cols  = ArgIn[SInt]("cols")

    setArg(rows, R)
    setArg(cols, C)
    setMem(x, xCPU)
    setMem(y, yCPU)
    setMem(mu0, mu0CPU)
    setMem(mu1, mu1CPU)

    Accel {
      val mu0Tile = BRAM[T]("mu0Tile", Cmax)
      val mu1Tile = BRAM[T]("mu1Tile", Cmax)
      Parallel {
        mu0Tile := mu0(0::Cmax, subLoopPar)  // Load mu0
        mu1Tile := mu1(0::Cmax, subLoopPar)  // Load mu1
      }

      val sigmaOut = BRAM[T]("sigmaOut", Cmax, Cmax)

      Pipe.fold(rows by rTileSize par outerPar, outerAccumPar)(sigmaOut){ r =>
        val yTile = BRAM[Bit]("yTile", rTileSize)
        val xTile = BRAM[T]("xTile", rTileSize, Cmax)
        Parallel {
          yTile := y(r::r+rTileSize, subLoopPar)
          xTile := x(r::r+rTileSize, 0::cols, subLoopPar)  // Load tile of x
        }

        val sigmaBlk = BRAM[T]("sigmaBlk", Cmax, Cmax)
        Pipe.fold(rTileSize par innerPar, prodLoopPar)(sigmaBlk){rr =>
          val subTile = BRAM[T]("subTile", Cmax)
          val sigmaTile = BRAM[T]("sigmaTile", Cmax, Cmax)
          Pipe(cols par subLoopPar){ cc =>
            subTile(cc) = xTile(rr,cc) - mux(yTile(rr), mu1Tile(cc), mu0Tile(cc))
          }
          Pipe(cols by 1, cols par prodLoopPar){ (ii,jj) =>
            sigmaTile(ii,jj) = subTile(ii) * subTile(jj)
          }
          sigmaTile
        }{_+_}
      }{_+_}

      sigma(0::Cmax, 0::Cmax, prodLoopPar) := sigmaOut
    }

    getMem(sigma)
  }

  def main() {
    val R = args(unit(0)).to[SInt]
    val C = Cmax.as[SInt] //args(unit(0)).to[SInt] // TODO: Should be selectable up to maximum

    val x  = Array.fill(R){ Array.fill(C){ random[T](10) }}
    val ys = Array.fill(R){ random[Bit] }
    val mu0 = Array.fill(C){ random[T](10) }
    val mu1 = Array.fill(C){ random[T](10) }

    val result = gda(x.flatten, ys, mu0, mu1)

    val gold = x.zip(ys){ (row, y) =>
      val sub = if (y) row.zip(mu1){_-_} else row.zip(mu0){_-_}
      Array.tabulate(C){i => Array.tabulate(C){j => sub(i) * sub(j) }}.flatten
    }.reduce{(a,b) => a.zip(b){_+_}}

    //println("actual: " + gold.mkString(", "))
    //println("result: " + result.mkString(", "))
    println("Sum of differences: " + gold.zip(result){_-_}.reduce{_+_})
    assert( result == gold )
  }
}
