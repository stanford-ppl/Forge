import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object GDACompiler extends DHDLApplicationCompiler with GDA
object GDAInterpreter extends DHDLApplicationInterpreter with GDA
trait GDA extends DHDLApplication {
  type T = Flt
  type Array[T] = ForgeArray[T]

  val innerPar = 1
  val outerPar = 1
  val rr = 384
  val cc = 96 //96

  def gda(xCPU: Rep[Array[T]], yCPU: Rep[Array[Bit]], mu0CPU: Rep[Array[T]], mu1CPU: Rep[Array[T]]) = {
    val rTileSize     = param(96);  domainOf(rTileSize) = (96, 19200, 1)
    val op      = param(outerPar);  domainOf(op)  = (1, 8, 1)
    val ip      = param(innerPar);  domainOf(ip)  = (1, 12, 1)
    val subLoopPar    = param(1);  domainOf(subLoopPar)    = (1, 16, 1)
    val prodLoopPar   = param(1);  domainOf(prodLoopPar)   = (1, 96, 1)
    val outerAccumPar = param(1);  domainOf(outerAccumPar) = (1, 1, 1)

    val rows = yCPU.length;   bound(rows) = 360000
    val cols = mu0CPU.length; bound(cols) = cc

    assert(cols == cc) // TODO: Shouldn't be necessary, but addressing requires it at the moment

    val R  = rr
    val C  = cc

    val x     = OffChipMem[T](R, C)
    val y     = OffChipMem[Bit](R)
    val mu0   = OffChipMem[T](C)
    val mu1   = OffChipMem[T](C)
    val sigma = OffChipMem[T](C, C)

    setMem(x, xCPU)
    setMem(y, yCPU)
    setMem(mu0, mu0CPU)
    setMem(mu1, mu1CPU)

    Accel {
      val mu0Tile = BRAM[T](cc)
      val mu1Tile = BRAM[T](cc)
      Parallel {
        mu0Tile := mu0(0::cc, subLoopPar)  // Load mu0
        mu1Tile := mu1(0::cc, subLoopPar)  // Load mu1
      }

      val sigmaOut = BRAM[T](cc, cc)

      Fold(R by rTileSize par op, outerAccumPar)(sigmaOut, 0.as[T]){ r =>
        val yTile = BRAM[Bit](rTileSize)
        val xTile = BRAM[T](rTileSize, cc)
        Parallel {
          yTile := y(r::r+rTileSize, subLoopPar)
          xTile := x(r::r+rTileSize, 0::C, subLoopPar)  // Load tile of x
        }

        val sigmaBlk = BRAM[T](cc, cc)
        Fold(rTileSize par ip, prodLoopPar)(sigmaBlk, 0.as[Flt]){rr =>
          val subTile = BRAM[T](cc)
          val sigmaTile = BRAM[T](cc, cc)
          Pipe(C par subLoopPar){ cc =>
            subTile(cc) = xTile(rr,cc) - mux(yTile(rr), mu1Tile(cc), mu0Tile(cc))
          }
          Pipe(C by 1, C par prodLoopPar){ (ii,jj) =>
            sigmaTile(ii,jj) = subTile(ii) * subTile(jj)
          }
          sigmaTile
        }{_+_}
      }{_+_}

      sigma(0::cc, 0::cc, prodLoopPar) := sigmaOut
    }

    getMem(sigma)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }


  def main() {
    val R = rr
    val C = cc //args(1).to[SInt] // TODO: Should be selectable up to maximum

    val x  = Array.fill(R){ Array.fill(C){ random[T](10) }}
    val ys = Array.fill(R){ random[Bit] }
    val mu0 = Array.fill(C){ random[T](10) }
    val mu1 = Array.fill(C){ random[T](10) }

    val result = gda(x.flatten, ys, mu0, mu1)

    // val gold = x.zip(ys){ (row, y) =>
    //   val sub = if (y) row.zip(mu1){_-_} else row.zip(mu0){_-_}
    //   Array.tabulate(C){i => Array.tabulate(C){j => sub(i) * sub(j) }}.flatten
    // }.reduce{(a,b) => a.zip(b){_+_}}

    //println("actual: " + gold.mkString(", "))
    //println("result: " + result.mkString(", "))
    // println("Sum of differences: " + gold.zip(result){_-_}.reduce{_+_})
    // assert( result == gold )
  }
}
