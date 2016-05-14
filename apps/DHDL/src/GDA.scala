import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

// TODO: How to optimize automatically when cTileSize == cols?
object GDACompiler extends DHDLApplicationCompiler with GDA
object GDAInterpreter extends DHDLApplicationInterpreter with GDA
trait GDA extends DHDLApplication {

  type Elem = Flt

  override def stageArgNames = List("rTileSize", "cTileSize")
  lazy val rows = ArgIn[SInt]("rows")
  lazy val cols = ArgIn[SInt]("cols")

  lazy val rTileSize = param("tileSize", 288)
  lazy val cTileSize = 96
  lazy val outerPar   = param("outerPar", 5)
  lazy val innerPar   = param("innerPar", 2)
  lazy val subLoopPar = param("subLoopPar", 1)
  lazy val prodLoopPar = param("prodLoopPar", 24)
  lazy val outerAccumPar = param("outerAccumPar", 1)  // Not used in old DHDL

  def gda(
    x:     Rep[OffChipMem[Elem]],
    y:     Rep[OffChipMem[Bit]],
    mu0:   Rep[OffChipMem[Elem]],
    mu1:   Rep[OffChipMem[Elem]],
    sub:   Rep[OffChipMem[Elem]],
    sigma: Rep[OffChipMem[Elem]]
  ) {
    val mu0Tile = BRAM[Elem]("mu0Tile", cTileSize)
    val mu1Tile = BRAM[Elem]("mu1Tile", cTileSize)
    Parallel {
      mu0Tile := mu0(0::cTileSize, subLoopPar)  // Load mu0
      mu1Tile := mu1(0::cTileSize, subLoopPar)  // Load mu1
    }

    val sigmaOut = BRAM[Elem]("sigmaOut", cTileSize, cTileSize)

    Pipe.fold((rows by rTileSize) par outerPar, outerAccumPar)(sigmaOut){ r =>
      val yTile = BRAM[Bit]("yTile", rTileSize)
      val xTile = BRAM[Elem]("xTile", rTileSize, cTileSize)
      Parallel {
        yTile := y(r::r+rTileSize, subLoopPar)
        xTile := x(r::r+rTileSize, 0::cTileSize, subLoopPar)  // Load tile of x
      }

      val sigmaBlk = BRAM[Elem]("sigmaBlk", cTileSize, cTileSize)
      Pipe.fold((rTileSize by 1) par innerPar, prodLoopPar)(sigmaBlk){rr =>
        val subTile = BRAM[Elem]("subTile", cTileSize)
        val sigmaTile = BRAM[Elem]("sigmaTile", cTileSize, cTileSize)
        Pipe((cTileSize by 1) par subLoopPar){ cc =>
          subTile(cc) = xTile(rr,cc) - mux(yTile(rr), mu1Tile(cc), mu0Tile(cc))
        }
        Pipe(cTileSize by 1, (cTileSize by 1) par prodLoopPar){ (ii,jj) =>
          sigmaTile(ii,jj) = subTile(ii) * subTile(jj)
        }
        sigmaTile
      }{_+_}
    }{_+_}

    sigma(0::cTileSize, 0::cTileSize, prodLoopPar) := sigmaOut
  }

  def main() {
    val R = args(unit(0)).to[SInt];   bound(R) = 360000
    val C = args(unit(0)).to[SInt];   bound(C) = 96
    domainOf(rTileSize)  = (96,19200,1) // 160
    domainOf(outerPar)   = (1,4,1)      // 4
    domainOf(innerPar)   = (1,12,1)      // 6
    domainOf(subLoopPar) = (1,16,1)     // 10
    domainOf(prodLoopPar) = (1,96,1)    // 24
    domainOf(outerAccumPar) = (1,1,1)  // 24

    val x = OffChipMem[Elem]("x", R, C)
    val y = OffChipMem[Bit]("y", R)
    val mu0 = OffChipMem[Elem]("mu0", C)
    val mu1 = OffChipMem[Elem]("mu1", C)
    val sub = OffChipMem[Elem]("sub", C)
    val sigma = OffChipMem[Elem]("sigma", C, C)

    val sX = Array.fill(R){ Array.fill(C){ random[Elem](10) }}
    val sY = Array.fill(R){ random[Bit] }
    val sMu0 = Array.fill(C){ random[Elem](10) }
    val sMu1 = Array.fill(C){ random[Elem](10) }

    //println("x:   " + sX.map(_.mkString(", ")).mkString("\n") )
    //println("y:   " + sY.mkString("\n\t"))
    //println("mu0: " + sMu0.mkString(", "))
    //println("mu1: " + sMu1.mkString(", "))

    // Transfer data and start accelerator
    setArg(rows, R)
    setArg(cols, C)
    setMem(x, sX.flatten)
    setMem(y, sY)
    setMem(mu0, sMu0)
    setMem(mu1, sMu1)
    Accel{ gda(x, y, mu0, mu1, sub, sigma) }

    val gold = sX.zip(sY){ (row, y) =>
      val sub = if (y) row.zip(sMu1){_-_} else row.zip(sMu0){_-_}
      Array.tabulate(C){i => Array.tabulate(C){j => sub(i) * sub(j) }}.flatten
    }.reduce{(a,b) => a.zip(b){_+_}}

    val result = getMem(sigma)

    println("actual: " + gold.mkString(", "))
    println("result: " + result.mkString(", "))
    println("diff:   " + gold.zip(result){_-_}.mkString(", "))
    assert( result == gold )
  }
}
