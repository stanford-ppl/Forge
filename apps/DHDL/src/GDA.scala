import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

// TODO: Optimize automatically when cTileSize == cols?
object GDACompiler extends DHDLApplicationCompiler with GDA
object GDAInterpreter extends DHDLApplicationInterpreter with GDA
trait GDA extends DHDLApplication {

  override def stageArgNames = List("rTileSize", "cTileSize")
  lazy val rTileSize = stageArgOrElse[Int](0, 2)
  lazy val cTileSize = stageArgOrElse[Int](1, 4)
  lazy val rows = ArgIn[Fix]("rows")
  lazy val cols = ArgIn[Fix]("cols")

  def gda(x: Rep[OffChipMem[Flt]], y: Rep[OffChipMem[Bit]], mu0: Rep[OffChipMem[Flt]], mu1: Rep[OffChipMem[Flt]]) = {

    val sub = OffChipMem[Flt]("sub", cols)
    val sigma = OffChipMem[Flt]("sigma", cols, cols)

    MetaPipe(rows by rTileSize){ r =>
      val yTile = BRAM[Bit]("yTile", rTileSize)
      y.ld(yTile, r, rTileSize)

      Sequential(rTileSize by 1){ rr =>
        MetaPipe(cols by cTileSize){ c =>
          val xTile = BRAM[Flt]("xTile", rTileSize, cTileSize)
          val mu0Tile = BRAM[Flt]("mu0Tile", cTileSize)
          val mu1Tile = BRAM[Flt]("mu1Tile", cTileSize)
          Parallel {
            x.ld(xTile, r, c, rTileSize, cTileSize) // Load a tile of x
            mu0.ld(mu0Tile, c, cTileSize)           // Load a tile of mu0
            mu1.ld(mu1Tile, c, cTileSize)           // Load a tile of mu1
          }
          val subTile = BRAM[Flt]("subTemp", cTileSize)
          Pipe(cTileSize by 1){ cc =>
            subTile(cc) = xTile(rr,cc) - mux(yTile(rr), mu0Tile(cc), mu1Tile(cc))
          }
          sub.st(subTile, c, cTileSize)
        }
        MetaPipe(cols by cTileSize, cols by cTileSize){ (i,j) =>
          val subTile1 = BRAM[Flt]("subTile1", cTileSize)
          val subTile2 = BRAM[Flt]("subTile2", cTileSize)
          val sigmaTile = BRAM[Flt]("sigmaTile", cTileSize, cTileSize)
          Parallel {
            sub.ld(subTile1, i, cTileSize)
            sub.ld(subTile2, j, cTileSize)
            sigma.ld(sigmaTile, i, j, cTileSize, cTileSize)
          }
          Pipe(cTileSize by 1, cTileSize by 1){ (ii,jj) =>
            sigmaTile(ii,jj) = sigmaTile(ii,jj) + subTile1(ii) * subTile2(jj)
          }
          sigma.st(sigmaTile, i, j, cTileSize, cTileSize)
        }
      }
    }
    sigma
  }

  def main() {
    val x = OffChipMem[Flt]("x", rows, cols)
    val y = OffChipMem[Bit]("y", rows)
    val mu0 = OffChipMem[Flt]("mu0", cols)
    val mu1 = OffChipMem[Flt]("mu1", cols)
    gda(x, y, mu0, mu1)
  }
}


object GDATestCompiler extends DHDLApplicationCompiler with GDATest
object GDATestInterpreter extends DHDLApplicationInterpreter with GDATest
trait GDATest extends GDA {

  override def stageArgNames = List("rTileSize", "cTileSize", "rows", "cols")
  lazy val srows = stageArgOrElse[Int](2, 6)
  lazy val scols = stageArgOrElse[Int](3, 8)

  override def main() {
    val sX = Seq.tabulate(srows){i => Seq.tabulate(scols){j => util.Random.nextInt(100)}}
    val sY = Seq.tabulate(srows){i => util.Random.nextBoolean()}
    val sMu0 = Seq.tabulate(scols){i => util.Random.nextFloat()*100}
    val sMu1 = Seq.tabulate(scols){i => util.Random.nextFloat()*100}

    val x = OffChipMem.withInit2D("x", sX.map(_.map(_.toFltPt)))
    val y = OffChipMem.withInit1D("y", sY.map(_.toBit))
    val mu0 = OffChipMem.withInit1D("mu0", sMu0.map(_.toFltPt))
    val mu1 = OffChipMem.withInit1D("mu1", sMu1.map(_.toFltPt))

    val gold = Array.fill(scols,scols){ 0f }
    sY.zipWithIndex.foreach{ case (yi, r) =>
      val sub = if (yi) (sX(r), sMu1).zipped.map(_-_) else (sX(r), sMu0).zipped.map(_-_)

      gold.zipWithIndex.foreach{ case(row, i) =>
        row.zipWithIndex.foreach{ case(elem, j) =>
          gold(i)(j) = elem + sub(i)*sub(j)
        }
      }
    }

    val sigma = gda(x, y, mu0, mu1)

    gold.flatten.zipWithIndex.foreach{ case (g,i) => assert(sigma.ld(i) == g) }
  }
}