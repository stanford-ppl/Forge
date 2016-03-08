import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object KmeansCompiler extends DHDLApplicationCompiler with Kmeans
object KmeansInterpreter extends DHDLApplicationInterpreter with Kmeans
trait Kmeans extends DHDLApplication {

  override def stageArgNames = List("tileSize", "dim", "numCents")
  lazy val tileSize  = stageArgOrElse[Int](0, 4)
  lazy val dim       = stageArgOrElse[Int](1, 4)
  lazy val numCents  = stageArgOrElse[Int](2, 4)
  lazy val numPoints = ArgIn[Fix]("numPoints")

  def kmeans(points: Rep[OffChipMem[Flt]], centroids: Rep[OffChipMem[Flt]]) = {
    val oldCents = BRAM[Flt](numCents, dim)
    val newCents = BRAM[Flt](numCents, dim)
    val centCount = BRAM[Fix](numCents)

    Sequential {
      // Load initial centroids (from points)
      points.ld(oldCents, 0, 0, numCents, dim)

      Sequential {
        val pointsTile = BRAM[Flt](tileSize, dim)
        Sequential(numPoints by tileSize) { i =>
          points.ld(pointsTile, i, 0, tileSize, dim)

          Sequential(tileSize by 1){ pt =>
            val minDist = Reg[Flt](-1.0f) // Minimum distance to closest centroid
            val minCent = Reg[Fix](0)     // Index of closest centroid

            MetaPipe(numCents by 1){ ct =>
              val dist = Reg[Flt](0.0f)
              Pipe(dim by 1, dist){d => (pointsTile(pt, d) - oldCents(ct, d)) ** 2 }{_+_}

              val closer = dist.value < minDist.value || minDist.value < 0f
              minDist := mux(closer, dist.value, minDist.value)
              minCent := mux(closer, ct, minCent.value)
            }
            // Add point and increment point count
            Parallel {
              Pipe(dim by 1){d => newCents(minCent.value, d) = newCents(minCent.value, d) + pointsTile(pt, d) }
              centCount(minCent.value) = centCount(minCent.value) + 1
            }
          } // End of points in tile
        } // End of point tiles
        Pipe(numCents by 1, dim by 1){(ct,d) =>
          newCents(ct, d) = newCents(ct, d) / centCount(ct).toFltPt
        }
      } // End of metapipe

      centroids.st(newCents, 0, 0, numCents, dim)
    }
  }

  def main() {
    val points = OffChipMem[Flt]("points", numPoints, dim)  // input points
    val centroids = OffChipMem[Flt]("centroids", numCents, dim) // output centroids
    kmeans(points, centroids)
	}
}


object KmeansTestCompiler extends DHDLApplicationCompiler with KmeansTest
object KmeansTestInterpreter extends DHDLApplicationInterpreter with KmeansTest
trait KmeansTest extends Kmeans {
  override def stageArgNames = List("tileSize", "dim", "numCents", "numPoints")
  lazy val snumPoints  = stageArgOrElse[Int](3, 16)

  override def main() {
    val sPoints = Seq.tabulate(snumPoints){i => Seq.tabulate(dim){d => Random.nextInt(100) }}
    val sCents  = Seq.tabulate(numCents){i => Seq.tabulate(dim){d => sPoints(i)(d) }}

    def dist(p1:Seq[Int], p2:Seq[Int]):Int = {
      p1.zip(p2).map{case (d1, d2) => (d1-d2)*(d1-d2)}.reduce(_+_)
    }
    val gold = Array.tabulate(numCents){i => Array.tabulate(dim){d => 0}}

    val closests = sPoints.map{ pt =>
      val distWithIndex = sCents.map{ct => dist(pt, ct) }
      val (minDist, minCent) = distWithIndex.zipWithIndex.reduce{(a,b) => if (a._1 < b._1) a else b}
      gold(minCent).zipWithIndex.foreach{case (d, i) => gold(minCent)(i) = d + pt(i) }
    }
    println("points:")
    sPoints.foreach{pt => println(pt.mkString(",")) }
    println("centroids:")
    gold.foreach{pt => println(pt.mkString(",")) }

    val points = OffChipMem.withInit2D("points", sPoints.map(_.map(_.toFltPt)) )
    val centroids = OffChipMem[Flt]("centroids", numCents, dim)
    kmeans(points, centroids)

    gold.flatten.zipWithIndex.foreach{case (g, i) => assert(centroids.ld(i) == g) }
  }
}
