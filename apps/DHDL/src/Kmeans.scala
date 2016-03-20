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
  lazy val numPoints = ArgIn[SInt]("numPoints")

  def kmeans(points: Rep[OffChipMem[Flt]], centroids: Rep[OffChipMem[Flt]]) = {
    val oldCents = BRAM[Flt](numCents, dim)
    val newCents = BRAM[Flt](numCents, dim)
    val centCount = BRAM[UInt](numCents)

    Sequential {
      // Load initial centroids (from points)
      oldCents := points(0::numCents,0::dim)

      Sequential {
        val pointsTile = BRAM[Flt](tileSize, dim)
        Sequential(numPoints by tileSize) { i =>
          pointsTile := points(i::i+tileSize, 0::dim) // TODO: change 0::dim with just *

          Sequential(tileSize by 1){ pt =>
            val minDist = Reg[Flt](-1.0f) // Minimum distance to closest centroid
            val minCent = Reg[SInt](0)    // Index of closest centroid

            MetaPipe(numCents by 1){ ct =>
              val dist = Reg[Flt](0.0f)
              Pipe(dim by 1, dist){d => (pointsTile(pt, d) - oldCents(ct, d)) ** 2 }{_+_}

              val closer = dist.value < minDist.value || minDist.value < 0f
              minDist := mux(closer, dist.value, minDist.value)
              minCent := mux(closer, ct, minCent.value)
            }
            // Add point and increment point count
            Parallel {
              Pipe(dim by 1){d =>
                newCents(minCent.value, d) = newCents(minCent.value, d) + pointsTile(pt, d)
              }
              centCount(minCent.value) = centCount(minCent.value) + 1
            }
          } // End of points in tile
        } // End of point tiles
        Pipe(numCents by 1, dim by 1){(ct,d) =>
          newCents(ct, d) = newCents(ct, d) / centCount(ct).to[Flt]
        }
      } // End of metapipe

      centroids(0::numCents, 0::dim) := newCents  // TODO: Change to centroids(*,*) := newCents ?
    }
  }

  def main() {
    val N = 16

    val points = OffChipMem[Flt]("points", N, dim)  // input points
    val centroids = OffChipMem[Flt]("centroids", numCents, dim) // output centroids

    val pts = Array.tabulate(N){i => Array.tabulate(dim){d => random[Flt](10) }}

    setMem(points, pts.flatten)
    setArg(numPoints, N)

    Accel{ kmeans(points, centroids) }

    val cts = Array.tabulate(numCents){i => pts(i) }

    val gold = Array.empty[ForgeArray[Flt]](numCents) // ew
    for (i <- 0 until numCents) { gold(i) = Array.fill(dim)(0.as[Flt]) }

    // Really bad imperative version
    def dist(p1: Rep[ForgeArray[Flt]], p2: Rep[ForgeArray[Flt]]) = p1.zip(p2){(a,b) => (a - b)**2 }.reduce(_+_)
    for (i <- 0 until N) {
      val pt = pts(i)
      val distWithIndex = cts.map{ct => dist(pt, ct) }.zipWithIndex
      val minIdx = distWithIndex.reduce{(a,b) => if (a._1 < b._1) a else b }._2
      for (j <- 0 until dim) {
        gold(minIdx)(j) = gold(minIdx).apply(j) + pt(j)
      }
    }

    val result = getMem(centroids)

    println("gold:   " + gold.flatten.mkString(", "))
    println("result: " + result.mkString(", "))
    assert( gold.flatten == result )
  }
}
