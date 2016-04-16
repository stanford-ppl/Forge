import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object KmeansCompiler extends DHDLApplicationCompiler with Kmeans
object KmeansInterpreter extends DHDLApplicationInterpreter with Kmeans
trait Kmeans extends DHDLApplication {

  override def stageArgNames = List("tileSize", "dim", "numCents")
  lazy val dim       = ArgIn[SInt]("dimension")
  lazy val numCents  = ArgIn[SInt]("numCents")
  lazy val numPoints = ArgIn[SInt]("numPoints")

  lazy val tileSize   = param(210)
  lazy val dTileSize  = 96
  lazy val kTileSize  = 16
  lazy val ptLoopPar  = unit(1)
  lazy val ctLoopPar  = param(1)
  lazy val dstLoopPar = param(4)
  lazy val accLoopPar = param(4)
  lazy val avgLoopPar = param(1)

  def kmeans(points: Rep[OffChipMem[Flt]], centroids: Rep[OffChipMem[Flt]]) = {
    val oldCents = BRAM[Flt](kTileSize, dTileSize)
    val newCents = BRAM[Flt](kTileSize, dTileSize)
    val centCount = BRAM[UInt](kTileSize)
    val centsOut = BRAM[Flt](kTileSize, dTileSize)

    Sequential {
      // Load initial centroids (from points)
      oldCents := points(0::kTileSize,0::dTileSize, ctLoopPar)

      MetaPipe((numPoints by tileSize) par unit(1)) { i =>
        val pointsTile = BRAM[Flt](tileSize, dTileSize)
        pointsTile := points(i::i+tileSize, 0::dTileSize, dstLoopPar)

        MetaPipe((tileSize by 1) par ptLoopPar){ pt =>
          val minDist = Reg[Flt](-1.0f) // Minimum distance to closest centroid
          val minCent = Reg[SInt](0)    // Index of closest centroid

          MetaPipe((kTileSize by 1) par ctLoopPar){ ct =>
            val dist = Reg[Flt](0.0f)
            Pipe((dTileSize by 1) par dstLoopPar, dist){d => (pointsTile(pt, d) - oldCents(ct, d)) ** 2 }{_+_}

            Pipe {
              val closer = dist.value < minDist.value || minDist.value < 0f
              minDist := mux(closer, dist.value, minDist.value)
              minCent := mux(closer, ct, minCent.value)
            }
          }
          // Add point and increment point count
          Parallel {
            Pipe((dTileSize by 1) par accLoopPar){d =>
              newCents(minCent.value, d) = newCents(minCent.value, d) + pointsTile(pt, d)
            }
            Pipe{ centCount(minCent.value) = centCount(minCent.value) + 1 }
          }
        } // End of points in tile
      } // End of point tiles

      Pipe(kTileSize by 1, (dTileSize by 1) par avgLoopPar){(ct,d) =>
        centsOut(ct, d) = newCents(ct, d) / centCount(ct).to[Flt]
      }

      // TODO: Change parallelization here
      centroids(0::kTileSize, 0::dTileSize, unit(1)) := centsOut
    }
  }

  def main() {
    val N = args(unit(0)).to[SInt];   bound(N) = 960000
    val K = args(unit(0)).to[SInt];   bound(K) = 16
    val D = args(unit(0)).to[SInt];   bound(D) = 96
    domainOf(tileSize) = (1,960,10)
    domainOf(ctLoopPar) = (1,16,1)
    domainOf(dstLoopPar) = (1,16,1)
    domainOf(accLoopPar) = (1,16,1)
    domainOf(avgLoopPar) = (1,16,1)


    val points = OffChipMem[Flt]("points", N, D)       // input points
    val centroids = OffChipMem[Flt]("centroids", K, D) // output centroids

    val pts = Array.tabulate(N){i => Array.tabulate(D){d => random[Flt](10) }}

    setMem(points, pts.flatten)
    setArg(numPoints, N)
    setArg(numCents,  K)
    setArg(dim, D)

    println("points: ")
    for (i <- 0 until N) { println(i.mkString + ": " + pts(i).mkString(", ")) }

    Accel{ kmeans(points, centroids) }

    val cts = Array.tabulate(K){i => pts(i) }

    val gold = Array.empty[ForgeArray[Flt]](numCents) // ew
    val counts = Array.empty[UInt](numCents)
    for (i <- 0 until numCents) {
      gold(i) = Array.fill(D)(0.as[Flt])  // TODO: Fix
    }
    for (i <- 0 until K) { counts(i) = 0.as[UInt] }
    // Really bad imperative version
    def dist(p1: Rep[ForgeArray[Flt]], p2: Rep[ForgeArray[Flt]]) = p1.zip(p2){(a,b) => (a - b)**2 }.reduce(_+_)
    for (i <- 0 until N) {
      val pt = pts(i)
      val distWithIndex = cts.map{ct => dist(pt, ct) }.zipWithIndex
      val minIdx = distWithIndex.reduce{(a,b) => if (a._1 < b._1) a else b }._2

      counts(minIdx) = counts(minIdx) + 1
      for (j <- 0 until D) {
        gold(minIdx)(j) = gold(minIdx).apply(j) + pt(j)
      }

      println(counts.mkString(", "))
      for (x <- 0 until K) { println(gold(x).mkString(", ")) }
    }
    val actual = gold.zip(counts){(ct,n) => ct.map{p => p / n.to[Flt] }}.flatten
    println("gold:   " + actual.mkString(", "))

    val result = getMem(centroids)
    println("result: " + result.mkString(", "))
    //assert( actual == result )
  }
}
