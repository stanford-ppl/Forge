import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object KmeansCompiler extends DHDLApplicationCompiler with Kmeans
object KmeansInterpreter extends DHDLApplicationInterpreter with Kmeans
trait Kmeans extends DHDLApplication {

  lazy val dim       = ArgIn[SInt]
  lazy val numCents  = ArgIn[SInt]
  lazy val numPoints = ArgIn[SInt]

  lazy val tileSize   = param(320)
  lazy val dTileSize  = 96
  lazy val ptLoopPar  = param(1)
  //lazy val ctLoopPar  = param(1)
  lazy val dstLoopPar = param(1)
  lazy val accLoopPar = param(1)
  lazy val avgLoopPar = param(1)
  lazy val ignorePar = param(1)
  lazy val MAXK = 8

  lazy val loadPar = param(96)

  def reduceTree(x: List[(Rep[Flt], Rep[SInt])]): List[(Rep[Flt], Rep[SInt])] = {
    if (x.length == 1) x
    else reduceTree(List.tabulate(x.length/2){i =>
      val (dist1,index1) = x(2*i)
      val (dist2,index2) = x(2*i+1)
      val closer = dist1 < dist2
      val minDist = mux(closer, dist1, dist2)
      val minIndx = mux(closer, index1, index2)
      (minDist, minIndx)
    })
  }

  def kmeans(points: Rep[OffChipMem[Flt]], centroids: Rep[OffChipMem[Flt]], K: Rep[SInt], D: Rep[SInt]) = {
    val oldCents = BRAM[Flt](MAXK, dTileSize)
    val newCents = BRAM[Flt](MAXK, dTileSize)
    val centCount = BRAM[UInt](MAXK)
    val centsOut = BRAM[Flt](MAXK, dTileSize)

    // Runtime is roughly (N*K/(Pc)*(D/Pd + 19 + log2(D))

    // Load initial centroids (from points)
    oldCents := points(0::K,0::dTileSize, dstLoopPar)

    Pipe((numPoints by tileSize) par ignorePar) { i =>
      val pointsTile = BRAM[Flt](tileSize, dTileSize)
      pointsTile := points(i::i+tileSize, 0::dTileSize, dstLoopPar)

      Pipe((tileSize by 1) par ptLoopPar){ pt =>
        Pipe(numCents by 1){ct =>
          val dist = Reg[Flt]
          Pipe.reduce((D by 1) par dstLoopPar)(dist){d => (pointsTile(pt,d) - oldCents(ct,d)) ** 2 }{_+_}

          val minCent = Reg[SInt](0)
          val minDist = Reg[Flt](-1)
          Pipe {
            val closer = dist.value < minDist.value || minDist.value < 0
            minCent := mux(closer, ct, minCent.value)
            minDist := mux(closer, dist.value, minDist.value)
          }

          Parallel {
            Pipe((D by 1) par accLoopPar){d =>
              newCents(minCent.value, d) = newCents(minCent.value, d) + pointsTile(pt, d)
            }
            Pipe{ centCount(minCent.value) = centCount(minCent.value) + 1 }
          }
        }
      } // End of points in tile
    } // End of point tiles

    Pipe(K by 1, (D by 1) par avgLoopPar){(ct,d) =>
      centsOut(ct,d) = newCents(ct,d) / centCount(ct).to[Flt]
    }
    centroids(0::K, 0::D, avgLoopPar) := centsOut

  }

  def main() {
    val N = args(0).to[SInt];   bound(N) = 960000
    val K = args(1).to[SInt];   bound(K) = 8
    val D = args(2).to[SInt];   bound(D) = 384
    domainOf(tileSize) = (1,9600,1)
    //domainOf(ctLoopPar) = (1,1,1)
    domainOf(dstLoopPar) = (1,96,1)
    domainOf(accLoopPar) = (1,96,1)
    domainOf(avgLoopPar) = (1,96,1)
    domainOf(ptLoopPar) = (1,1,1)
    domainOf(ignorePar) = (1,1,1)
    domainOf(loadPar) = (96,96,1)

    val points = OffChipMem[Flt](N, D)      // input points
    val centroids = OffChipMem[Flt](K, D)   // output centroids

    val pts = Array.tabulate(N){i => Array.tabulate(D){d => random[Flt](10) }}

    setMem(points, pts.flatten)
    setArg(numPoints, N)
    setArg(numCents,  K)
    setArg(dim, D)

    println("points: ")
    for (i <- 0 until N) { println(i.mkString + ": " + pts(i).mkString(", ")) }

    Accel{ kmeans(points, centroids, K, D) }

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
