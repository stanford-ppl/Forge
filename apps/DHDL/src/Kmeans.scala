import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object KmeansCompiler extends DHDLApplicationCompiler with Kmeans
object KmeansInterpreter extends DHDLApplicationInterpreter with Kmeans
trait Kmeans extends DHDLApplication {
  type Array[T] = ForgeArray[T]

  lazy val MAXK = 8
  lazy val MAXD = 384

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

  def kmeans(points_in: Rep[Array[Flt]], numPoints: Rep[SInt], numCents: Rep[SInt], numDims: Rep[SInt]) = {
    bound(numPoints) = 960000
    bound(numCents) = MAXK
    bound(numDims) = MAXD

    val BN = param(320);  domainOf(BN) = (96, 9600, 96)
    val BD = param(384);  domainOf(BD) = (MAXD, MAXD, MAXD)
    val PX = param(1);    domainOf(PX) = (1,1,1)
    val P0 = param(1);    domainOf(P0) = (1,96,1)     // Dimensions loaded in parallel
    val P1 = param(1);    domainOf(P1) = (1,12,1)     // Sets of points calculated in parallel
    val P2 = param(1);    domainOf(P2) = (1,MAXD,1)   // Dimensions accumulated in parallel (outer)
    val P3 = param(1);    domainOf(P3) = (1,16,1)     // Points calculated in parallel
    val P4 = param(1);    domainOf(P4) = (1,MAXD,1)   // Dimensions accumulated in parallel (inner)
    val P5 = param(1);    domainOf(P5) = (1,MAXD,1)   // Dimensions compared in parallel
    val P6 = param(1);    domainOf(P6) = (1,MAXD,1)   // Dimensions saved in parallel
    val P7 = param(1);    domainOf(P7) = (1,MAXK,1)   // Centroid counts copied in parallel
    val P8 = param(1);    domainOf(P8) = (1,MAXD,1)   // Dimensions averaged in parallel
    val P9 = param(1);    domainOf(P9) = (1,MAXD,1)   // Dimensions stored in parallel

    val N = ArgIn[SInt]
    val K = ArgIn[SInt]
    val D = ArgIn[SInt]
    setArg(N, numPoints)
    setArg(K, numCents)
    setArg(D, numDims)

    val points = OffChipMem[Flt](N, D)    // Input points
    val centroids = OffChipMem[Flt](K, D) // Output centroids
    setMem(points, points_in)

    Accel {
      val cts = BRAM[Flt](MAXK, MAXD)

      val DP1 = Reg[SInt]
      Pipe { DP1 := D + 1 }

      // Load initial centroids (from points)
      cts := points(0::K,0::D, P0)

      val newCents = BRAM[Flt](MAXK,MAXD+1)
      // For each set of points
      Fold(N by BN par P1, P2)(newCents, 0.as[Flt]){i =>
        val pts = BRAM[Flt](BN, BD)
        pts := points(i::i+BN, 0::BD, P0)

        val centTile = BRAM[Flt](MAXK, MAXD+1)
        // For each point in this set
        Fold(BN par P3, P4)(centTile, 0.as[Flt]){pt =>
          val minCent = Reg[SInt](0)  // Index of closest centroid
          val minDist = Reg[Flt](-1)  // Distance to closest centroid

          // Find the index of the closest centroid
          Pipe(K par PX){ct =>
            val dist = Reduce(D par P5)(0.as[Flt]){d => (pts(pt,d) - cts(ct,d)) ** 2 }{_+_}
            Pipe {
              minDist := min(dist.value, minDist.value)
              minCent := mux(minDist.value == dist.value, ct, minCent.value)
            }
          }

          // Store this point to the set of accumulators
          val localCent = BRAM[Flt](MAXK,MAXD+1)
          Pipe(K by 1, DP1 par P6){(ct,d) =>
            val elem = mux(d == D, 1.as[Flt], pts(pt, d))
            localCent(ct, d) = mux(ct == minCent.value, elem, 0.as[Flt])
          }
          localCent
        }{_+_} // Add the current point to the accumulators for this centroid
      }{_+_}

      val centCount = BRAM[Flt](MAXK)
      Pipe(K par P7){ct => centCount(ct) = newCents(ct,D+1) }

      // Average each new centroid
      val centsOut = BRAM[Flt](MAXK, MAXD)
      Pipe(K by 1, D par P8){(ct,d) =>
        centsOut(ct, d) = newCents(ct,d) / centCount(ct)
      }
      // Store the centroids out
      centroids(0::K,0::D,P9) := centsOut
    }

    getMem(centroids)
  }

  def main() {
    val N = args(0).to[SInt];
    val K = MAXK.as[SInt];
    val D = MAXD.as[SInt];

    val pts = Array.tabulate(N){i => Array.tabulate(D){d => random[Flt](10) }}

    println("points: ")
    for (i <- 0 until N) { println(i.mkString + ": " + pts(i).mkString(", ")) }

    val result = kmeans(pts.flatten, N, K, D)

    val cts = Array.tabulate(K){i => pts(i) }

    val gold = Array.empty[ForgeArray[Flt]](K) // ew
    val counts = Array.empty[UInt](K)
    for (i <- 0 until K) {
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
    println("result: " + result.mkString(", "))
    //assert( actual == result )
  }

  /*def kmeans_SLOW(points_in: Rep[Array[Flt]], numPoints: Rep[SInt], numCents: Rep[SInt], numDims: Rep[SInt]) = {
    bound(numPoints) = 960000
    bound(numCents) = MAXK
    bound(numDims) = MAXD

    val BN = param(320);  domainOf(BN) = (96, 9600, 96)
    val BD = param(384);  domainOf(BD) = (MAXD, MAXD, MAXD)
    val PX = param(1);    domainOf(PX) = (1,1,1)
    val P0 = param(1);    domainOf(P0) = (1,96,1)     // Dimensions loaded in parallel
    val P1 = param(1);    domainOf(P1) = (1,96,1)     // Points calculated in parallel
    val P2 = param(1);    domainOf(P2) = (1,MAXD,1)   // Dimensions subtracted in parallel
    val P3 = param(1);    domainOf(P3) = (1,MAXD,1)   // Dimensions updated in parallel
    val P4 = param(1);    domainOf(P4) = (1,MAXD,1)   // Dimensions averaged in parallel
    val P5 = param(1);    domainOf(P5) = (1,MAXD,1)   // Dimensions stored in parallel

    val N = ArgIn[SInt]
    val K = ArgIn[SInt]
    val D = ArgIn[SInt]
    setArg(N, numPoints)
    setArg(K, numCents)
    setArg(D, numDims)

    val points = OffChipMem[Flt](N, D)    // Input points
    val centroids = OffChipMem[Flt](K, D) // Output centroids
    setMem(points, points_in)

    Accel {
      val cts = BRAM[Flt](MAXK, dTileSize)
      val newCents = BRAM[Flt](MAXK, dTileSize)
      val centCount = BRAM[UInt](MAXK)
      val centsOut = BRAM[Flt](MAXK, dTileSize)

      // Load initial centroids (from points)
      cts := points(0::K,0::dTileSize, P0)

      // For each set of points
      Pipe(N by BN par PX){i =>
        val pts = BRAM[Flt](BN, BD)
        pts := points(i::i+BN, 0::BD, P0)

        // For each point in this set
        Pipe(BN par PX){pt =>
          val minCent = Reg[SInt](0)  // Index of closest centroid
          val minDist = Reg[Flt](-1)  // Distance to closest centroid

          // Find the index of the closest centroid
          Pipe(K by 1 par PX){ct =>
            val dist = Reduce(D par P2)(0.as[Flt]){d => (pts(pt,d) - cts(ct,d)) ** 2 }{_+_}

            Pipe {
              minDist := min(dist.value, minDist.value)
              minCent := mux(minDist.value == dist.value, ct, minCent.value)
            }
          }

          // Add the current point to the accumulators for this centroid
          Parallel {
            Pipe(D par P3){d => newCents(minCent.value,d) = newCents(minCent.value,d) + pts(pt,d) }
            Pipe { centCount(minCent.value) = centCount(minCent.value) + 1 }
          }
        }
      }
      // Average each new centroid
      Pipe(K by 1, D par P4){(ct,d) =>
        centsOut(ct, d) = newCents(ct,d) / centCount(ct).to[Flt]
      }
      // Store the centroids out
      centroids(0::K,0::D,P5) := centsOut
    }

    getMem(centroids)
  }*/

}
