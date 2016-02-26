import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object MatMultCompiler extends DHDLApplicationCompiler with MatMult
object MatMultInterpreter extends DHDLApplicationInterpreter with MatMult

trait MatMult extends DHDLApplication {
  def printUsage = {
    println("Usage: matmult")
    exit(-1)
	}
	//def matmultTile(tileA: Rep[BRAM[FixPt]], tileB: Rep[BRAM[FixPt]], tileC: Rep[BRAM[FixPt]],
	//	bm:Int, bn:Int, bp:Int) = {
	//	val ijCounter = CounterChain(Counter(bn, 1), Counter(bn, 1))
	//	Sequential(ijCounter) {case i::j::_ =>
	//		val accum = Reg[FixPt](0)
	//		val kCounter = CounterChain(Counter(bp, 1))
	//		Pipe[FixPt](true, kCounter, accum, _+_) {case k::_ =>
	//			val x = tileA.ld(i, k)
	//			val y = tileB.ld(k, j)
	//			x * y
	//		}
	//		Pipe {
	//			tileC.st(i, j, accum.value)
	//		}
	//		()
	//	}
	//}

  def main() = {
    val bm = 2
    val bn = 2
    val bp = 2
    val sM = 4
    val sN = 4
    val sP = 2
    val sM1 = Seq.fill(sM)(Seq.fill(sP)(Random.nextInt(100)))
    val sM2 = Seq.fill(sP)(Seq.fill(sN)(Random.nextInt(100)))
    println("M1")
    sM1.map{_.mkString(",")}.mkString("\n")
    println("M2")
    sM2.map{_.mkString(",")}.mkString("\n")
    val M3Gold = Seq.tabulate(sM){i =>
      val v1 = sM1(i)
      Seq.tabulate(sN) {j =>
        val v2 = sM2.map(row => row(j))
        v1.zip(v2).map{case (a,b) => a*b}.reduce(_+_)
      }
    }
    println("M3Gold")
    M3Gold.map{_.mkString(",")}.mkString("\n")

		val M = ArgIn[FixPt](sM).value
		val N = ArgIn[FixPt](sN).value
		val P = ArgIn[FixPt](sP).value
		val M1 = OffChipMem[FixPt]("M1", sM1.flatten.map(i => i.toFixPt): _*)
		val M2 = OffChipMem[FixPt]("M2", sM2.flatten.map(i => i.toFixPt): _*)
		val M3 = OffChipMem[FixPt]("M3", M*N)
		//val ijCounter = CounterChain(Counter(M, bm), Counter(N,bn))

		//MetaPipe(true, ijCounter) {case i::j::_ =>
		//	val m1Tile = BRAM[FixPt](bm, bp)
		//	val m2Tile = BRAM[FixPt](bp, bn)
		//	val multTile = BRAM[FixPt](bm, bn)
		//	val kCounter = CounterChain(Counter(P, bp))
		//	//BramReduce[FixPt](true, kCounter, multTile, (_+_)) {case k::_ =>
		//	//	Parallel{
		//	//		M1.ld(m1Tile, i, k, bm, bp, M)
		//	//		M2.ld(m2Tile, k, j, bp, bn, P)
		//	//	}
		//	//	val result = BRAM[FixPt](bm*bn)
		//	//	//matmultTile(m1Tile, m2Tile, result, bm, bn, bp)
		//	//	//println(result)
		//	//	result
		//	//}
		//	()
		//}

  }
}
