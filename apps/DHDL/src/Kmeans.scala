import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object KmeansCompiler extends DHDLApplicationCompiler with Kmeans 
object KmeansInterpreter extends DHDLApplicationInterpreter with Kmeans

trait KmeansTest {
	def test (sPoints:Seq[Seq[Int]], sNumCents:Int, sDim:Int) = {
		val sCents = Seq.tabulate(sNumCents){i =>
			sPoints(i)
		}
		def dist(p1:Seq[Int], p2:Seq[Int]):Int = {
			p1.zip(p2).map{case (d1, d2) => (d1-d2)*(d1-d2)}.reduce(_+_)
		}
		var gold = Array.tabulate(sNumCents){i =>
			Array.tabulate(sDim){d => 0}
		}
		val closests = sPoints.map{p =>
			var minCent = 0
			var minDist = -1 
			sCents.zipWithIndex.foreach{case (c, i) =>
				val currDist = dist(p, c)
				if ((currDist<minDist) || (minDist < 0)) {
					minDist = currDist
					minCent = i
				}
			}
			gold(minCent).zipWithIndex.foreach{case (d, i) => 
				gold(minCent)(i) = d + p(i)
			} 
		}
		println("points:")
		println(sPoints.map(p => p.mkString(",")).mkString("\n"))
		println("cents:")
		println(sCents.map(c => c.mkString(",")).mkString("\n"))
		println("newCents:")
		println(gold.map(c => c.mkString(",")).mkString("\n"))
	}
}
trait Kmeans extends DHDLApplication with KmeansTest{
	def printUsage = {
    println("Usage: kmeans")
    exit(-1)
	}
  def main() = {
		val tileSize = 4
		val sDim = 4
		val sNumPoints = 16
		val sNumCents = 4
		val sPoints = Seq.tabulate(sNumPoints){i =>
			Seq.tabulate(sDim){d => 
				Random.nextInt(100)
			}
		}

		test(sPoints, sNumCents, sDim)

		val numPoints = ArgIn[FixPt](sNumPoints).value
		val numCents = ArgIn[FixPt](sNumCents).value

		val points = OffChipMem[FixPt](sPoints.flatten.map(i => i.toFixPt): _*)
		val oldCents = BRAM[FixPt](sNumCents, sDim)
		points.ld(oldCents, 0, sNumCents*sDim)

		val tileCtr = CounterChain(Counter(max=numPoints, step=tileSize))
		val pointsB = BRAM[FixPt](tileSize, sDim)
		Sequential(tileCtr) { case iTile::_ => 
			points.ld(pointsB, iTile, tileSize*sDim)
			val ptCtr = CounterChain(Counter(max=tileSize))
			MetaPipe(true, ptCtr) { case iP::_ =>
				val ctCtr = CounterChain(Counter(max=numCents))
				MetaPipe(true, ctCtr) { case iC::_ =>
					val dimCtr = CounterChain(Counter(max=sDim))
					val dist = Reg[FixPt](0)
					Pipe[FixPt](dimCtr, dist, _+_) { case iD::_ =>
						(pointsB.ld(iP, iD) - oldCents.ld(iC, iD)).pow(2)
					}

				}
			}
		}
		println("DHDL:")
		println("points")
		println(points.mkString)
		println("cents")
		println(oldCents.mkString)

		/*
		val ctrs_out = CounterChain(Counter(max=dataSize/tileSize))
		val accum_out = ArgOut[FixPt](0)
		MetaPipe[FixPt](1, true, ctrs_out, accum_out, _+_) {case i::_ => 
			val bm1 = BRAM[FixPt]("bm1", tileSize)
			val bm2 = BRAM[FixPt]("bm2", tileSize)
			Parallel {
				vec1.ld(bm1, i*tileSize, tileSize)
				vec2.ld(bm2, i*tileSize, tileSize)
			}
			val accum_in = Reg[FixPt]()
			val ctrs_in = CounterChain(Counter(max=tileSize))
			Pipe[FixPt](1, true, ctrs_in, accum_in, _+_) { case j::_ =>
				bm1.ld(j)*bm2.ld(j)
			}
			accum_in.value
		}

		assert(accum_out.value==FixPt(gold))
		*/
	}
}
