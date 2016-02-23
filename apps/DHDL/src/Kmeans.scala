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

		val points = OffChipMem[FixPt]("points", sPoints.flatten.map(i => i.toFixPt): _*)
		val cents = BRAM[FixPt]("cents", tileSize*sNumCents)
		points.ld(cents,0, 0, sDim, sNumCents, sDim)

		val ptCtr = CounterChain(Counter(max=5))
		val accum1 = Reg[FixPt](0)
		val accum2 = Reg[FixPt](0)
		MetaReduceMany[FixPt](1, true, ptCtr, Seq(accum1, accum2), Seq(_+_, _+_), {case i::_ => 
			println(i)
			Seq(i, i)
		})
		println(accum1.value)
		println(accum2.value)
		//val ptCtr = CounterChain(Counter(max=sDim))
		//MetaPipe()
		//
		//val dimCtr = CounterChain(Counter(max=sDim))
		//Pipe[Float](1, true, dimCtr, distAccum, _+_, {case i::_ =>
		//	
		//})
		println("DHDL:")
		println("points")
		println(points.mkString)
		println("cents")
		println(cents.mkString)

		/*
		val ctrs_out = CounterChain(Counter(max=dataSize/tileSize))
		val accum_out = ArgOut[FixPt](0)
		MetaPipe[FixPt](1, true, ctrs_out, accum_out, _+_, {case i::_ => 
			val bm1 = BRAM[FixPt]("bm1", tileSize)
			val bm2 = BRAM[FixPt]("bm2", tileSize)
			Parallel({
				vec1.ld(bm1, i*tileSize, tileSize)
				vec2.ld(bm2, i*tileSize, tileSize)
			})
			val accum_in = Reg[FixPt]()
			val ctrs_in = CounterChain(Counter(max=tileSize))
			Pipe[FixPt](1, true, ctrs_in, accum_in, _+_, { case j::_ =>
				bm1.ld(j)*bm2.ld(j)
			})
			accum_in.value
		})

		assert(accum_out.value==FixPt(gold))
		*/
	}
}
