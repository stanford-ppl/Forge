import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object KmeansCompiler extends DHDLApplicationCompiler with Kmeans 
object KmeansInterpreter extends DHDLApplicationInterpreter with Kmeans

trait KmeansTest {
	def test (sPoints:Seq[Seq[Float]], sNumCents:Int, sDim:Int) = {
		val sCents = Seq.tabulate(sNumCents){i =>
			sPoints(i)
		}
		def dist(p1:Seq[Float], p2:Seq[Float]):Float = {
			p1.zip(p2).map{case (d1, d2) => (d1-d2)*(d1-d2)}.reduce(_+_)
		}
		var centSum = Array.tabulate(sNumCents){i =>
			Array.tabulate(sDim){d => 0f}
		}
		var centCount = Array.tabulate(sNumCents){i => 0f}
		sPoints.foreach { p =>
			var minCent = 0
			var minDist = -1.0 
			sCents.zipWithIndex.foreach{case (c, i) =>
				val currDist = dist(p, c)
				if ((currDist<minDist) || (minDist < 0f)) {
					minDist = currDist
					minCent = i
				}
			}
			centSum(minCent).zipWithIndex.foreach { case (d, i) => 
				centSum(minCent)(i) = d + p(i)
			} 
			centCount(minCent) = centCount(minCent) + 1
		}
		val gold = centSum.zipWithIndex.map{ case (c,i) =>
			c.map(d => d/centCount(i).toFloat)
		}

		//println("points:")
		//println(sPoints.map(p => p.mkString(",")).mkString("\n"))
		//println("cents:")
		//println(sCents.map(c => c.mkString(",")).mkString("\n"))
		//println("newCents:")
		//println(gold.map(c => c.mkString(",")).mkString("\n"))

		gold
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
				Random.nextFloat()*100f
			}
		}

		val gold = test(sPoints, sNumCents, sDim)

		val numPoints = ArgIn[FixPt](sNumPoints).value
		val numCents = ArgIn[FixPt](sNumCents).value

		val points = OffChipMem[Float](sPoints.flatten.map(i => unit(i)): _*)
		val oldCents = BRAM[Float](sNumCents, sDim)
		val newCents = BRAM[Float](sNumCents, sDim)
		val centCount = BRAM[FixPt](sNumCents)
		points.ld(oldCents, 0, sNumCents*sDim)

		MetaPipe {
			val tileCtr = CounterChain(Counter(max=numPoints, step=tileSize))
			val pointsB = BRAM[Float](tileSize, sDim)
			Sequential(tileCtr) { case iTile::_ => 
				points.ld(pointsB, iTile*sDim, tileSize*sDim)
				val ptCtr = CounterChain(Counter(max=tileSize))
				val minDist = Reg[Float](-1f)
				val minCent = Reg[FixPt](0)
				Sequential(ptCtr) { case iP::_ =>
					//TODO; this should be a reduce on two regs
					minDist.reset
					minCent.reset
					val ctCtr = CounterChain(Counter(max=numCents))
					MetaPipe(true, ctCtr) { case iC::_ =>
						val dimCtr = CounterChain(Counter(max=sDim))
						val dist = Reg[Float](0)
						Pipe[Float](dimCtr, dist, _+_) { case iD::_ =>
							(pointsB.ld(iP, iD) - oldCents.ld(iC, iD)).pow(2)
						}
						val closer = (dist.value < minDist.value) || (minDist.value < unit(0f))
						minDist.write(mux(closer, dist.value, minDist.value))
						minCent.write(mux(closer, iC, minCent.value))
					}
					Parallel {
						val dimCtr = CounterChain(Counter(max=sDim))
						Pipe(dimCtr) { case iD::_ =>
							newCents.st(minCent.value, iD,
								pointsB.ld(iP, iD) + newCents.ld(minCent.value, iD)) 
						}
						centCount.st(minCent.value, centCount.ld(minCent.value) + FixPt(1))
					}
				}
			}
			val newCentCtr = CounterChain(Counter(max=numCents), Counter(max=sDim))
			Pipe(newCentCtr) {case iC::iD::_ =>
				newCents.st(iC, iD, newCents.ld(iC, iD)/centCount.ld(iC).toFloat)
			}
		}
		//println("DHDL:")
		//println("points")
		//println(points.mkString)
		//println("cents")
		//println(newCents.mkString)
		val fgold = gold.flatten
		fgold.zipWithIndex.foreach{case (g, i) => 
			assert(unit(g) == newCents.ld(i))
		}
	}
}
