import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object GDACompiler extends DHDLApplicationCompiler with GDA 
object GDAInterpreter extends DHDLApplicationInterpreter with GDA

trait GDATest {
	def test(sX:Array[Array[Int]], sY:Array[Boolean], sMu0:Array[Float], sMu1:Array[Float], r:Int,
		c:Int) = {
			var gold = Array.fill(c,c){ 0f }
			sY.zipWithIndex.foreach{ case (yi, rIdx) =>
				val mu_temp = if (yi) (sX(rIdx), sMu1).zipped.map(_-_)
				else (sX(rIdx), sMu0).zipped.map(_-_)
				val outProd = Array.tabulate(c){i =>
				Array.tabulate(c){j=> mu_temp(i)*mu_temp(j)}}
				gold = Array.tabulate(c){i =>
					Array.tabulate(c){j => 
						gold(i)(j) + outProd(i)(j)
					}
				}
			}
			gold
	}
}
trait GDA extends DHDLApplication with GDATest{
	def printUsage = {
		println("Usage: gda")
		exit(-1)
	}
	def main() = {
		val rTileSize = 2
		val cTileSize = 4
		val r = 6
		val c = 8

		val sX = Array.tabulate(r){i => Array.tabulate(c){j => util.Random.nextInt(100)}}
		val sY = Array.tabulate(r){i => util.Random.nextBoolean()}
		val sMu0 = Array.tabulate(c){i => util.Random.nextFloat()*100}
		val sMu1 = Array.tabulate(c){i => util.Random.nextFloat()*100}
		val gold = test(sX, sY, sMu0, sMu1, r, c)

		val row = ArgIn[FixPt](r).value
		val col = ArgIn[FixPt](c).value
		val x = OffChipMem[FixPt]("x", sX.flatten.map(i => i.toFixPt): _*)
		val y = OffChipMem[Boolean]("y", sY.map(i => unit(i)): _*)
		val mu0 = OffChipMem[Float]("mu0", sMu0.map(i => unit(i)):_*)
		val mu1 = OffChipMem[Float]("mu1", sMu1.map(i => unit(i)):_*)
		val muTemp = OffChipMem[Float]("muTemp", col)
		val sigma = OffChipMem[Float]("sigma", col * col)

		val rtCtr = CounterChain(Counter(max=row, step=rTileSize))
		MetaPipe(rtCtr) { case irt::_ => 
			val yB = BRAM[Boolean]("yB", rTileSize)
			y.ld(yB, irt, rTileSize)
			val rCtr = CounterChain(Counter(max=rTileSize))
			Sequential(rCtr) { case ir::_ =>
				val ctCtr = CounterChain(Counter(max=c, step=cTileSize))
				MetaPipe(ctCtr) { case ict::_ => 
					val xB = BRAM[FixPt]("xB", rTileSize, cTileSize)
					val mu0B = BRAM[Float]("mu0B", cTileSize)
					val mu1B = BRAM[Float]("mu1B", cTileSize)
					Parallel {
						x.ld(xB, irt, ict, rTileSize, cTileSize, col)
						mu0.ld(mu0B, ict, cTileSize)
						mu1.ld(mu1B, ict, cTileSize)
					}
					val muTempB = BRAM[Float]("muTempB", cTileSize)
					val cCtr = CounterChain(Counter(max=cTileSize))
					Pipe(cCtr) {case ic::_ =>
						muTempB.st(ic, 
							xB.ld(ir, ic).toFloat - mux(yB.ld(ir), mu1B.ld(ic), mu0B.ld(ic)))
					}
					muTemp.st(muTempB, ict, cTileSize)
				}
				val cctCtr = CounterChain(
					Counter(max=col, step=cTileSize), 
					Counter(max=col, step=cTileSize))
				MetaPipe(cctCtr) { case i::j::_ => 
					val bm1 = BRAM[Float]("bm1", cTileSize)
					val bm2 = BRAM[Float]("bm2", cTileSize)
					val sigmaB = BRAM[Float]("sigmaB", cTileSize, cTileSize)
					Parallel {
						muTemp.ld(bm1, i, cTileSize)
						muTemp.ld(bm2, j, cTileSize)
						sigma.ld(sigmaB, i, j, cTileSize, cTileSize, c)
					}
					val ccCtr = CounterChain(
						Counter(max=cTileSize),
						Counter(max=cTileSize))
					Pipe(ccCtr) { case ii::jj::_ =>
						sigmaB.st(ii, jj, bm1.ld(ii) * bm2.ld(jj) + sigmaB.ld(ii, jj))
					}
					sigma.st(sigmaB, i, j, cTileSize, cTileSize, col)
				}
			}
		}

		val fgold = gold.flatten
		//println("fgold: " + fgold.mkString(","))
		//println("sigma: " + sigma.mkString)
		fgold.zipWithIndex.foreach{case (g, i) => 
			assert(unit(g) == sigma.ld(i))
		}
	}
}
