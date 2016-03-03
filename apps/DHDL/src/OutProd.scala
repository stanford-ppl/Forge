import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object OutProdCompiler extends DHDLApplicationCompiler with OutProd 
object OutProdInterpreter extends DHDLApplicationInterpreter with OutProd

trait OutProd extends DHDLApplication {
	def printUsage = {
    println("Usage: outprod")
    exit(-1)
	}


	//TODO: need to give common interface to FixPt, and Float
	//def tileOutProd[T:Manifest](b1:Rep[BRAM[T]], b2:Rep[BRAM[T]], br:Rep[BRAM[T]], tileSize:Int) = {
	//	val ctrs_in = CounterChain(Counter(max=tileSize), Counter(max=tileSize))
	//	Pipe(ctrs_in) { case ii::jj::_ =>
	//		br.st(ii, jj, b1.ld(ii) * b2.ld(jj))
	//	}
	//}
	//def outProd[T:Manifest](vec1:Rep[OffChipMem[T]], vec2:Rep[OffChipMem[T]], result:Rep[OffChipMem[T]],
	//	dataSize:Rep[FixPt], tileSize:Int) = {
	//	val ctrs_out = CounterChain(Counter(max=dataSize, step=tileSize), 
	//															Counter(max=dataSize, step=tileSize))
	//	MetaPipe(ctrs_out) {case i::j::_ => 
	//		val bm1 = BRAM[T]("bm1", tileSize)
	//		val bm2 = BRAM[T]("bm2", tileSize)
	//		Parallel {
	//			vec1.ld(bm1, i, tileSize)
	//			vec2.ld(bm2, j, tileSize)
	//		}
	//		val bmResult = BRAM[T]("bmResult", tileSize, tileSize)
	//		tileOutProd(bm1, bm2, bmResult, tileSize)
	//		result.st(bmResult, i, j, tileSize, tileSize, dataSize)
	//		()
	//	}
	//}

  def main() = {
		val tileSize = 2
		val vecLength = 4
		val svec1 = Seq.fill(vecLength)(Random.nextInt(100))
		val svec2 = Seq.fill(vecLength)(Random.nextInt(100))
		val gold = Seq.tabulate(vecLength){i => 
				Seq.tabulate(vecLength){j =>
				svec1(i)*svec2(j)
			}
		}

		val dataSize = ArgIn[FixPt](vecLength).value
		val vec1 = OffChipMem[FixPt]("vec1", svec1.map(i => i.toFixPt): _*)
		val vec2 = OffChipMem[FixPt]("vec2", svec2.map(i => i.toFixPt): _*)
		val result = OffChipMem[FixPt]("result", dataSize * dataSize)

		val ctrs_out = CounterChain(Counter(max=dataSize, step=tileSize), 
																Counter(max=dataSize, step=tileSize))
		MetaPipe(ctrs_out) {case i::j::_ => 
			val bm1 = BRAM[FixPt]("bm1", tileSize)
			val bm2 = BRAM[FixPt]("bm2", tileSize)
			Parallel {
				vec1.ld(bm1, i, tileSize)
				vec2.ld(bm2, j, tileSize)
			}
			val bmResult = BRAM[FixPt]("bmResult", tileSize, tileSize)
			val ctrs_in = CounterChain(Counter(max=tileSize), Counter(max=tileSize))
			Pipe(ctrs_in) { case ii::jj::_ =>
				bmResult.st(ii, jj, bm1.ld(ii) * bm2.ld(jj))
			}
			result.st(bmResult, i, j, tileSize, tileSize, dataSize)
			()
		}

		val fgold = gold.flatten
		fgold.zipWithIndex.foreach{case (g, i) => 
			assert(FixPt(g) == result.ld(i))
		}
	}
}
