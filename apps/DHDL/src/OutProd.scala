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

		val dataSize = ArgIn[Int](vecLength).value
		val vec1 = OffChipMem[FixPt]("vec1", svec1.map(i => i.toFixPt): _*)
		val vec2 = OffChipMem[FixPt]("vec2", svec2.map(i => i.toFixPt): _*)
		val result = OffChipMem[FixPt]("result", dataSize * dataSize)

		val ctrs_out = CounterChain(Counter(dataSize, tileSize), 
														Counter(dataSize, tileSize))
		MetaPipe(2, ctrs_out, {case i::j::_ => 
			val bm1 = BRAM[FixPt]("bm1", tileSize)
			val bm2 = BRAM[FixPt]("bm2", tileSize)
			Parallel({
				vec1.ld(bm1, i, tileSize)
				vec2.ld(bm2, j, tileSize)
			})
			val bmResult = BRAM[FixPt]("bmResult", tileSize*tileSize)
			val ctrs_in = CounterChain(Counter(max=tileSize), Counter(max=tileSize))
			Pipe(2, ctrs_in, { case ii::jj::_ =>
				val addr = ii * tileSize + jj
				bmResult.st(addr, bm1.ld(ii) * bm2.ld(jj))
			})
			MetaPipe({
				//TODO:FIX THIS
				result.st(bmResult, j, i, tileSize, tileSize, vecLength)
			})
			()
		})

		val fgold = gold.flatten
		fgold.zipWithIndex.foreach{case (g, i) => 
			assert(FixPt(g) == result.ld(i))
		}
	}
}
