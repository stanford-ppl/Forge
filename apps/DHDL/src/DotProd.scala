import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object DotProdCompiler extends DHDLApplicationCompiler with DotProd 
object DotProdInterpreter extends DHDLApplicationInterpreter with DotProd

trait DotProd extends DHDLApplication {
	def printUsage = {
    println("Usage: dotprod")
    exit(-1)
	}
  def main() = {
		val tileSize = 4
		val sDataSize = 32
		val dataSize = ArgIn[FixPt](sDataSize).value
		val svec1 = Seq.fill(sDataSize)(Random.nextInt(100))
		val svec2 = Seq.fill(sDataSize)(Random.nextInt(100))
		val gold = svec1.zip(svec2).map{case (x,y) => x*y}.reduce(_+_)

		val vec1 = OffChipMem[FixPt]("vec1", svec1.map(i => i.toFixPt): _*)
		val vec2 = OffChipMem[FixPt]("vec2", svec2.map(i => i.toFixPt): _*)
		val ctrs_out = CounterChain(Counter(max=dataSize/tileSize))
		val accum_out = ArgOut[FixPt](0)
		MetaPipe[FixPt](true, ctrs_out, accum_out, _+_) {case i::_ => 
			val bm1 = BRAM[FixPt]("bm1", tileSize)
			val bm2 = BRAM[FixPt]("bm2", tileSize)
			Parallel {
				vec1.ld(bm1, i*tileSize, tileSize)
				vec2.ld(bm2, i*tileSize, tileSize)
			}
			val accum_in = Reg[FixPt](0)
			val ctrs_in = CounterChain(Counter(max=tileSize))
			Pipe[FixPt](ctrs_in, accum_in, _+_) { case j::_ =>
				bm1.ld(j)*bm2.ld(j)
			}
			accum_in.value
		}

		assert(accum_out.value==FixPt(gold))
	}
}
