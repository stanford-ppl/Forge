import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object DotProductCompiler extends DHDLApplicationCompiler with DotProduct 
object DotProductInterpreter extends DHDLApplicationInterpreter with DotProduct 

trait DotProduct extends DHDLApplication {
	def printUsage = {
    println("Usage: dotprod")
    exit(-1)
	}
  def main() = {
		val tileSize = 4
		val dataSize = 64
		val svec1 = Seq.fill(dataSize)(Random.nextInt(100))
		val svec2 = Seq.fill(dataSize)(Random.nextInt(100))
		val vec1 = OffChipMem[FixPt]("vec1", svec1.map(i => i.toFixPt): _*)
		val vec2 = OffChipMem[FixPt]("vec2", svec2.map(i => i.toFixPt): _*)
		val bm1 = BRAM[FixPt]("bm1", 16)
		val bm2 = BRAM[FixPt]("bm2", 16)

		val ctrs = CtrChain(Ctr("ctr", 0, dataSize/tileSize, 1), Ctr("ctr1", 0, tileSize, 1))
	}
}
