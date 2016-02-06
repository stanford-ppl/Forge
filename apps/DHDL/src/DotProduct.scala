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
		val dataSize = 12
		val svec1 = Seq.fill(dataSize)(Random.nextInt(100))
		val svec2 = Seq.fill(dataSize)(Random.nextInt(100))

		val vec1 = OffChipMem[FixPt]("vec1", svec1.map(i => i.toFixPt): _*)
		val vec2 = OffChipMem[FixPt]("vec2", svec2.map(i => i.toFixPt): _*)
		val bm1 = BRAM[FixPt]("bm1", tileSize)
		val bm2 = BRAM[FixPt]("bm2", tileSize)

		val ctrs_out = CtrChain(Ctr(max=dataSize/tileSize))
		val ctrs_in = CtrChain(Ctr(max=tileSize))

		val accum = Reg[FixPt]("accum", 0)

		MetaPipe1(ctrs_out, {case i::_ => 
			vec1.ld(bm1, i*tileSize, tileSize)
			vec2.ld(bm2, i*tileSize, tileSize)
			Pipe1(ctrs_in, accum, ((_+_):(Rep[FixPt],Rep[FixPt]) => Rep[FixPt]), { case j::_ =>
				bm1.ld(j)*bm2.ld(j)
			})
			()
		})

		val gold = svec1.zip(svec2).map{case (x,y) => x*y}.reduce(_+_)
		assert(accum.value==FixPt(gold))
		
	}
}
