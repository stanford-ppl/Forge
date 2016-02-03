import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TestCompiler extends DHDLApplicationCompiler with Test 
object TestInterpreter extends DHDLApplicationInterpreter with Test 

trait Test extends DHDLApplication {
	def printUsage = {
    println("Usage: dotprod")
    exit(-1)
	}
  def main() = {
		//val om = OffChipMem[FixPt]("om", Array[FixPt](1l,2l,3l))

		val a = FixPt(5, 31, 0)
		val b = FixPt(7, 31, 0)
		val r = Reg("a", a)
		println(a.mkString)
		assert(r.value==a)
		r.write(b)
		assert(r.value==b)
		assert(r.init==a)
		r.reset
		assert(r.value==a)

		val m = BRAM[FixPt]("m", 16)
		m.st(3,b)
		assert(m.ld(3)==b)

		val ctr1 = Ctr("ctr1", 0, 10, 1)
		val ctrs = CtrChain(ctr1)
		println(ctrs.mkString)

		Map(ctrs, { c =>
			m.st(c, FixPt(c))
			println(c)
		})
		println(m.mkString)

	}
}
