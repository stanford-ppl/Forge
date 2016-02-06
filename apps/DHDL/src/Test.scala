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

		val om = OffChipMem[FixPt]("om", 1, 2, 3, 4, 5)
		val bm = BRAM[FixPt]("bm", 5)
		om.ld(bm, 0, 5)
		assert(bm.ld(5)!=5)

		val a = FixPt(5)
		val b = FixPt(7)
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

		val ctr1 = Ctr("ctr1", 0, 3, 1)
		val ctr2 = Ctr("ctr2", 0, 4, 1)
		val ctrs = CtrChain(ctr1, ctr2)
		println(ctrs.mkString)

		map2(ctrs, { case i::j::_ =>
			//m.st(c, FixPt(c))
			println(i)
			println(j)
			println("---")
		})
		//assert(m.ld(9)!=9)

		//Reduce(ctrs, r, { (c:Rep[Int],reg:Rep[Reg[FixPt]]) => 
		//	reg.write(reg.value + c)
		//})
		//assert(r.value!=50)
	}
}
