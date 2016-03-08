import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TestCompiler extends DHDLApplicationCompiler with Test
object TestInterpreter extends DHDLApplicationInterpreter with Test
trait Test extends DHDLApplication {

  def main() {
		val om = OffChipMem.withInit1D("om", Seq(1,2,3,4,5,6).map(_.toFixPt))
		val bm = BRAM[Fix](6)
    val out = ArgOut[Fix]("out")

    MetaPipe(6 by 2, out){i =>
      om.ld(bm, i, 2)
      val acc = Reg[Fix]("acc")
      Pipe(0 until 2, acc){ii => bm(ii) }{_+_}
      acc.value
    }{_+_}

    println("out: " + out.value.mkString)
    assert(out.value == 21)
	}
}
