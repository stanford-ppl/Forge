import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TestCompiler extends DHDLApplicationCompiler with Test
object TestInterpreter extends DHDLApplicationInterpreter with Test
trait Test extends DHDLApplication {

  def main() {
		val om = OffChipMem[Fix]("om", 6)
    val out = ArgOut[Fix]("out")

    val arr = Array.tabulate(6){i => i}

    setMem(om, arr)

    Accel {
  		val bm = BRAM[Fix](6)
      MetaPipe(6 by 2, out){i =>
        om.ld(bm, i, 2)
        val acc = Reg[Fix]("acc")
        Pipe(0 until 2, acc){ii => bm(ii) }{_+_}
        acc.value
      }{_+_}
    }

    val result = getArg(out)
    println("expected: 15")
    println("out: " + result.mkString)
    assert(result == 15)
	}
}
