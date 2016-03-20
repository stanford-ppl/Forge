import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TestCompiler extends DHDLApplicationCompiler with Test
object TestInterpreter extends DHDLApplicationInterpreter with Test
trait Test extends DHDLApplication {

  def main() {
		val outer = ArgOut[Fix]

    val v1 = OffChipMem[Fix]("v1", 10)

    val vec1 = Array.fill(10)(randomFix(10))
    setMem(v1, vec1)

    Accel {
      val b1 = BRAM[Fix]("b1", 5)

			Parallel {
				val a = 3.toFixPt
				val b = 3.toFixPt
				val c = 3.toFixPt
				println(a + b * c)
			}
      MetaPipe(10 by 5, outer){i =>
				Parallel{
					val a = 3.toFixPt
					val b = 4.toFixPt
					val d = abs(b)
					val c = a + d
					println(c)
				}

        v1.ld(b1, i, 5)
        val inner = Reg[Fix]
        Pipe(0 until 5, inner){ii => b1(ii) ** 2 }{_+_}
        inner.value
      }{_+_}
    }

    val gold = vec1.map{_**2}.reduce{_+_}

    println("outer: " + getArg(outer).mkString + " (should be " + gold.mkString + ")")

    /*val v1 = Array.fill(10)(randomFix(10))
    val outer = ArgOut[Fix] //Array.empty[Fix](1)

    val blk = Array.empty[Fix](5)

    MetaPipe(10 by 5){i => //for (i <- 0 until 10 by 5) {
      for (j <- 0 until 5) { blk(j) = v1(i + j) }

      val inner = Reg[Fix] //Array.empty[Fix](1)
      Pipe(0 until 5) { j =>
        //inner(0) = inner(0) + blk(j)**2
        inner := inner.value + blk(j)**2
      }
      //outer(0) = outer(0) + inner(0)
      outer := outer.value + inner.value
    }

    val gold = v1.map{_**2}.reduce{_+_}

  }
	*/
  }
}
