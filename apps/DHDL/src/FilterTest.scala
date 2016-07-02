import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object FilterTestCompiler extends DHDLApplicationCompiler with FilterTest
object FilterTestInterpreter extends DHDLApplicationInterpreter with FilterTest
trait FilterTest extends DHDLApplication {
  def main() {
    val N = args(0).to[SInt]
    val B = param(10)

    val data = OffChipMem[SInt](N)
    val size = ArgIn[SInt]
    val sum = ArgOut[SInt]

    val vec = Array.tabulate(N){i => i}

    println(vec.mkString(","))

    setArg(size, N)
    setMem(data, vec)

    Accel {
      Pipe.fold(size by B par unit(1))(sum){i =>
        val tile = BRAM[SInt](B)
        val fifo = FIFO[SInt](B)
        tile := data(i::i+B)

        Pipe(B by 1 par unit(2)){j =>
          val x = tile(j)
          fifo.push(x, x % 2 == 0)
        }

        val innerSum = Reg[SInt]
        Pipe.reduce(fifo.count by 1 par unit(2))(innerSum){j =>
          fifo.pop()
        }{_+_}
      }{_+_}
      ()
    }
    val result = getArg(sum)
    println("result = " + result)
  }
}
