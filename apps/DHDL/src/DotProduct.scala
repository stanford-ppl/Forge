import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object DotProductCompiler extends DHDLApplicationCompiler with DotProduct
object DotProductInterpreter extends DHDLApplicationInterpreter with DotProduct
trait DotProduct extends DHDLApplication {
  type Elem = Fix

	override def stageArgNames = List("tileSize")
  lazy val tileSize = stageArgOrElse[Int](0, 4)
  lazy val dataSize = ArgIn[Fix]("dataSize")

  def dotproduct(v1: Rep[OffChipMem[Elem]], v2: Rep[OffChipMem[Elem]], out: Rep[Reg[Fix]]) {
		val b1 = BRAM[Elem]("b1", tileSize)
    val b2 = BRAM[Elem]("b2", tileSize)

		MetaPipe(dataSize by tileSize, out){ i =>
			Parallel {
				v1.ld(b1, i, tileSize)
				v2.ld(b2, i, tileSize)
			}
      val acc = Reg[Elem]("acc")
			Pipe(0 until tileSize, acc){ ii => b1(ii) * b2(ii) }{_+_}
      acc.value
		}{_+_}
	}

  def main() {
    val N = 8

    // Bad things will happen if you try to set the Offchip size from a register value
    val v1 = OffChipMem[Elem]("v1", N)
    val v2 = OffChipMem[Elem]("v2", N)
    val out = ArgOut[Elem]("out")

    val vec1 = Array.fill(N)(randomFix(10))
    val vec2 = Array.fill(N)(randomFix(10))

    println("vec1: " + vec1.mkString(", "))
    println("vec2: " + vec2.mkString(", "))

    setArg(dataSize, N)
    setMem(v1, vec1)
    setMem(v2, vec2)

    Accel {
      dotproduct(v1, v2, out)
    }

    val result = getArg(out)
    val gold = vec1.zip(vec2){_*_}.reduce{_+_}
    println("expected: " + gold.mkString)
    println("result: " + result.mkString)
    assert(result == gold)
  }
}
