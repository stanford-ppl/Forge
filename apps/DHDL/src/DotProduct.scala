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
  lazy val dataSize = ArgIn[Elem]("dataSize", 32)

  def dotproduct(vec1: Rep[OffChipMem[Elem]], vec2: Rep[OffChipMem[Elem]]): Rep[Elem] = {
		val b1 = BRAM[Elem]("b1", tileSize)
    val b2 = BRAM[Elem]("b2", tileSize)
    val out = ArgOut[Elem]("out")

		MetaPipe(dataSize by tileSize, out){ i =>
			Parallel {
				vec1.ld(b1, i, tileSize)
				vec2.ld(b2, i, tileSize)
			}
      val acc = Reg[Elem]("acc")
			Pipe(0 until tileSize, acc){ ii => b1(ii) * b2(ii) }{_+_}
      acc.value
		}{_+_}

    out.value
	}

  def main() {
    val v1 = OffChipMem[Elem]("v1", dataSize)
    val v2 = OffChipMem[Elem]("v2", dataSize)
    dotproduct(v1, v2)
  }
}


object DotProductTestCompiler extends DHDLApplicationCompiler with DotProductTest
object DotProductTestInterpreter extends DHDLApplicationInterpreter with DotProductTest
trait DotProductTest extends DotProduct {
  override def stageArgNames = List("tileSize", "testSize")

  lazy val testSize = stageArgOrElse[Int](1, 32)

  override def main() {
    val svec1 = Seq.fill(testSize)(Random.nextInt(100))
    val svec2 = Seq.fill(testSize)(Random.nextInt(100))
    val gold = svec1.zip(svec2).map{case (x,y) => x*y}.reduce(_+_)

    val v1 = OffChipMem.withInit1D("v1", svec1.map(i => i.toFixPt))
    val v2 = OffChipMem.withInit1D("v2", svec2.map(i => i.toFixPt))
    val res = dotproduct(v1, v2)

    println("out: " + res.mkString)

    assert(res == gold.toFixPt)
  }
}
