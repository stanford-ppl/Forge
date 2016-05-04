import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object DotProductCompiler extends DHDLApplicationCompiler with DotProduct
object DotProductInterpreter extends DHDLApplicationInterpreter with DotProduct
trait DotProduct extends DHDLApplication {
  type Elem = Flt //FixPt[Signed, B16, B16]

  override def stageArgNames = List("tileSize")
  lazy val tileSize = param("tileSize", 9600)
  lazy val outerPar = param("outerPar", 5)
  lazy val innerPar = param("innerPar", 60)
  lazy val dataSize = ArgIn[SInt]("dataSize")

  def dotproduct(v1: Rep[OffChipMem[Elem]], v2: Rep[OffChipMem[Elem]], out: Rep[Reg[Elem]]) {
    Pipe(dataSize by tileSize par outerPar).fold(out){ i =>
      val b1 = BRAM[Elem]("b1", tileSize)
      val b2 = BRAM[Elem]("b2", tileSize)
      Parallel {
        b1 := v1(i::i+tileSize, innerPar)
        b2 := v2(i::i+tileSize, innerPar)
      }
      val acc = Reg[Elem]("acc")
      Pipe(0 until tileSize par innerPar).fold(acc){ ii => b1(ii) * b2(ii) }{_+_}
    }{_+_}
  }

  def main() {
    val N = args(unit(0)).to[SInt]

    bound(N) = 187200000
    domainOf(tileSize) = (96,19200,96)
    domainOf(outerPar) = (1,6,1)
    domainOf(innerPar) = (1,192,1)

    val v1 = OffChipMem[Elem]("v1", N)
    val v2 = OffChipMem[Elem]("v2", N)
    val out = ArgOut[Elem]("out")

    val vec1 = Array.fill(N)(random[Elem](10))
    val vec2 = Array.fill(N)(random[Elem](10))

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
