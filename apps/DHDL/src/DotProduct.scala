import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object DotProductCompiler extends DHDLApplicationCompiler with DotProduct
object DotProductInterpreter extends DHDLApplicationInterpreter with DotProduct
trait DotProduct extends DHDLApplication {
  type A = Flt //FixPt[Signed, B16, B16]

  def dotproduct(a: Rep[ForgeArray[A]], b: Rep[ForgeArray[A]]) = {
    val tileSize = param("tileSize", 4); domainOf(tileSize) = (96, 19200, 96)
    val outerPar = param("outerPar", 2); domainOf(outerPar) = (1, 6, 1)
    val innerPar = param("innerPar", 2); domainOf(innerPar) = (1, 192, 1)
    val N = a.length; bound(N) = 187200000

    val v1 = OffChipMem[A]("v1", N)
    val v2 = OffChipMem[A]("v2", N)
    val dataSize = ArgIn[SInt]("dataSize")
    val out = ArgOut[A]("out")

    setMem(v1, a)
    setMem(v2, b)
    setArg(dataSize, N)

    Accel {
      Pipe.fold(dataSize by tileSize par outerPar)(out){ i =>
        val b1 = BRAM[A]("b1", tileSize)
        val b2 = BRAM[A]("b2", tileSize)
        Parallel {
          b1 := v1(i::i+tileSize, param(1))
          b2 := v2(i::i+tileSize, param(1))
        }
        Pipe.reduce(tileSize par innerPar)(Reg[A]){ii => b1(ii) * b2(ii) }{_+_}
      }{_+_}
      ()
    }
    getArg(out)
  }

  def main() {
    val N = args(unit(0)).to[SInt]
    val a = Array.fill(N)(random[A](10))
    val b = Array.fill(N)(random[A](10))

    val result = dotproduct(a, b)
    val gold = a.zip(b){_*_}.reduce{_+_}
    println("expected: " + gold.mkString)
    println("result:   " + result.mkString)
    assert(result == gold)
  }
}
