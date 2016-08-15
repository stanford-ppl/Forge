import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object DotProductCompiler extends DHDLApplicationCompiler with DotProduct
object DotProductInterpreter extends DHDLApplicationInterpreter with DotProduct
trait DotProduct extends DHDLApplication {
  type T = SInt
  val N = 93600000
  val tileSize = 19200
  val innerPar = 1
  val outerPar = 1
  type Array[T] = ForgeArray[T]

  def dotproduct(a: Rep[Array[T]], b: Rep[Array[T]]) = {
    val B = param(tileSize); domainOf(B) = (96, 19200, 96)
    val P1 = param(outerPar); domainOf(P1) = (1, 6, 1)
    val P2 = param(innerPar); domainOf(P2) = (1, 192, 1)
    val P3 = param(innerPar); domainOf(P3) = (1, 192, 1)
    val dataSize = a.length; bound(dataSize) = 187200000

    val N = ArgIn[SInt]
    setArg(N, dataSize)

    val out = ArgOut[T]

    val v1 = OffChipMem[T](N)
    val v2 = OffChipMem[T](N)
    setMem(v1, a)
    setMem(v2, b)

    Accel {
      Fold(N by B par P1)(out, 0.as[T]){ i =>
        val b1 = BRAM[T](B)
        val b2 = BRAM[T](B)
        Parallel {
          b1 := v1(i::i+B, P3)
          b2 := v2(i::i+B, P3)
        }
        Reduce(B par P2)(0.as[T]){ii => b1(ii) * b2(ii) }{_+_}
      }{_+_}
    }
    getArg(out)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val a = Array.fill(N)(random[T](10))
    val b = Array.fill(N)(random[T](10))

    // printArr(a, "a")
    // printArr(b, "b")

    val result = dotproduct(a, b)
    val gold = a.zip(b){_*_}.reduce{_+_}
    println("expected: " + gold)
    println("result: " + result)
//    assert(result == gold)
  }
}
