import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object DotProductCompiler extends DHDLApplicationCompiler with DotProduct
object DotProductInterpreter extends DHDLApplicationInterpreter with DotProduct
trait DotProduct extends DHDLApplication {
  type T = SInt
  val tileSize = 9600
  val innerPar = 64
  val outerPar = 1
  type Array[T] = ForgeArray[T]

  def dotproduct(a: Rep[Array[T]], b: Rep[Array[T]]) = {
    val B = param(tileSize); domainOf(B) = (96, 19200, 96)
    val P1 = param(outerPar); domainOf(P1) = (1, 6, 1)
    val P2 = param(innerPar); domainOf(P2) = (1, 192, 1)
    val P3 = param(innerPar); domainOf(P3) = (1, 192, 1)
    val dataSize = a.length; bound(dataSize) = 187200000

    val N = ArgIn[T]
    val out = ArgOut[T]
    setArg(N, a.length)

    val v1 = OffChipMem[T](N)
    val v2 = OffChipMem[T](N)
    setMem(v1, a)
    setMem(v2, b)

    Accel {
      val reg = Reg[T]
      Fold(N by B par P1)(reg, 0.as[T]){ i =>
        val b1 = FIFO[T](512)
        val b2 = FIFO[T](512)
        Parallel {
          b1 := v1(i::i+B, P3)
          b2 := v2(i::i+B, P3)
        }
        Reduce(B par P2)(0.as[T]){ii =>
          b1.pop() * b2.pop()
        }{_+_}
      }{_+_}
      Pipe { out := reg }
    }
    getArg(out)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val N = args(0).to[SInt]
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