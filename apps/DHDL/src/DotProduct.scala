import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object DotProductCompiler extends DHDLApplicationCompiler with DotProduct
object DotProductInterpreter extends DHDLApplicationInterpreter with DotProduct
trait DotProduct extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]

  def dotproduct(a: Rep[Array[T]], b: Rep[Array[T]]) = {
    val B = param(4); domainOf(B) = (96, 19200, 96)
    val P1 = param(1); domainOf(P1) = (1, 6, 1)
    val P2 = param(1); domainOf(P2) = (1, 192, 1)
    val P3 = param(1); domainOf(P3) = (1, 192, 1)
    val dataSize = a.length; bound(dataSize) = 187200000

    val N = ArgIn[SInt]
    val out = ArgOut[T]
    setArg(N, dataSize)

    val v1 = OffChipMem[T](N)
    val v2 = OffChipMem[T](N)
    setMem(v1, a)
    setMem(v2, b)

    Accel {
      Fold(N by B par P1)(out, 0.as[T]){ i =>
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
    }
    getArg(out)
  }

  def main() {
    val N = args(0).to[SInt]
    val a = Array.fill(N)(random[T](10))
    val b = Array.fill(N)(random[T](10))

    println("a: " + a.mkString(", "))
    println("b: " + b.mkString(", "))

    val result = dotproduct(a, b)
    val gold = a.zip(b){_*_}.reduce{_+_}
    println("expected: " + gold.mkString)
    println("result:   " + result.mkString)
    assert(result == gold)
  }
}
