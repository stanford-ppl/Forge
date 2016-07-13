import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object OuterProductCompiler extends DHDLApplicationCompiler with OuterProduct
object OuterProductInterpreter extends DHDLApplicationInterpreter with OuterProduct
trait OuterProduct extends DHDLApplication {
  type T = Flt

  def outerproduct(a: Rep[ForgeArray[T]], b: Rep[ForgeArray[T]]) = {
    val tileSizeA = param(8);  domainOf(tileSizeA) = (96, 38400, 96)
    val tileSizeB = param(8);  domainOf(tileSizeB) = (96, 38400, 96)
    val outerPar  = param(2);  domainOf(outerPar) = (1, 4, 1)
    val innerPar  = param(2);  domainOf(innerPar) = (1, 38400, 1)

    val M = a.length;  bound(M) = 38400
    val N = b.length;  bound(N) = 38400

    val vec1 = OffChipMem[T](M)
    val vec2 = OffChipMem[T](N)
    val out = OffChipMem[T](M, N)
    val sizeA = ArgIn[SInt]
    val sizeB = ArgIn[SInt]

    // Transfer data and start accelerator
    setArg(sizeA, M)
    setArg(sizeB, N)
    setMem(vec1, a)
    setMem(vec2, b)

    Accel {
      Pipe(sizeA by tileSizeA, sizeB by tileSizeB par outerPar){ (i,j) =>
        val b1 = BRAM[T](tileSizeA)
        val b2 = BRAM[T](tileSizeB)
        val outTile = BRAM[T](tileSizeA, tileSizeB)
        Parallel {
          b1 := vec1(i::i+tileSizeA)
          b2 := vec2(j::j+tileSizeB)
        }
        Pipe(tileSizeA by 1, tileSizeB par innerPar){ (ii,jj) => outTile(ii, jj) = b1(ii) * b2(jj) } // 2

        out(i::i+tileSizeA, j::j+tileSizeB) := outTile
      }
    }
    getMem(out)
  }

  def main() = {
    val M = args(0).to[SInt]
    val N = args(1).to[SInt]
    val a = Array.fill(M)(random[T](100))
    val b = Array.fill(N)(random[T](100))

    val result = outerproduct(a, b)

    val gold = Array.tabulate(N){i => Array.tabulate(N){j => a(i) * b(j) }}.flatten
    //println("expected: " + gold.mkString(", "))
    //println("result:   " + result.mkString(", "))
    assert( result == gold )
  }
}
