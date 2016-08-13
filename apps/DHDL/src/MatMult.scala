import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

// multiFold(p by bm, n by bn){ (i,j) => (i,j)}{(i,j) =>
//   multiFold(p by bp){k => 0}{ k =>
//     aTile = a(bm @@ i, bp @@ k)
//     bTile = b(bp @@ k, bn @@ j)
//     map(bm, bn){(ii,jj) =>
//       reduce(bp){kk => aTile(ii,kk) * bTile(kk,jj) }{_+_}
//     }
//   }{_+_}
// }{_+_}

object MatMultCompiler extends DHDLApplicationCompiler with MatMult
object MatMultInterpreter extends DHDLApplicationInterpreter with MatMult
trait MatMult extends DHDLApplication {
  type T = Flt //FixPt[Signed,B16,B16]
  type Array[T] = ForgeArray[T]

  val mm = 96
  val nn = 96
  val pp = 96
  def matmult(A: Rep[Array[T]], B: Rep[Array[T]], M: Rep[SInt], N: Rep[SInt], P: Rep[SInt]) = {
    bound(M) = 1536
    bound(N) = 1536
    bound(P) = 1536


    val a = OffChipMem[T](mm, pp)
    val b = OffChipMem[T](pp, nn)
    val c = OffChipMem[T](mm, nn)

    val bm        = param(96);   domainOf(bm) = (1,1536,1)
    val bn        = param(96);   domainOf(bn) = (96,1536,96)
    val bp        = param(96);   domainOf(bp) = (96,1536,96)
    val outerPar  = param(1);   domainOf(outerPar)  = (1,6,1)
    val middlePar = param(2);   domainOf(middlePar) = (1,96,1)
    val innerPar  = param(2);   domainOf(innerPar)  = (1,96,1)
    val upMidPar  = param(1);   domainOf(upMidPar)  = (1,1,1)
    val stPar     = param(1);   domainOf(stPar)     = (1,1,1)

    setMem(a, A)
    setMem(b, B)

    Accel {
      Pipe(mm by bm, (nn by bn) par outerPar){(i,j) =>
        Pipe((pp by bp) par upMidPar){k =>
          val tileA = BRAM[T](bm, bp)
          val tileB = BRAM[T](bp, bn)
          val tileC = BRAM[T](bm, bn)
          Parallel {
            tileA := a(i::i+bm, k::k+bp, param(1))
            tileB := b(k::k+bp, j::j+bn, param(1))
          }
          Sequential(bm by 1, (bn by 1) par middlePar){ (ii,jj) =>    // MetaPipe?
            val prod = Reduce((bp by 1) par innerPar)(0.as[T]){ kk => tileA(ii, kk) * tileB(kk, jj) }{_+_}
            val prev = mux(k == 0, 0.as[T], tileC(ii,jj))
            tileC(ii,jj) = prev + prod.value
          }
          c(i::i+bm, j::j+bn, stPar) := tileC
        }
      }
    }
    getMem(c)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {
    val M = mm
    val N = nn
    val P = pp

    val a = Array.fill(M){ Array.fill(P){random[T](100)} }
    val b = Array.fill(P){ Array.fill(N){random[T](100)} }

    val result = matmult(a.flatten, b.flatten, M, N, P)

    val gold = Array.tabulate(M){i =>
      val aRow = a(i)
      Array.tabulate(N){j =>
        val bCol = b.map{row => row(j)}
        aRow.zip(bCol){_*_}.reduce{_+_}
      }
    }.flatten

    printArr(gold, "expected ")
    printArr(result, "got  ")

    assert(gold == result)
  }
}
