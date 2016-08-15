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

  val mm = 1920
  val nn = 19200
  val pp = 19200
  val tileSizeM = 192
  val tileSizeN = 192
  val tileSizeP = 384
  val innerPar = 1
  val midPar = 1
  val outerPar = 1
  def matmult(A: Rep[Array[T]], B: Rep[Array[T]], M: Rep[SInt], N: Rep[SInt], P: Rep[SInt]) = {
    bound(M) = 1536
    bound(N) = 1536
    bound(P) = 1536


    val a = OffChipMem[T](mm, pp)
    val b = OffChipMem[T](pp, nn)
    val c = OffChipMem[T](mm, nn)

    val bm        = param(tileSizeM);   domainOf(bm) = (1,1536,1)
    val bn        = param(tileSizeN);   domainOf(bn) = (96,1536,96)
    val bp        = param(tileSizeP);   domainOf(bp) = (96,1536,96)
    val op  = param(outerPar);   domainOf(op)  = (1,6,1)
    val mp = param(midPar);   domainOf(mp) = (1,96,1)
    val ip  = param(innerPar);   domainOf(ip)  = (1,96,1)
    val upMidPar  = param(1);   domainOf(upMidPar)  = (1,1,1)
    val stPar     = param(1);   domainOf(stPar)     = (1,1,1)

    setMem(a, A)
    setMem(b, B)

    Accel {
      Pipe(mm by bm, (nn by bn) par op){(i,j) =>
        Pipe((pp by bp) par upMidPar){k =>
          val tileA = BRAM[T](bm, bp)
          val tileB = BRAM[T](bp, bn)
          val tileC = BRAM[T](bm, bn)
          Parallel {
            tileA := a(i::i+bm, k::k+bp, param(1))
            tileB := b(k::k+bp, j::j+bn, param(1))
          }
          Sequential(bm by 1, (bn by 1) par mp){ (ii,jj) =>    // MetaPipe?
            val prod = Reduce((bp by 1) par ip)(0.as[T]){ kk => tileA(ii, kk) * tileB(kk, jj) }{_+_}
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
    val M = args(0).to[SInt]
    val N = args(1).to[SInt]
    val P = args(2).to[SInt]

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
