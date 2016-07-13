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
  type Elem = Flt //FixPt[Signed,B16,B16]

  lazy val bm = param(50)
  lazy val bn = param(96)
  lazy val bp = param(896)
  lazy val outerPar  = param(1)
  lazy val middlePar = param(2)
  lazy val innerPar  = param(4)

  lazy val upMidPar = param(1)
  lazy val stPar = param(1)

  lazy val m = ArgIn[SInt]
  lazy val n = ArgIn[SInt]
  lazy val p = ArgIn[SInt]

  def matmult(a: Rep[OffChipMem[Elem]], b: Rep[OffChipMem[Elem]], c: Rep[OffChipMem[Elem]]) {
    Pipe(m by bm, (n by bn) par outerPar){(i,j) =>
      Pipe((p by bp) par upMidPar){k =>
        val tileA = BRAM[Elem](bm, bp)
        val tileB = BRAM[Elem](bp, bn)
        val tileC = BRAM[Elem](bm, bn)
        Parallel {
          tileA := a(i::i+bm, k::k+bp, param(1))
          tileB := b(k::k+bp, j::j+bn, param(1))
        }
        Sequential(bm by 1, (bn by 1) par middlePar){ (ii,jj) =>    // MetaPipe?
          val accum = Reg[Elem]
          Pipe.reduce((bp by 1) par innerPar)(accum){ kk => tileA(ii, kk) * tileB(kk, jj) }{_+_}
          Pipe {
            val prev = mux(k == 0, 0.as[Elem], tileC(ii,jj))
            tileC(ii,jj) = prev + accum.value
            ()
          }
        }
        c(i::i+bm, j::j+bn, stPar) := tileC
      }
    }
  }

  def main() = {
    val M = args(0).to[SInt]
    val N = args(1).to[SInt]
    val P = args(2).to[SInt]

    bound(M) = 1536
    bound(N) = 1536
    bound(P) = 1536
    domainOf(bm) = (1,1536,1)
    domainOf(bn) = (96,1536,96)
    domainOf(bp) = (96,1536,96)

    domainOf(outerPar)  = (1,6,1)
    domainOf(middlePar) = (1,96,1)
    domainOf(innerPar)  = (1,96,1)

    domainOf(upMidPar) = (1,1,1)
    domainOf(stPar) = (1,1,1)

    val a = OffChipMem[Elem](M, P)
    val b = OffChipMem[Elem](P, N)
    val c = OffChipMem[Elem](M, N)

    val sa = Array.fill(M){ Array.fill(P){random[Elem](100)} }
    val sb = Array.fill(P){ Array.fill(N){random[Elem](100)} }

    setArg(m, M)
    setArg(n, N)
    setArg(p, P)
    setMem(a, sa.flatten)
    setMem(b, sb.flatten)
    Accel{ matmult(a, b, c) }

    val gold = Array.tabulate(M){i =>
      val aRow = sa(i)
      Array.tabulate(N){j =>
        val bCol = sb.map{row => row(j)}
        aRow.zip(bCol){_*_}.reduce{_+_}
      }
    }.flatten

    val result = getMem(c)

    println("expected: " + gold.mkString(", "))
    println("result:   " + result.mkString(", "))
    assert(gold == result)
  }
}
