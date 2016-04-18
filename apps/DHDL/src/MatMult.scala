import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

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

  override def stageArgNames = List("bm", "bn", "bp")
  lazy val bm = param(70)
  lazy val bn = param(96)
  lazy val bp = param(960)
  lazy val outerPar  = param(1)
  //lazy val blockPar  = param(1)
  lazy val middlePar = param(1)
  lazy val innerPar  = param(1)

  lazy val m = ArgIn[SInt]("m")
  lazy val n = ArgIn[SInt]("n")
  lazy val p = ArgIn[SInt]("p")

  def matmult(a: Rep[OffChipMem[Elem]], b: Rep[OffChipMem[Elem]], c: Rep[OffChipMem[Elem]]) {
    MetaPipe(m by bm, (n by bn) par outerPar){(i,j) =>
      MetaPipe((p by bp) par unit(1)){k =>
        val tileA = BRAM[Elem]("tileA", bm, bp)
        val tileB = BRAM[Elem]("tileB", bp, bn)
        val tileC = BRAM[Elem]("tileC", bm, bn)
        //BlockReduce((p by bp) par blockPar, tileC){ k =>
        Parallel {
          tileA := a(i::i+bm, k::k+bp, innerPar)
          tileB := b(k::k+bp, j::j+bn, innerPar)
        }
        //val accTile = BRAM[Elem]("accTile", bm, bn)
        Sequential(bm by 1, (bn by 1) par middlePar){ (ii,jj) =>    // MetaPipe?
          val accum = Reg[Elem]
          Pipe((bp by 1) par innerPar, accum){ kk => tileA(ii, kk) * tileB(kk, jj) }{_+_}
          Pipe {
            val prev = mux(k == 0, 0.as[Elem], tileC(ii,jj))
            tileC(ii,jj) = prev + accum.value
          }
        }
        //accTile
        //}{_+_}
        c(i::i+bm, j::j+bn, param(1)) := tileC
      }
    }
  }

  def main() = {
    val M = args(unit(0)).to[SInt]
    val N = args(unit(0)).to[SInt]
    val P = args(unit(0)).to[SInt]

    bound(M) = 1536
    bound(N) = 1536
    bound(P) = 1536
    domainOf(bm) = (1,960,10)
    domainOf(bn) = (96,960,96)
    domainOf(bp) = (96,1920,96)
    domainOf(outerPar)  = (1,6,1)
    domainOf(middlePar) = (1,96,6)
    domainOf(innerPar)  = (1,96,6)

    val a = OffChipMem[Elem]("A", M, P)
    val b = OffChipMem[Elem]("B", P, N)
    val c = OffChipMem[Elem]("C", M, N)

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
