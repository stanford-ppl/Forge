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
  type Elem = Fix

  override def stageArgNames = List("bm", "bn", "bp")
  lazy val bm = stageArgOrElse[Int](0, 2)
  lazy val bn = stageArgOrElse[Int](1, 2)
  lazy val bp = stageArgOrElse[Int](2, 2)

  lazy val m = ArgIn[Fix]("m")
  lazy val n = ArgIn[Fix]("n")
  lazy val p = ArgIn[Fix]("p")

  def matmult(a: Rep[OffChipMem[Elem]], b: Rep[OffChipMem[Elem]], c: Rep[OffChipMem[Elem]]) {
    MetaPipe(m by bm, n by bn){ (i,j) =>
      val tileA = BRAM[Elem](bm, bp)
      val tileB = BRAM[Elem](bp, bn)
      val tileC = BRAM[Elem](bm, bn)
      BlockReduce(p by bp, tileC){ k =>
        Parallel {
          a.ld(tileA, i, k, bm, bp)   // tileA = a(bm @@ i, bp @@ k)
          b.ld(tileB, k, j, bp, bn)   // tileB = b(bp @@ k, bp @@ j)
        }
        val accTile = BRAM[Elem](bm, bn)
        Sequential(bm by 1, bn by 1){ (ii,jj) =>    // MetaPipe?
          val accum = Reg[Elem]
          Pipe(bp by 1, accum){ kk => tileA(ii, kk) * tileB(kk, jj) }{_+_}
          Pipe{ accTile(ii,jj) = accum.value }
        }
        accTile
      }{_+_}
      c.st(tileC, i, j, bm, bn)      // c(bm at i, bn at j) = tileC
    }
  }

  def main() = {
    val M = 8
    val N = 8
    val P = 8

		val a = OffChipMem[Elem]("A", m, p)
		val b = OffChipMem[Elem]("B", p, n)
		val c = OffChipMem[Elem]("C", m, n)

    val sa = Array.fill(M){ Array.fill(P){random[Elem] * 100} }
    val sb = Array.fill(P){ Array.fill(N){random[Elem] * 100} }

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

    val result = Array.empty[Fix](M*N)
    getMem(c, result)

    assert(gold == result)
	}
}
