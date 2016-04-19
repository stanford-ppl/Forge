import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object LogRegCompiler extends DHDLApplicationCompiler with LogReg
object LogRegInterpreter extends DHDLApplicationInterpreter with LogReg
trait LogReg extends DHDLApplication {
  type Elem = Flt

  lazy val tileSize = param("tileSize", 96)
  lazy val outerPar = param("outerPar", 1)
  lazy val innerPar = param("innerPar", 1)

  lazy val N = ArgIn[SInt]("nrow")
  lazy val D = 16.as[SInt]
  lazy val A = ArgIn[Elem]("alpha")

  def sigmoid(t:Rep[Elem]) = 1.as[Elem]/(1+exp(-t))

  def logreg(x: Rep[OffChipMem[Elem]], y: Rep[OffChipMem[Elem]], theta: Rep[OffChipMem[Elem]]) {
    val tB = BRAM[Elem]("tB", D)
    Sequential {
      tB := theta(0::D)
      val gradAcc = BRAM[Elem]("gradAcc", D)
      MetaPipe((N by tileSize) par unit(1)) { i =>
        val xB = BRAM[Elem]("xB", tileSize, D)
        val yB = BRAM[Elem]("yB", tileSize)
        Parallel {
          xB := x(i::i+tileSize, 0::D)
          yB := y(i::i+tileSize)
        }
        MetaPipe((tileSize by 1) par unit(1)) { ii =>
          val dotAccum = Reg[Elem]("dotAccum")
          val pipe2Res = Reg[Elem]("pipe2Res")
          val tB2 = BRAM[Elem]("tB2", D)

          Pipe((D by 1) par innerPar, dotAccum) { j => xB(ii,j) * tB(j) }{_+_}

          Pipe { pipe2Res := (yB(ii) - sigmoid(dotAccum.value)) }
          Pipe((D by 1) par innerPar) {j => tB2(j) = xB(ii,j) - pipe2Res.value }
          Pipe((D by 1) par innerPar) {j => gradAcc(j) = gradAcc(j) + tB2(j) }
        }
      }
      Pipe ((D by 1) par innerPar) { j => tB(j) = gradAcc(j)*A.value + tB(j) }
      theta(0::D) := tB
    }
  }

  def main() {
    val nrows = args(unit(0)).to[SInt];   bound(N) = 9993600
    val alpha = args(unit(1)).to[Elem]

    val x = OffChipMem[Elem]("x", nrows, D)
    val y = OffChipMem[Elem]("y", nrows)
    val theta = OffChipMem[Elem]("theta", D)

    val sX = Array.fill(nrows){ Array.fill(D){ random[Elem](10.0)} }
    val sY = Array.fill(nrows)( random[Elem](10.0) )

    println("x: " + sX.mkString(", "))
    println("y: " + sY.mkString(", "))

    setArg(N, nrows)
    setArg(A, alpha)
    setMem(x, sX.flatten)
    setMem(y, sY)

    /* OptiMl version
    val w = untilconverged(theta, maxIter = 30) { (cur, iter) =>
      val gradient = ((0::x.numRows) { i =>
        x(i)*(y(i) - sigmoid(cur *:* x(i)))
      }).sum
      val z = cur + gradient*alpha
      z
    }
    */

    Accel {
      logreg(x, y, theta)
    }
    val result = getMem(theta)
  }
}