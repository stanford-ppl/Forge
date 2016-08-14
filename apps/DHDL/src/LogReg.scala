import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object LogRegCompiler extends DHDLApplicationCompiler with LogReg
object LogRegInterpreter extends DHDLApplicationInterpreter with LogReg
trait LogReg extends DHDLApplication {
  type Elem = Flt
  type T = Flt

  val tileSizeH = 192
  val innerParH = 16
  val outerParH = 1
  lazy val tileSize = param(tileSizeH)
  lazy val outerMpPar = param(outerParH)
  lazy val innerMpPar = param(1)
  lazy val innerPar   = param(innerParH)
  lazy val noPar = param(1)

  val N = 1152
  lazy val D = 768.as[SInt]
  val A = 1

  def sigmoid(t:Rep[Elem]) = 1.as[Elem]/(exp(-t)+1)

  def logreg(x: Rep[OffChipMem[Elem]], y: Rep[OffChipMem[Elem]], theta: Rep[OffChipMem[Elem]]) {
    Sequential {
      val gradAcc = BRAM[Elem](D)
      Fold((N by tileSize) par outerMpPar, noPar)(gradAcc, 0.as[T]){ i =>
        val xB = BRAM[Elem](tileSize, D)
        val yB = BRAM[Elem](tileSize)
        val btheta = BRAM[Elem](D)
        Parallel {
          xB := x(i::i+tileSize, 0::D, innerPar)
          yB := y(i::i+tileSize, innerMpPar)
          btheta := theta(0::D, innerPar)
        }
        val gradient = BRAM[Elem](D)
        Fold((tileSize by 1) par innerMpPar, innerPar)(gradient, 0.as[T]){ ii =>
          val pipe2Res = Reg[Elem]
          val subRam   = BRAM[Elem](D)

          val dotAccum = Reduce((D by 1) par innerPar)(0.as[T]){ j => xB(ii,j) * btheta(j) }{_+_}
          Pipe { pipe2Res := (yB(ii) - sigmoid(dotAccum.value)) }
          Pipe((D by 1) par innerPar) {j => subRam(j) = xB(ii,j) - pipe2Res.value }
          subRam
        }{_+_}
      }{_+_}

      val outerTheta = BRAM[Elem](D)
      val newTheta = BRAM[Elem](D)
      outerTheta := theta(0::D, innerPar)
      Pipe ((D by 1) par innerPar) { j => newTheta(j) = gradAcc(j)*A + outerTheta(j) }
      theta(0::D, innerPar) := newTheta
    }
  }

  def main() {
    val nrows = N

    domainOf(tileSize) = (96,9600,96)
    domainOf(outerMpPar) = (1,3,1)
    domainOf(innerMpPar) = (1,1,1)
    domainOf(innerPar) = (1,192,1)
    domainOf(noPar) = (1,1,1)

    val x = OffChipMem[Elem](nrows, D)
    val y = OffChipMem[Elem](nrows)
    val theta = OffChipMem[Elem](D)

    val sX = Array.fill(nrows){ Array.fill(D){ random[Elem](10.0)} }
    val sY = Array.fill(nrows)( random[Elem](10.0) )

    // println("x: " + sX.mkString(", "))
    // println("y: " + sY.mkString(", "))

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
