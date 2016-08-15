import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object LogRegCompiler extends DHDLApplicationCompiler with LogReg
object LogRegInterpreter extends DHDLApplicationInterpreter with LogReg
trait LogReg extends DHDLApplication {
  type Array[T] = ForgeArray[T]
  type Elem = Flt
  type T = Flt

  val tileSizeH = 12345
  val innerParH = 16
  val outerParH = 1
  lazy val tileSize = param(tileSizeH)
  lazy val outerMpPar = param(outerParH)
  lazy val innerMpPar = param(1)
  lazy val innerPar   = param(innerParH)
  lazy val noPar = param(1)

  val N = 1152
  val D = 768
  val A = 1

  def sigmoid(t:Rep[Elem]) = 1.as[Elem]/(exp(-t)+1)

  def logreg(x_in: Rep[Array[Elem]], y_in: Rep[Array[Elem]], theta_in: Rep[Array[Elem]]) {

    val BN = param(96); domainOf(BN) = (96,9600,96)
    val PX = param(1);  domainOf(PX) = (1,1,1)
    val P0 = param(1);  domainOf(P0) = (1,4,1)
    val P1 = param(1);  domainOf(P1) = (1,D,1)
    val P2 = param(1);  domainOf(P2) = (1,D,1)
    val P3 = param(1);  domainOf(P3) = (1,96,1)

    val x = OffChipMem[Elem](N, D)
    val y = OffChipMem[Elem](N)
    val theta = OffChipMem[Elem](D)

    setMem(x, x_in)
    setMem(y, y_in)
    setMem(theta, theta_in)

    Accel {
      val btheta = BRAM[Elem](D)
      btheta := theta(0::D, P2)

      val gradAcc = BRAM[Elem](D)
      Fold(N by BN par P0, P1)(gradAcc, 0.as[T]){ i =>
        val xB = BRAM[Elem](BN, D)
        val yB = BRAM[Elem](BN)
        Parallel {
          xB := x(i::i+BN, 0::D, P2)
          yB := y(i::i+BN, P3)
        }
        val gradient = BRAM[Elem](D)
        Fold(BN par P3, P2)(gradient, 0.as[T]){ ii =>
          val pipe2Res = Reg[Elem]
          val subRam   = BRAM[Elem](D)

          val dotAccum = Reduce(D par P2)(0.as[T]){ j => xB(ii,j) * btheta(j) }{_+_}
          Pipe { pipe2Res := (yB(ii) - sigmoid(dotAccum.value)) }
          Pipe(D par P2) {j => subRam(j) = xB(ii,j) - pipe2Res.value }
          subRam
        }{_+_}
      }{_+_}

      val newTheta = BRAM[Elem](D)
      Pipe (D par P2) { j => newTheta(j) = gradAcc(j)*A + btheta(j) }
      theta(0::D, P2) := newTheta
    }
    getMem(theta)
  }

  def main() {
    val sX = Array.fill(N){ Array.fill(D){ random[Elem](10.0)} }
    val sY = Array.fill(N)( random[Elem](10.0) )
    val theta = Array.fill(D) {random[Elem](1.0) }

    val result = logreg(sX.flatten,sY, theta)

    // println("x: " + sX.mkString(", "))
    // println("y: " + sY.mkString(", "))



    /* OptiMl version
    val w = untilconverged(theta, maxIter = 30) { (cur, iter) =>
      val gradient = ((0::x.numRows) { i =>
        x(i)*(y(i) - sigmoid(cur *:* x(i)))
      }).sum
      val z = cur + gradient*alpha
      z
    }
    */

  }
}
