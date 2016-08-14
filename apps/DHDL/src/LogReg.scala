import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object LogRegCompiler extends DHDLApplicationCompiler with LogReg
object LogRegInterpreter extends DHDLApplicationInterpreter with LogReg
trait LogReg extends DHDLApplication {
  type Array[T] = ForgeArray[T]
  type Elem = Flt
  type T = Flt

  val tileSizeH = 192
  val innerParH = 16
  val outerParH = 2
  lazy val tileSize = param(tileSizeH)
  lazy val outerMpPar = param(outerParH)
  lazy val innerMpPar = param(1)
  lazy val innerPar   = param(innerParH)
  lazy val noPar = param(1)

  val N = 1152
  lazy val D = 768.as[SInt]
  val A = 1

  def sigmoid(t:Rep[Elem]) = 1.as[Elem]/(exp(-t)+1)

  def logreg(x_in: Rep[Array[Elem]], y_in: Rep[Array[Elem]], theta: Rep[Array[Elem]]) {

    val N = 192
    val D = 192

    val BN = param(96); domainOf(BN) = (96,9600,96)
    val PX = param(1);  domainOf(PX) = (1,1,1)
    val P0 = param(1);  domainOf(P0) = (1,3,1)
    val P1 = param(1);  domainOf(P1) = (1,D,1)
    val P2 = param(innerParH);  domainOf(P2) = (1,D,1)
    val P3 = param(1);  domainOf(P3) = (1,96,1)

    val x = OffChipMem[Elem](N, D)
    val y = OffChipMem[Elem](N)
    val theta = OffChipMem[Elem](D)

    setMem(x, x_in)
    setMem(y, y_in)

    Accel {
      val gradAcc = BRAM[Elem](D)
      Fold(N by BN par P0, P1)(gradAcc, 0.as[T]){ i =>
        val xB = BRAM[Elem](BN, D)
        val yB = BRAM[Elem](BN)
        val btheta = BRAM[Elem](D)
        Parallel {
          xB := x(i::i+BN, 0::D, P2)
          yB := y(i::i+BN, P3)
          btheta := theta(0::D, P2)
        }
        val gradient = BRAM[Elem](D)
        Fold(BN par P3, P2)(gradient, 0.as[T]){ ii =>
          val pipe2Res = Reg[Elem]
          val subRam   = BRAM[Elem](D)

          val dotAccum = Reduce(D par P2)(0.as[T]){ j => xB(ii,j) * btheta(j) }{_+_}
          Pipe { pipe2Res := (yB(ii) - sigmoid(dotAccum.value)) }
          Pipe(D by 1) {j => subRam(j) = xB(ii,j) - pipe2Res.value }
          subRam
        }{_+_}
      }{_+_}

      val outerTheta = BRAM[Elem](D)
      val newTheta = BRAM[Elem](D)
      outerTheta := theta(0::D, innerPar)
      Pipe ((D by 1) par innerPar) { j => newTheta(j) = gradAcc(j)*A + outerTheta(j) }
      theta(0::D, innerPar) := newTheta
    }
    getMem(theta)
  }

  def main() {
    val N = 192
    val D = 192

    /*domainOf(tileSize) = (96,9600,96)
    domainOf(outerMpPar) = (1,3,1)
    domainOf(innerMpPar) = (1,1,1)
    domainOf(innerPar) = (1,192,1)
    domainOf(noPar) = (1,1,1)*/


    val sX = Array.fill(N){ Array.fill(D){ random[Elem](10.0)} }
    val sY = Array.fill(N)( random[Elem](10.0) )
    val theta = Array.fill(D) {random[Elem](1.0) }

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

    val result = logreg(sX.flatten,sY, theta)
  }
}
