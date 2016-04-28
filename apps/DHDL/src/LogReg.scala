import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object LogRegCompiler extends DHDLApplicationCompiler with LogReg
object LogRegInterpreter extends DHDLApplicationInterpreter with LogReg
trait LogReg extends DHDLApplication {
  type Elem = FltPt[B23, B8]

  override def stageArgNames = List("rts", "cts")
  lazy val rts = stageArgOrElse[Int](0, 2)
  lazy val cts = stageArgOrElse[Int](1, 2)
  lazy val nrow = ArgIn[SInt]("nrow")
  lazy val ncol = ArgIn[SInt]("ncol")
  lazy val alpha = ArgIn[Elem]("alpha")
  lazy val numIter = ArgIn[SInt]("numIter")

	def sigmoid(t:Rep[Elem]) = {
		1.as[Elem]/(1+exp(-t))
	}
  def logreg(x: Rep[OffChipMem[Elem]], y: Rep[OffChipMem[Elem]], theta: Rep[OffChipMem[Elem]]) {
		//Sequential(numIter by 1) { iter =>
    val tB = BRAM[Elem]("tB", cts)
    Sequential {
			tB := theta(0::ncol.value)
		  val gradAcc = BRAM[Elem]("gradAcc", cts)
		  MetaPipe(nrow by rts) { itr =>
        val xB = BRAM[Elem]("xB", rts, cts)
        val yB = BRAM[Elem]("yB", rts)
		  	Parallel {
		  		xB := x(itr::itr+rts, 0::ncol.value)
		  		yB := y(itr::itr+rts)
		  	}
		  	MetaPipe(rts by 1) { ir =>
		  		val dotAccum = Reg[Elem]("dotAccum")
		  	  Pipe(ncol by 1, dotAccum) { ic =>
            xB(ir,ic) * tB(ic)
		  		}{_+_}
		  		val pipe2Res = Reg[Elem]("pipe2Res")
		  	  Pipe {
		  	    pipe2Res := (yB(ir) - sigmoid(dotAccum.value))
		  	  }
          val tB2 = BRAM[Elem]("tB2", cts)
          Pipe (ncol by 1) {ic =>
            tB2(ic) = xB(ir,ic) - pipe2Res.value
          }
          Pipe (ncol by 1) {ic =>
            gradAcc(ic) = gradAcc(ic) + tB2(ic)
          }
        }
		  }
      Pipe (ncol by 1) { ic =>
        tB(ic) = gradAcc(ic)*alpha.value + tB(ic)
      }
			theta(0::ncol.value) := tB
    }
  }

  def main() {
		val NR = 4 // Number of rows
		val NC = 6 // Number of columns
		val rts = 2 // Row tile size
		val cts = 2 // Column tile size
		val A = 1.0
		val MI = 30

    val x = OffChipMem[Elem]("x", nrow.value, ncol.value)
    val y = OffChipMem[Elem]("y", nrow.value)
    val theta = OffChipMem[Elem]("theta", ncol.value)

		val sX = Array.fill(NR){ Array.fill(NC){ random[Elem](10.0)} }
		val sY = Array.fill(NR)( random[Elem](10.0) )

    println("x: " + sX.mkString(", "))
   	println("y: " + sY.mkString(", "))

    setArg(nrow, NR)
    setArg(ncol, NC)
    setArg(alpha, A)
    setArg(numIter, MI)
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
