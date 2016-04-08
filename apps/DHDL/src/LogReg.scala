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
  def logreg(x: Rep[OffChipMem[Elem]], y: Rep[OffChipMem[Elem]], theta: Rep[OffChipMem[Elem]],
		gradient:Rep[OffChipMem[Elem]]) {
    val xB = BRAM[Elem]("xB", rts, cts)
    val yB = BRAM[Elem]("yB", rts)
		Sequential(numIter by 1) { iter =>
			MetaPipe(nrow by rts) { itr =>
    		val dB = BRAM[Elem]("dB", rts)
				Parallel {
					/* Calculate dot product for row tile */
    			val tB1 = BRAM[Elem]("tB1", cts)
					BlockReduce(ncol by cts, dB) { itc =>
						Parallel {
							xB := x(itr::itr+rts, itc::itc+rts)
							tB1 := theta(itc::itc+cts)
						}
    				val dAccOuter = BRAM[Elem]("dAccOuter", rts)
						BlockReduce(cts by 1, dAccOuter) { ic =>
    					val dAccInner = BRAM[Elem]("dAccInner", rts)
							Pipe(rts by 1) { ir =>
								dAccInner(ir) = tB1(ic) * xB(ir, ic)
							}
							dAccInner
						}{_+_}
						dAccOuter
					}{_+_}
					/* Load y for row tile */
					yB := y(itr::itr+rts)
				}
				/* Calculate partial gradient for a row tile and accumulate to offchip*/
				MetaPipe(ncol by cts) { itc =>
					xB := x(itr::itr+rts, itc::itc+rts)
					val gradAcc = BRAM[Elem]("gradAcc", cts)
					gradAcc := gradient(itc::itc+cts)
					MetaPipe(cts by 1) {ic =>
						/* Calculate partial gradient of a row tile for 1 column */
						val gradRTC = Reg[Elem]("gradRTC")
						Pipe(rts by 1, gradRTC) {ir =>
							xB(ir,ic) * ( yB(ir) - sigmoid(dB(ir)) )
						}{_+_}
						Pipe {
							gradAcc(ic) = gradRTC.value + gradAcc(ic)
						}
					}
					gradient(itc::itc+cts) := gradAcc 
				}
			}
			/* multiply gradient with alpha and accumulate to previous theta */
			val gB = BRAM[Elem]("gB", cts)
    	val tB2 = BRAM[Elem]("tB2", cts)
			MetaPipe(ncol by cts) { itc =>
				tB2 := theta(itc::itc+cts)
				gB := gradient(itc::itc+cts)
				MetaPipe(cts by 1) {ic =>
					tB2(ic) = tB2(ic) + gB(ic) * alpha.value
				}
				theta(itc::itc+cts) := tB2
			}
		}
  }

  def main() {
		val NR = 4 // Number of rows
		val NC = 6 // Number of columns
		val rts = 2 // Row tile size
		val cts = 2 // Column tile size
		val A = 1.0
		val MI = 30

    val x = OffChipMem[Elem]("x", NR, NC)
    val y = OffChipMem[Elem]("y", NR)
    val theta = OffChipMem[Elem]("theta", NC)
    val gradient = OffChipMem[Elem]("gradient",NC)

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
      logreg(x, y, theta, gradient)
    }
    val result = getMem(theta)

	 	var sTheta = Array.fill(NC) { 0.as[Elem] }
		(0 until MI).foreach { iter =>
			val gradient = sX.zip(sY) { case (xr, ye) =>
				xr.map( _ * ( ye - sigmoid(sTheta.zip(xr) {case (a,b) => a*b}.reduce(_+_)) ) )
			}.reduce { case (v1,v2) => v1.zip(v2) { case (a,b) => a + b}}	
			sTheta = sTheta.zip(gradient.map( _ * A)) { case (a,b) => a + b}
		}
	 	val gold = sTheta 
		/*
		*/
    //println("expected: " + gold.mkString)
    //println("result: " + result.mkString)
    //assert(result == gold)
  }
}
