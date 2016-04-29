import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object ArgInOutCompiler extends DHDLApplicationCompiler with ArgInOut
object ArgInOutInterpreter extends DHDLApplicationInterpreter with ArgInOut
trait ArgInOut extends DHDLApplication {

  def main() {

    //println("vec1: " + vec1.mkString(", "))
    //println("vec2: " + vec2.mkString(", "))

  	val a = ArgIn[SInt]("a")
  	val b = ArgIn[SInt]("b")
    val out = ArgOut[SInt]("out")

    setArg(a, 1)
    setArg(b, 1)

    Accel {
			Pipe{
				out := a.value + b.value
			}
    }

  }
}
