import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object ArgInOutTestCompiler extends DHDLApplicationCompiler with ArgInOutTest
object ArgInOutTestInterpreter extends DHDLApplicationInterpreter with ArgInOutTest
trait ArgInOutTest extends DHDLApplication {


  def main() {
    val N = 8
  	val x = ArgIn[SInt]("x")
  	val y = ArgOut[SInt]("y")
    setArg(x, N)
    Accel {
      Pipe { y := x + 4 }
    }
    val result = getArg(y)
    println("result = " + result)
  }
}
