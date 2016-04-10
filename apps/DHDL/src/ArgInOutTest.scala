import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object ArgInOutTestCompiler extends DHDLApplicationCompiler with ArgInOutTest
object ArgInOutTestInterpreter extends DHDLApplicationInterpreter with ArgInOutTest
trait ArgInOutTest extends DHDLApplication {

  lazy val x = ArgIn[SInt]("x")
  lazy val y = ArgOut[SInt]("y")

  def main() {
    val N = 8
    setArg(x, N)

    Accel {
      y := x + 4
    }
    val result = getArg(y)
    println("result = " + result)
  }
}
