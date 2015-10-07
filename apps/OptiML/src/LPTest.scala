import optiml.compiler._
import optiml.library._
import optiml.shared._

object LPTestCompiler extends OptiMLApplicationCompiler with LPTest
object LPTestInterpreter extends OptiMLApplicationInterpreter with LPTest

trait LPTest extends OptiMLApplication {
  def printUsage = {
    println("Usage: LogReg <input 1> <input 2>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) printUsage

    val x = args(0).toLong
    val y = args(1).toLong

    println("x: " + x)
    println("y: " + y)
    println("x + y: " + (x + y))
    println("x * y: " + (x * y))

  }
}
