import optiml.compiler._
import optiml.library._
import optiml.shared._

object LPTestCompiler extends OptiMLApplicationCompiler with LPTest
object LPTestInterpreter extends OptiMLApplicationInterpreter with LPTest

trait LPTest extends OptiMLApplication {
  def printUsage = {
    println("Usage: LPTest <input 1> <input 2>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) printUsage

    val x8 = args(0).toByte
    val y8 = args(1).toByte

    println("x8: " + x8)
    println("y8: " + y8)
    println("x8 + y8: " + (x8 + y8))
    println("x8 * y8: " + (x8 * y8))

    val x16 = args(0).toShort
    val y16 = args(1).toShort

    println("x16: " + x16)
    println("y16: " + y16)
    println("x16 + y16: " + (x16 + y16))
    println("x16 * y16: " + (x16 * y16))

    val v8 = DenseVector(x8, y8)

    println("v8: " + v8)
    println("dot(v8, v8): " + (v8 *:* v8))
    println("lpdot(v8, v8): " + lpdot(v8, v8))
  }
}
