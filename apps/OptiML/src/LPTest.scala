import optiml.compiler._
import optiml.library._
import optiml.shared._

object LPTestCompiler extends OptiMLApplicationCompiler with LPTest
object LPTestInterpreter extends OptiMLApplicationInterpreter with LPTest

trait LPTest extends OptiMLApplication {
  def printUsage = {
    println("Usage: LPTest <test size>")
    exit(-1)
  }

  def main() = {
    if (args.length < 1) printUsage

    val test_size = args(0).toInt

    val x8 = (0::test_size) { i =>
        ((i + 234) * (i + 344534345)).toShort
    }

    val y8 = (0::test_size) { i =>
        ((i + 232) * (i + 345654465)).toShort
    }

    tic("dot", x8.size, y8.size)
    //val xydot = (x8 *:* y8)
    val xydot = sum(0, test_size) { i => x8(i) * y8(i)}
    toc("dot", xydot)
    println("dot(x8, y8): " + xydot)

    tic("fpdot", x8.size, y8.size)
    val xyfpdot = sum(0, test_size) { i => (x8(i).toFloat) * (y8(i).toFloat)}
    toc("fpdot", xyfpdot)
    println("fpdot(x8, y8): " + xyfpdot)

    tic("lpdot", x8.size, y8.size)
    val xylpdot = DenseVector.lpdot(x8, y8)
    toc("lpdot", xylpdot)
    println("lpdot(x8, y8): " + xylpdot)

    // val x8 = args(0).toByte
    // val y8 = args(1).toByte

    // println("x8: " + x8)
    // println("y8: " + y8)
    // println("x8 + y8: " + (x8 + y8))
    // println("x8 * y8: " + (x8 * y8))

    // val x16 = args(0).toShort
    // val y16 = args(1).toShort

    // println("x16: " + x16)
    // println("y16: " + y16)
    // println("x16 + y16: " + (x16 + y16))
    // println("x16 * y16: " + (x16 * y16))

    // val v8 = DenseVector(x8, y8)
    // val n2: Rep[Byte] = (v8 *:* v8)
    
    // println("dot(v8, v8): " + n2)
    // println("lpdot(v8, v8): " + DenseVector.lpdot(v8, v8))
  }
}
