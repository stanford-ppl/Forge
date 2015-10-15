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
        ((i + 234) * (i + 344534345)).toByte
    }

    val y8 = (0::test_size) { i =>
        ((i + 232) * (i + 345654465)).toByte
    }

    tic("dot", x8, y8)
    //val xydot = (x8 *:* y8)
    val xydot = sum(0, test_size) { i => x8(i) * y8(i)}
    toc("dot", xydot)
    println("dot8(x8, y8): " + xydot)

    tic("fpdot8", x8, y8)
    val xyfpdot = sum(0, test_size) { i => (x8(i).toFloat) * (y8(i).toFloat)}
    toc("fpdot8", xyfpdot)
    println("fpdot8(x8, y8): " + xyfpdot)

    tic("lpdot", x8, y8)
    val xylpdot = DenseVector.lpdot(x8, y8)
    toc("lpdot8", xylpdot)
    println("lpdot8(x8, y8): " + xylpdot)



    val x16 = (0::test_size) { i =>
        ((i + 234) * (i + 344534345)).toShort
    }

    val y16 = (0::test_size) { i =>
        ((i + 232) * (i + 345654465)).toShort
    }

    tic("dot16", x16, y16)
    //val xydot = (x16 *:* y16)
    val xydot16 = sum(0, test_size) { i => x16(i) * y16(i)}
    toc("dot16", xydot16)
    println("dot(x16, y16): " + xydot16)

    tic("fpdot16", x16, y16)
    val xyfpdot16 = sum(0, test_size) { i => (x16(i).toFloat) * (y16(i).toFloat)}
    toc("fpdot16", xyfpdot16)
    println("fpdot(x16, y16): " + xyfpdot16)

    tic("lpdot16", x16, y16)
    val xylpdot16 = DenseVector.lpdot(x16, y16)
    toc("lpdot16", xylpdot16)
    println("lpdot(x16, y16): " + xylpdot16)

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
