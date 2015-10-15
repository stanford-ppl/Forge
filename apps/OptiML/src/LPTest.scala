import optiml.compiler._
import optiml.library._
import optiml.shared._

object LPTest8Compiler extends OptiMLApplicationCompiler with LPTest8
object LPTest8Interpreter extends OptiMLApplicationInterpreter with LPTest8

object LPTest16Compiler extends OptiMLApplicationCompiler with LPTest16
object LPTest16Interpreter extends OptiMLApplicationInterpreter with LPTest16

trait LPTest8 extends OptiMLApplication {
  def printUsage = {
    println("Usage: LPTest8 <test size>")
    exit(-1)
  }

  def main() = {
    if (args.length < 1) printUsage

    val test_size = args(0).toInt

    val x = (0::test_size) { i =>
        ((i + 234) * (i + 344534345)).toByte
    }

    val y = (0::test_size) { i =>
        ((i + 232) * (i + 345654465)).toByte
    }

    tic("dot", x, y)
    //val xydot = (x *:* y)
    val xydot = sum(0, test_size) { i => x(i) * y(i)}
    toc("dot", xydot)
    println("dot(x, y): " + xydot)

    tic("fpdot", x, y)
    val xyfpdot = sum(0, test_size) { i => (x(i).toFloat) * (y(i).toFloat)}
    toc("fpdot", xyfpdot)
    println("fpdot8(x, y): " + xyfpdot)

    tic("lpdot", x, y)
    val xylpdot = DenseVector.lpdot(x, y)
    toc("lpdot", xylpdot)
    println("lpdot8(x, y): " + xylpdot)
  }
}


trait LPTest16 extends OptiMLApplication {
  def printUsage = {
    println("Usage: LPTest16 <test size>")
    exit(-1)
  }

  def main() = {
    if (args.length < 1) printUsage

    val test_size = args(0).toInt

    val x = (0::test_size) { i =>
        ((i + 234) * (i + 344534345)).toShort
    }

    val y = (0::test_size) { i =>
        ((i + 232) * (i + 345654465)).toShort
    }

    tic("dot", x, y)
    //val xydot = (x *:* y)
    val xydot = sum(0, test_size) { i => x(i) * y(i)}
    toc("dot", xydot)
    println("dot(x, y): " + xydot)

    tic("fpdot", x, y)
    val xyfpdot = sum(0, test_size) { i => (x(i).toFloat) * (y(i).toFloat)}
    toc("fpdot", xyfpdot)
    println("fpdot8(x, y): " + xyfpdot)

    tic("lpdot", x, y)
    val xylpdot = DenseVector.lpdot(x, y)
    toc("lpdot", xylpdot)
    println("lpdot8(x, y): " + xylpdot)
  }
}
