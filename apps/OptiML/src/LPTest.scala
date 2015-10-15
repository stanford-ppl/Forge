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


object LPLogRegCompiler extends OptiMLApplicationCompiler with LPLogReg
object LPLogRegInterpreter extends OptiMLApplicationInterpreter with LPLogReg

trait LPLogReg extends OptiMLApplication {
  def printUsage = {
    println("Usage: LPLogReg <input training data file> <input training label file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) printUsage

    val xfp = readMatrix(args(0))
    val yfp = readVector(args(1)).t

    println("x.numRows: " + xfp.numRows)
    println("x.numCols: " + xfp.numCols)
    println("y.length:  " + yfp.length)

    val x: Rep[DenseMatrix[Byte]] = xfp map {z => z.toByte}
    val y: Rep[DenseVector[Byte]] = yfp map {z => z.toByte}

    tic()
    val theta = (0::x.numCols) { (0.0).toByte }

    // gradient descent with logistic function
    val alpha = 1.0

    val w = untilconverged_withdiff(theta, maxIter = 30) { (cur, iter) =>
      val gradient = ((0::x.numRows) { i =>
        val ui: Rep[Float] = (y(i).toFloat) - (sigmoid(DenseVector.lpdot(cur, x(i)).toDouble).toFloat) 
        x map {z => z.toFloat * ui}
      }).sum

      // println("gradient: ")
      // gradient.pprint

      // alpha*gradient returns an inaccessible type when using implicits (instead of infix)
      val v = cur + gradient*alpha
      // println("next value (c): ")
      // z.pprint
      v
    }((x,y) => 1.0)

    toc(w)
    println("w:")
    (w map {z => z.toFloat}).pprint
  }
}

