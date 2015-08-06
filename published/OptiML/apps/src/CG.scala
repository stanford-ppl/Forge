// import optiml.compiler._
// import optiml.library._
// import optiml.shared._

// object CGCompiler extends OptiMLApplicationCompiler with CG
// object CGInterpreter extends OptiMLApplicationInterpreter with CG
// object CGFunction extends OptiMLApplicationCompiler with CG {
//   registerFunction(conjugateGradient _)
//   override def functionName = "CG"
// }

import optiml.direct._

trait OptiMLApplication extends optiml.direct.OptiMLApplication {
  def args: Rep[Array[String]]
  implicit class IntOps2(x:Int) {
    def ::(y:Int): Rep[IndexVector] = unit(y)::unit(x)
  }
  //implicit def int2intRep(x:Int): Rep[Int] = unit(x)
  //implicit def int2doubleRep(x:Int): Rep[Double] = unit(x)
  implicit class StringOps2(s:Rep[String]) {
    def ++(x:Rep[Any]): Rep[String] = ???
    def ++[T](x:Var[T]): Rep[String] = ???
    def ^(x:Rep[Any]): Rep[String] = ???
    def ^[T](x:Var[T]): Rep[String] = ???
  }
  implicit def string2ops(s:String) = StringOps2(unit(s))
  implicit class IntVarOps(s:Var[Int]) {
    def +=(x:Rep[Int]): Rep[Unit] = ???
  }
  def infix_==(x:Rep[Any], y: Rep[Any]): Rep[Boolean] = ???
  def __assign[T](x:Var[T], y: T): Rep[Unit] = ???
  def __assign[T](x:Var[T], y: Rep[T]): Rep[Unit] = ???
}

import org.scala_lang.virtualized.virtualize  

@virtualize
trait CG extends OptiMLApplication { 

  def conjugateGradient(A0: Rep[ForgeArray[Double]], numRows: Rep[Int], numCols: Rep[Int], b0: Rep[ForgeArray[Double]], maxIters: Rep[Int]): Rep[ForgeArray[Double]] = {
    
    val A = DenseMatrix(A0, numRows, numCols)
    val b = DenseVector(b0, false)

    implicit def diffCG(t1: Rep[Tup3[DenseVector[Double],DenseVector[Double],DenseVector[Double]]],
                        t2: Rep[Tup3[DenseVector[Double],DenseVector[Double],DenseVector[Double]]]) = {
      val (x, r, p) = unpack(t2)
      sqrt(r *:* r)
    }

    val x0 = DenseVector.zeros(A.numCols).t

    val result = untilconverged(pack(x0, b, b), maxIter = maxIters) { (cur, iter) =>
      val (x, r, p) = unpack(cur)

      val Ap = A * p
      val alpha = (r *:* r) / (p *:* Ap)
      val x_next = x + alpha * p
      val r_next = r - alpha * Ap
      val beta = (r_next *:* r_next) / (r *:* r)
      val p_next = r_next + beta*p

      pack(x_next, r_next, p_next)
    }

    val (x_soln, r_soln, p_soln) = unpack(result)
    x_soln.toArray
  }

  def main() = {
    if (args.length < 3) printUsage()
    val A = readMatrix(args(0))
    val b = readVector(args(1)).t
    val expected_result = readVector(args(2)).t
    val maxIters = args(3).toInt

    val x_soln = DenseVector(conjugateGradient(A.toArray, A.numRows, A.numCols, b.toArray, maxIters), false)
 
    val err = x_soln - expected_result
    println(sqrt(err *:* err))
  }

  def printUsage() = {
    println("Usage: CG <input matrix> <input vector> <expected vector> <maximum iterations>")
    exit(-1)
  }

}
