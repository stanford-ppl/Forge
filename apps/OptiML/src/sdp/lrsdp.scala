import optiml.compiler._
import optiml.library._
import optiml.shared._

object LRSDPCompiler extends OptiMLApplicationCompiler with LRSDP
object LRSDPInterpreter extends OptiMLApplicationInterpreter with LRSDP

trait LRSDP extends OptiMLApplication { 
  def print_usage = {
    println("Usage: LRSDP <coo matrix>")
    exit(-1)
  }

  def main() = {
    val cl = readVector[Tup3[Int,Int,Double]](args(0), line => pack(line(0).toInt, line(1).toInt, line(2).toDouble), " ")
    //val c = readMatrix(args(0))
    val v0 = readMatrix(args(1))
    val soln = readMatrix(args(2))
    val alpha = args(3).toDouble

    val n = v0.numRows
    val m = v0.numCols

    val cmb = SparseMatrix[Double](n, n, (0::cl.length) { i => cl(i)._3 }, (0::cl.length) { i => cl(i)._1 }, (0::cl.length) { i => cl(i)._2 })
    val c = cmb.finish

    println(n)
    println(m)

    val result = untilconverged(v0, maxIter = 100) { v =>
      val x = v*v.t
      println(normf(x - soln))
      // val cv = DenseMatrix((0::n) map { k =>
      //   sum(0,c.length) { l =>
      //     val (i, j, s) = unpack(c(l))
      //     if((i == k)&&(j == k)) {
      //       v.getRow(k) * (s * 2)
      //     }
      //     else if(i == k) {
      //       v.getRow(j) * s
      //     }
      //     else if(j == k) {
      //       v.getRow(i) * s
      //     }
      //     else {
      //       DenseVector.zeros(m)
      //     }
      //   }
      // })

      // val cv = DenseMatrix((0::n) map { k =>
      //   val (i, j, s) = unpack(c(k))
      //   v.getRow(k) * (s * 2)
      // })

      val cv = c * v
      val vvtv = v * (v.t * v)
      val dv = vvtv - cv
      //println(sum(0,n)(i => dv.getRow(i) *:* dv.getRow(i)))
      v - (dv * alpha)
    }
  }

  def normf(x: Rep[DenseMatrix[Double]]) = {
    sqrt(sum(0,x.numRows)(i => x.getRow(i) *:* x.getRow(i)))
  }
}
