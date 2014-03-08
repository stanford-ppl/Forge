import optiml.compiler._
import optiml.library._
import optiml.shared._

object NetflixCompiler extends OptiMLApplicationCompiler with Netflix
object NetflixInterpreter extends OptiMLApplicationInterpreter with Netflix

trait Netflix extends OptiMLApplication { 
  def print_usage = {
    println("Usage: Netflix <train set> <m> <n>")
    exit(-1)
  }

  def main() = {
    val cy = readMatrix[Int](args(0), line => line.toInt, " ")
    //val c = readMatrix(args(0))
    val m = args(1).toInt
    val n = args(2).toInt
    val r = args(3).toInt
    
    val mu = args(4).toDouble
    val sigma = args(5).toDouble

    val alpha = args(6).toDouble

    println(m)
    println(n)
    println(r)

    println(cy.numRows)
    println(cy.numCols)

    // val cyx_d = (0::cy.numRows) { k => cy(k, 2).toDouble }
    // val cyx_i = (0::cy.numRows) { k => cy(k, 0) % m }
    // val cyx_j = (0::cy.numRows) { k => cy(k, 1) % n }

    // println(cyx_d.length)
    // println(cyx_i.length)
    // println(cyx_j.length)

    // val cyc = SparseMatrix[Double](m, n, cyx_d, cyx_j, cyx_i).finish

    // println(cyc.numCols)
    // println(cyc.numRows)

    // val cyr = SparseMatrix[Double](n, m, cyx_d, cyx_i, cyx_j).finish

    // println(cyr.numCols)
    // println(cyr.numRows)

    // initialize the matrix
    val v0 = (0::(m+n), 0::r) { (i, j) => 
      if(i == j) 1.0 else 0.0
    }

    val result = untilconverged(v0) { v =>
      val dv = v + mu * y_step(m, n, r, cy, v) + sigma * v * (v.t * v)
      println(normf(dv))
      v - alpha * dv
    }

    println("result:")
    println(normf(result))
  }

  def y_step(m: Rep[Int], n: Rep[Int], r: Rep[Int], cy: Rep[DenseMatrix[Int]], v: Rep[DenseMatrix[Double]]): Rep[DenseMatrix[Double]] = {
    val mdvhi = (0::cy.numRows).groupByReduce(
      { k => cy(k, 0) % m },
      { k =>
        val i = cy(k, 0) % m
        val j = cy(k, 1) % n
        val y = cy(k, 2).toDouble

        val vi = v.getRow(i + n).toDense
        val vj = v.getRow(j).toDense

        val xmy = (vi *:* vj) - y

        xmy * vj
      },
      { (x: Rep[DenseVector[Double]], y: Rep[DenseVector[Double]]) => x + y })
    
    val mdvhj = (0::cy.numRows).groupByReduce(
      { k => cy(k, 1) % n },
      { k =>
        val i = cy(k, 0) % m
        val j = cy(k, 1) % n
        val y = cy(k, 2).toDouble

        val vi = v.getRow(i + n).toDense
        val vj = v.getRow(j).toDense

        val xmy = (vi *:* vj) - y

        xmy * vi
      },
      { (x: Rep[DenseVector[Double]], y: Rep[DenseVector[Double]]) => x + y })

    val mdv = (0::(m+n), 0::r) { (i, j) =>
      if (i < n) {
        if(mdvhi.contains(i)) {
          mdvhi.apply(i).apply(j)
        }
        else {
          0.0
        }
      }
      else {
        if(mdvhj.contains(i - n)) {
          mdvhj.apply(i - n).apply(j)
        }
        else {
          0.0
        }
      }
    }

    mdv
  }

  def normf(x: Rep[DenseMatrix[Double]]) = {
    sqrt(sum(0, x.numRows){ i => sum(0, x.numCols) { j =>  
      val y = x(i,j)
      y * y
    }})
  }
}
