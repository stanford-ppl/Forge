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

    println(normf(v0))

    println(v0.numRows)
    println(v0.numCols)

    println("test 1")

    val mdvhi = (0::cy.numRows).groupByReduce(
      { k => cy(k, 0) % m },
      { k =>
        val i = cy(k, 0) % m
        val j = cy(k, 1) % n
        val y = cy(k, 2).toDouble

        val vi = v0.getRow(i + n).toDense
        val vj = v0.getRow(j).toDense

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

        val vi = v0.getRow(i + n).toDense
        val vj = v0.getRow(j).toDense

        val xmy = (vi *:* vj) - y

        xmy * vi
      },
      { (x: Rep[DenseVector[Double]], y: Rep[DenseVector[Double]]) => x + y })

    val mdv = (0::(m+n), 0::r) { (i, j) =>
      if (i < n) {
        mdvhi.apply(i).apply(j)
      }
      else {
        mdvhj.apply(i - n).apply(j)
      }
    }

    // val mdv = sum(0, cy.numRows) { k =>
    //   val i = cy(k, 0) % m
    //   val j = cy(k, 1) % n
    //   val y = cy(k, 2).toDouble

    //   //val ei = (0::(m+n)) { k => if(k == i + n) 1.0 else 0.0 }
    //   //val ej = (0::(m+n)) { k => if(k == j) 1.0 else 0.0 }

    //   val vi = v0.getRow(i + n)
    //   val vj = v0.getRow(j)

    //   val xmy = (vi *:* vj) - y

    //   // (0::(m+n), 0::r) { (mi, mj) =>
    //   //   if(mi == i + n) {
    //   //     xmy * vj(mj)
    //   //   }
    //   //   else if(mi == j) {
    //   //     xmy * vi(mj)
    //   //   }
    //   //   else {
    //   //     0.0
    //   //   }
    //   // }
    //   xmy * (vi + vj)
    //   //xmy * (ei.t ** vj + ej.t ** vi)
    // }

    // println("test 2")

    // val mdvi = (0::n) { i =>
    //   val ci = cyr.getRowAsSparseVector(i)
    //   val cii = ci.indices
    //   val ciy = ci.nz

    //   val vi = v0.getRow(i).toDense

    //   if(cii.length == 0) {
    //     DenseVector.zeros(r)
    //   }
    //   else {
    //     sum(0, cii.length) { k =>
    //       val vj = v0.getRow(cii(k) + n).toDense
    //       val y: Rep[Double] = ciy(k)
    //       val xmy: Rep[Double] = (vi *:* vj) - y
    //       xmy * vj
    //     }
    //   }
    // }

    // val mdvj = (0::m) { i =>
    //   val ci = cyc.getRowAsSparseVector(i)
    //   val cii = ci.indices
    //   val ciy = ci.nz

    //   val vi = v0.getRow(i + n).toDense

    //   if(cii.length == 0) {
    //     DenseVector.zeros(r)
    //   }
    //   else {
    //     sum(0, cii.length) { k =>
    //       val vj = v0.getRow(cii(k)).toDense
    //       val y: Rep[Double] = ciy(k)
    //       val xmy: Rep[Double] = (vi *:* vj) - y
    //       xmy * vj
    //     }
    //   }
    // }

    // val mdv2 = (0::(m+n), 0::r) { (i, j) =>
    //   if (i < n) {
    //     mdvi.apply(i).apply(j)
    //   }
    //   else {
    //     mdvj.apply(i - n).apply(j)
    //   }
    // }

    // println(mdv2.numRows)
    // println(mdv2.numCols)

    // println(normf(mdv2))
    println(normf(mdv))
    // println(normf(mdv - mdv2))
  }

  def normf(x: Rep[DenseMatrix[Double]]) = {
    sqrt(sum(0, x.numRows){ i => sum(0, x.numCols) { j =>  
      val y = x(i,j)
      y * y
    }})
  }
}
