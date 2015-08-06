import optiml.compiler._
import optiml.library._
import optiml.shared._

import scala.virtualization.lms.common.Record

object ScratchpadRunner extends OptiMLApplicationCompiler with Scratchpad
trait Scratchpad extends OptiMLApplication {

  def parseRow(r_in: Rep[DenseVector[String]]): Rep[DenseVector[Double]] = {
    // drop bad rows
    if (r_in.length != 10) DenseVector[Double]()
    else {
      DenseVector(
        r_in(0).toDouble,
        r_in(1).toDouble,
        r_in(5).toDouble,
        r_in(7).toDouble,
        r_in(9).toDouble
      )
    }
  }

  def inMemory() = {
    val t1 = time()
    val data = readMatrixAndParse[Double]("raw.dat", v => parseRow(v), "\\|")
    val t2 = time(data)
    println("initial pass took " + (t2-t1)/1000.0 + "s")

    val customers = data.groupRowsBy(row => row(0))
    val accounts = data.groupRowsBy(row => row(1))
    val t3 = time(customers, accounts)
    println("computed hashes (" + (t3-t2)/1000.0 + "s)")

    val features = accounts.toVector.map { account =>
      // account is looked up from the HashStream as a Rep[DenseMatrix[Double]] (the value in the bucket)
      val custId = account(0, 0)
      val customer = customers(custId)

      DenseVector[Double](
        customer.getCol(2).min,
        customer.getCol(3).max,
        customer.getCol(4).sum
      )
    }

    val t4 = time(features)
    println("computed " + features.length + " feature vectors (" + (t4-t3)/1000.0 + "s)")
    writeMatrix(DenseMatrix(features), "features_inmem.dat", "|")
  }

  // All of these operations persist to disk and only use memory as a cache.
  def inStream() = {
    val t1 = time()
    val raw = FileStream("raw.dat")
    val data = raw.mapRows("etl.dat", "\\|", "|") { v => parseRow(v) }
    val t2 = time(data)
    println("initial pass took " + (t2-t1)/1000.0 + "s")

    // There is some weirdness going in converting a double value to a string key here.
    // We need to use a canonical representation of the double, so we use Scala's (rather than
    // the formatted version we read from the file).
    val customers = data.groupRowsBy("cust.hash", "\\|")(row => ""+row(0).toDouble, _.map(_.toDouble))
    val accounts = data.groupRowsBy("accounts.hash", "\\|")(row => ""+row(1).toDouble, _.map(_.toDouble))
    val t3 = time(customers, accounts)
    println("computed hashes (" + (t3-t2)/1000.0 + "s)")

    val features = accounts.mapValues("features.dat", "|") { (acctId, account) =>
      // account is looked up from the HashStream as a Rep[DenseMatrix[Double]] (the value in the bucket)
      val custId = account(0, 0)
      val customer = customers(""+custId)

      DenseVector[Double](
        customer.getCol(2).min,
        customer.getCol(3).max,
        customer.getCol(4).sum
      )
    }

    customers.close()
    accounts.close()

    val t4 = time(features)
    println("computed feature vectors (" + (t4-t3)/1000.0 + "s)")
  }

  def main() = {
    // val raw = DenseMatrix.rand(1000000,8)
    // val custIds = (0::1000000) { i => (i % 100).toDouble }
    // val accountIds = (0::1000000) { i => (i % 1000).toDouble }
    // val initialData = custIds.t.toMat <<| accountIds.t.toMat <<| raw
    // writeMatrix(initialData, "raw.dat", "|")

    tic("inMemory")
    inMemory()
    toc("inMemory")

    tic("inStream")
    inStream()
    toc("inStream")
  }
}
