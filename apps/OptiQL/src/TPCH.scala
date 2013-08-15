import optiql.compiler._
import optiql.library._
import optiql.shared._

import scala.virtualization.lms.common.Record

object Q1Compiler extends OptiQLApplicationCompiler with Q1
object Q1Interpreter extends OptiQLApplicationInterpreter with Q1

trait Q1 extends OptiQLApplication {

  type LineItem = Record {
    val l_orderkey: Int
    val l_partkey: Int
    val l_suppkey: Int
    val l_linenumber: Int
    val l_quantity: Double
    val l_extendedprice: Double
    val l_discount: Double
    val l_tax: Double
    val l_returnflag: Char
    val l_linestatus: Char
    val l_shipdate: Date
    val l_commitdate: Date
    val l_receiptdate: Date
    val l_shipinstruct: String
    val l_shipmode: String
    val l_comment: String
  }

  def fromLine(line: Rep[String]): Rep[LineItem] = {
    val fields = line.fsplit("\\Q" + "|" + "\\E")
    new Record {
      val l_orderkey = { date_object_apply(array_apply(fields, 10).asInstanceOf[Rep[String]]); array_apply(fields, 0).toInt }
      val l_partkey = array_apply(fields, 1).toInt
      val l_suppkey = array_apply(fields, 2).toInt
      val l_linenumber = array_apply(fields, 3).toInt
      val l_quantity = array_apply(fields, 4).toDouble
      val l_extendedprice = array_apply(fields, 5).toDouble
      val l_discount = array_apply(fields, 6).toDouble
      val l_tax = array_apply(fields, 7).toDouble
      val l_returnflag = infix_fcharAt(array_apply(fields, 8), 0) //.fcharAt doesn't resolve for some reason
      val l_linestatus = infix_fcharAt(array_apply(fields, 9), 0)
      val l_shipdate = Date(array_apply(fields, 10))
      val l_commitdate = Date(array_apply(fields, 11))
      val l_receiptdate = Date(array_apply(fields, 12))
      val l_shipinstruct = array_apply(fields, 13)
      val l_shipmode = array_apply(fields, 14)
      val l_comment = array_apply(fields, 15)
    }
  }

  def printUsage = {
    println("Usage: Q1 <input tpch directory>")
    exit(-1)
  }

  def loadLineItems(tpchDataPath: Rep[String]) = Table.fromFile(tpchDataPath+"/lineitem.tbl", fromLine)

  def main() = {
    println("TPCH Query 1")
    if (args.length < 1) printUsage
    val lineItems = loadLineItems(args(0))
    query(lineItems)
  }

  def query(lineItems: Rep[Table[LineItem]]) = {
    tic(lineItems)

    val q = lineItems Where(_.l_shipdate <= Date("1998-12-01")) GroupBy(l => (l.l_returnflag,l.l_linestatus)) Select(g => new Record {
      val returnFlag = g.key._1
      val lineStatus = g.key._2
      val sumQty = g.value.Sum(_.l_quantity)
      val sumBasePrice = g.value.Sum(_.l_extendedprice)
      val sumDiscountedPrice = g.value.Sum(l => l.l_extendedprice * (1.0d - l.l_discount))
      val sumCharge = g.value.Sum(l => l.l_extendedprice * (1.0d - l.l_discount) * (unit(1.0d) + l.l_tax))  // unit required for + for some reason (infix fails to resolve otherwise)
      val avgQty = g.value.Average(_.l_quantity)
      val avgPrice = g.value.Average(_.l_extendedprice)
      val avgDiscount = g.value.Average(_.l_discount)
      val countOrder = g.value.Count
    }) OrderBy(_.returnFlag) ThenBy(_.lineStatus)

    toc(q)
    println(q)
  }
}
