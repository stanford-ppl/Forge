package optiql.compiler.datastruct.scala

import ppl.delite.runtime.data._
import scala.collection.mutable.HashMap

object TablePrinter {

  def writeAsJSON(table: AnyRef, path: String) {
    val numRows = tableSize(table)
    val data = table.getClass.getMethod("data").invoke(table)
    val columnStrings = if (data.isInstanceOf[Array[_]]) getCaseClassFields(data.asInstanceOf[Array[_]](0).getClass) else getCaseClassFields(data.getClass)

    val xfs = new java.io.BufferedWriter(new java.io.FileWriter(path))
    xfs.write("[")
    var firstO = true
    for (i <- 0 until numRows) {
      if(!firstO) xfs.write(",")
      firstO = false
      xfs.write("{")
      var firstI = true
      for (name <- columnStrings) {
        if (!firstI) xfs.write(",")
        firstI = false
        xfs.write("\"" + name + "\" : \"" + readArray(data, name, i) + "\"")
      }
      xfs.write("}\n")
    }
    xfs.write("]")
    xfs.close()
  }

  def printAsTable(table: AnyRef, maxRows: Int) {

    implicit val tableStr = new StringBuilder
    val numRows = math.min(tableSize(table), maxRows)
    val data = table.getClass.getMethod("data").invoke(table)
    val columnStrings = if (data.isInstanceOf[Array[_]]) getCaseClassFields(data.asInstanceOf[Array[_]](0).getClass) else getCaseClassFields(data.getClass)
    val columnSizes = getTableColSizes(data, columnStrings, numRows)

    def repeat(s: String, n:Int) {
      for (i <- 0 until n) tableStr append s
    }

    def horizontalRule = {
      for(size <- columnSizes.values)
        repeat("=", size+1)
      tableStr append "=\n"
    }

    def emitRecordAsRow(row: Int) {
      tableStr append "| "

      for(col <- columnStrings) {
        val str = readArray(data, col, row)
        tableStr append str
        repeat(" ", columnSizes(col) - str.length - 1)
        tableStr append "| "
      }
      tableStr append "\n"
    }

    // Check if Table is empty
    if(numRows == 0) {
      println("=====================================================")
      println("|                  EMPTY TABLE                      |")
      println("=====================================================")
      return
    }

    horizontalRule
    tableStr append "|"
    for(col <- columnStrings) {
      tableStr append (" " + col)
      repeat(" " , columnSizes(col) - col.length - 1)
      tableStr append "|"
    }
    tableStr append "\n"
    horizontalRule

    for(r <- 0 until numRows) {
      emitRecordAsRow(r)
    }

    horizontalRule
    println(tableStr.toString)
  }

  private def tableSize(table: AnyRef) = table.getClass.getMethod("size").invoke(table) match {
    case i:Integer => i.intValue
    case i:java.lang.Long => i.intValue
    case i => throw new RuntimeException("Unexpected type: " + i + " for table size")
  }

  private def getCaseClassFields(clazz: Class[_]) = {
    import scala.reflect.runtime.{universe => ru}
    val mirror: ru.Mirror = ru.runtimeMirror(clazz.getClassLoader)
    val membersSorted: List[ru.Symbol] = mirror.classSymbol(clazz).toType.members.sorted // sorts members in declaration order

    val fields = membersSorted.map(_.toString).filter(_.endsWith("_="))
                  .map(_.stripSuffix("_=")).map(_.stripPrefix("method "))
                  .toArray
    if (fields.length == 0) Array("") else fields
  }

  private def readArray(x: Any, col: String, row: Int): String = x match {
    case d: DeliteArray[_] if col != "" => 
      val e = d.readAt(row)
      e.getClass.getMethod(col).invoke(e).toString
    case d: DeliteArray[_] => 
      d.readAt(row).toString
    case a: Array[_] if col != "" => 
      val e = a(row)
      e.getClass.getMethod(col).invoke(e).toString
    case a: Array[_] =>
      a(row).toString
    case _ if col != "" => 
      readArray(x.getClass.getMethod(col).invoke(x), "", row)
    case _ => 
      throw new IllegalArgumentException(x.getClass.getSimpleName + " cannot be printed as a table")

  }

  private def getTableColSizes(data: Any, columnStrings: Array[String], numRows: Int) = {
    val colSizes = new HashMap[String,Int]

    //columns should be at least the size of the headers
    for(col <- columnStrings) {
      colSizes(col) = col.length + 2
    }

    //columns should be at least the size of maximal element
    for (col <- columnStrings) {
      for (j <- 0 until numRows) {
        val d = readArray(data, col, j)
        colSizes(col) = math.max(colSizes(col), d.length + 2)
      }
    }

    colSizes
  }
}
