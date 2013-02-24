package ppl.dsl.forge
package examples

import core.{ForgeApplication, ForgeApplicationRunner}

object OptiWrangleRunner extends ForgeApplicationRunner with OptiWrangle

trait OptiWrangle extends ForgeApplication with ScalaOps {

  //name of the DSL
  def dslName = "OptiWrangle"

  //DSL specification
  def specification() = {
    // Include Scala Ops
    addScalaOps()

    //Types
    val Table = tpe("Table")
    val AS = tpeInst(GArray(tpePar("T")), List(MString))
    val AAS = tpeInst(GArray(tpePar("T")), List(AS))

    data(Table, List(), ("_length", MInt), ("_data", AAS), ("_header", AS))
    
    val stream = ForgePrinter()

    // Delite Collection Implementation
    val table_new = op (Table) ("apply", static, List(), List(MInt), Table, codegenerated, effect = mutable)
    val table_length = op (Table) ("length", infix, List(), List(Table), MInt, codegenerated) 
    val table_apply = op (Table) ("apply", infix, List(), List(Table, MInt), AS, codegenerated)
    val table_update = op (Table) ("update", infix, List(), List(Table, MInt, AS), MUnit, codegenerated, effect= write(0))

    val table_copy = op (Table) ("apply", static, List(), List(MInt, AAS, AS), Table, codegenerated)

    codegen (table_new) ($cala, "new "+table_new.tpeName+"("+quotedArg(0)+", new Array[Array[String]]("+quotedArg(0)+"), new Array[String]("+quotedArg(0)+"))")
    codegen (table_length) ($cala, quotedArg(0) + "._length")
    codegen (table_apply) ($cala, quotedArg(0) + "._data.apply("+quotedArg(1)+")")
    codegen (table_update) ($cala, quotedArg(0) + "._data.update("+quotedArg(1)+", "+quotedArg(2)+")")

    codegen (table_copy) ($cala, "new "+table_copy.tpeName+"("+quotedArg(0)+","+quotedArg(1)+","+quotedArg(2)+")")

    Table is DeliteCollection(AS, table_new, table_length, table_apply, table_update)

    // global helpers // accessors
    val table_getData = op (Table) ("data", infix, List(), List(Table), AAS, codegenerated)
    val table_getData_row = op (Table) ("data", infix, List(), List(Table, MInt), AS, codegenerated)
    val table_getHeader = op (Table) ("header", infix, List(), List(Table), AS, codegenerated)
    codegen (table_getData) ($cala, quotedArg(0) + "._data")
    codegen (table_getData_row) ($cala, quotedArg(0) + "._data("+quotedArg(1)+")")
    codegen (table_getHeader) ($cala, quotedArg(0) + "._header")

    val table_headerIndex = op (Table) ("headerIndex", infix, List(), List(Table, MString), MInt, codegenerated)
    codegen(table_headerIndex) ($cala, stream.printLines(
      "val x =" +quotedArg(0) + "._header",
      "var index = -1",
      "for (i <- 0 until x.length) {",
      " if(x(i) == "+quotedArg(1)+") index = i",
      "}",
      "index"
    ))
  
    // separate into full and convenience
    //val table_cut = op (Table) ("cut", infix, List(), List(Table, MString, MString, MInt, MString, MString, MInt, MInt), Table, 
  //map((AS, AS, Table), 0, "e => "+quotedArg(0)+".table_cut_all_helper(e, "+quotedArg(1)+")"))

    val table_cut_all = op (Table) ("cut", infix, List(), List(Table, MString), Table, map((AS, AS, Table), 0, "e => "+quotedArg(0)+".table_cut_all_helper(e, "+quotedArg(1)+")"))
    
    val table_cut_all_helper = op (Table) ("table_cut_all_helper", infix, List(), List(Table, AS, MString), AS, codegenerated)
    codegen (table_cut_all_helper) ($cala, quotedArg(1) + " map (x => x.replaceAll("+quotedArg(2)+", \"\"))")
    
    // Drop by column index
    val table_drop = op (Table) ("drop", infix, List(), List(Table, MInt), Table, map((AS, AS, Table), 0, "e => "+quotedArg(0)+".table_drop_helper(e, "+quotedArg(1)+")"))
    val table_drop_helper = op (Table) ("table_drop_helper", infix, List(), List(Table, AS, MInt), AS, codegenerated)
    codegen (table_drop_helper) ($cala, quotedArg(1) + ".take("+quotedArg(2)+") ++ "+quotedArg(1)+".drop("+quotedArg(2)+"+1)")

    // drop by column name (header)
    //val table_drop_header = op (Table) ("drop", infix, List(), List(Table, MString), Table, single(Table, {quotedArg(0)+".drop("+quotedArg(0)+".headerIndex("+quotedArg(1)+"))"}))

    // a filter operation
    //val table_delete = op (Table) ("delete", infix, List(), List(Table, MInt, MString, MInt), Table, map((AS, AS, Table), 0, "e => "+quotedArg(0)+".table_delete_helper(e, "+quotedArg(1)+", "+quotedArg(2)+", "+quotedArg(3)+")"))
    //codegen (table_delete_helper) ($cala, quotedArg(1) + "" )

    val table_delete = op (Table) ("delete", infix, List(), List(Table, MInt, MString, MInt), Table, codegenerated) 
    codegen (table_delete) ($cala, stream.printLines("val newarr = " +quotedArg(0) + "._data filter (x => !(x("+quotedArg(1)+").indexOf("+quotedArg(2)+") == "+quotedArg(3)+"))",
    "new " + table_delete.tpeName +"(newarr.length, newarr, "+quotedArg(0)+"._header)"
    ))
 
    val table_delete_row = op (Table) ("delete", infix, List(), List(Table, MInt), Table, codegenerated)
    codegen (table_delete_row) ($cala, stream.printLines(
      "val newarr = "+quotedArg(0)+"._data.take("+quotedArg(1)+")  ++ "+quotedArg(0)+"._data.drop("+quotedArg(1)+"+1)",
      "new "+table_delete_row.tpeName+"(newarr.length, newarr, "+quotedArg(0)+"._header)"
    ))

    val table_promote = op (Table) ("promote", infix, List(), List(Table, MInt), Table, single(Table, {
      stream.printLines(
        "val newHeader = " + quotedArg(0) + ".data("+quotedArg(1)+")",
        "val newTable = "+quotedArg(0) +".delete("+quotedArg(1)+").setHeader(newHeader)",
        "newTable"
    )}))
 
    val table_setHeader = op (Table) ("setHeader", infix, List(), List(Table, AS), Table, single(Table, {
    stream.printLines(
      "Table("+quotedArg(0)+".length(),"+quotedArg(0)+".data(),"+quotedArg(1)+")"
    )}))

    // todo - autodetect headers or lack-there-of
    val table_fromFile = op (Table) ("apply", static, List(), List(MString, MString), Table, codegenerated)    
    codegen (table_fromFile) ($cala, stream.printLines(
      "import io.Source",
      "val lines = Source.fromFile("+quotedArg(0)+").getLines().map(x => x.split("+quotedArg(1)+").toArray).toArray",
      // todo conditionally lines.drop(1)
      "new " +table_fromFile.tpeName+"(lines.length, lines, new Array[String](0))"  
    ))

    val table_writeToFile = op (Table) ("write", infix, List(), List(Table, MString, MString), MUnit, codegenerated)
    codegen (table_writeToFile) ($cala, stream.printLines(
      "import java.io._",
      "println(\"Well at least it got called \")",
      "val pw = new PrintWriter(new File("+quotedArg(1)+"))",
      "var headerIndex = 0",
      "var header = " + quotedArg(0) + ".header()",
      "while (headerIndex < header.length-1) { ",
      " pw.write(header(headerIndex)+"+quotedArg(2)+")",
      " headerIndex += 1",
      "}",
      "pw.write(header(headerIndex))",
      "pw.write(\"\\n\")",
      "var i = 0",
      "while (i < "+quotedArg(0)+".length) {",
      " var j = 0",
      " while (j < "+quotedArg(0)+"(i).length-1) {",
      "   pw.write("+quotedArg(0)+"(i).apply(j)+"+quotedArg(2)+")",
      "   j += 1",
      " }",
      " pw.write("+quotedArg(0)+"(i).apply(j))",
      " pw.write(\"\\n\")",
      " i += 1",
      "}",
      "pw.close()"
    ))

    val testMessage = "\"Accessed Test\""
    val test = op (Table) ("test", infix, List(), List(Table), MUnit, codegenerated)
    codegen (test) ($cala, "println("+testMessage+")")

    ()
  }
}
