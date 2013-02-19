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

    data(Table, List(), ("_length", MInt), ("_data", AAS))
    
    val stream = ForgePrinter()

    val table_new = op (Table) ("apply", static, List(), List(MInt), Table, codegenerated, effect = mutable)
    val table_length = op (Table) ("length", infix, List(), List(Table), MInt, codegenerated) 
    val table_apply = op (Table) ("apply", infix, List(), List(Table, MInt), AS, codegenerated)
    val table_update = op (Table) ("update", infix, List(), List(Table, MInt, AS), MUnit, codegenerated, effect= write(0))


    codegen (table_new) ($cala, "new "+table_new.tpeName+"("+quotedArg(0)+", new Array[Array[String]]("+quotedArg(0)+"))")
    codegen (table_length) ($cala, quotedArg(0) + "._length")
    codegen (table_apply) ($cala, quotedArg(0) + "._data.apply("+quotedArg(1)+")")
    codegen (table_update) ($cala, quotedArg(0) + "._data.update("+quotedArg(1)+", "+quotedArg(2)+")")

    Table is DeliteCollection(AS, table_new, table_length, table_apply, table_update)

    val table_cut = op (Table) ("cut", infix, List(), List(Table, MString), Table, map((AS, AS, Table), 0, "e => "+quotedArg(0)+".table_cut_helper(e, "+quotedArg(1)+")"))
    
    val table_cut_helper = op (Table) ("table_cut_helper", infix, List(), List(Table, AS, MString), AS, codegenerated)
    codegen (table_cut_helper) ($cala, quotedArg(1) + " map (x => x.replaceAll("+quotedArg(2)+", \"\"))")
    
    val table_drop = op (Table) ("drop", infix, List(), List(Table, MInt), Table, map((AS, AS, Table), 0, "e => "+quotedArg(0)+".table_drop_helper(e, "+quotedArg(1)+")"))
    val table_drop_helper = op (Table) ("table_drop_helper", infix, List(), List(Table, AS, MInt), AS, codegenerated)
    codegen (table_drop_helper) ($cala, quotedArg(1) + ".take("+quotedArg(2)+") ++ "+quotedArg(1)+".drop("+quotedArg(2)+"+1)")

    // a filter operation
    //val table_delete = op (Table) ("delete", infix, List(), List(Table, MInt, MString, MInt), Table, map((AS, AS, Table), 0, "e => "+quotedArg(0)+".table_delete_helper(e, "+quotedArg(1)+", "+quotedArg(2)+", "+quotedArg(3)+")"))
    //codegen (table_delete_helper) ($cala, quotedArg(1) + "" )

    val table_delete = op (Table) ("delete", infix, List(), List(Table, MInt, MString, MInt), Table, codegenerated) 
    codegen (table_delete) ($cala, stream.printLines("val newarr = " +quotedArg(0) + "._data filter (x => !(x("+quotedArg(1)+").indexOf("+quotedArg(2)+") == "+quotedArg(3)+"))",
    "new " + table_delete.tpeName +"(newarr.length, newarr)"
    ))
  

    val table_fromFile = op (Table) ("apply", static, List(), List(MString, MString), Table, codegenerated)    
    codegen (table_fromFile) ($cala, stream.printLines(
      "import io.Source",
      "val lines = Source.fromFile("+quotedArg(0)+").getLines().map(x => x.split("+quotedArg(1)+").toArray).toArray",
      "new " +table_fromFile.tpeName+"(lines.length, lines)" 
    ))

    val table_writeToFile = op (Table) ("write", infix, List(), List(Table, MString, MString), MUnit, codegenerated)
    codegen (table_writeToFile) ($cala, stream.printLines(
      "import java.io._",
      "val pw = new PrintWriter(new File("+quotedArg(1)+"))",
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

    ()
  }
}
