package ppl.dsl.forge
package examples

import core.{ForgeApplication, ForgeApplicationRunner}

object OldWrangleRunner extends ForgeApplicationRunner with OldWrangle

trait OldWrangle extends ForgeApplication with ScalaOps {

  //name of the DSL
  def dslName = "OldWrangle"

  //DSL specification
  def specification() = {
    // Include Scala Ops
    addScalaOps()

    //Types
    val T = tpePar("T")
    val DataWrangler = tpe("DataWrangler")
    val Table = tpe("Table")
    val Column = tpe("Column", List(T)) // parametrized columns?
    val Transform = tpe("Transform")
    val ArrayColumn = tpeInst(MArray(T), List(Column))
    val ArrayTransform = tpeInst(MArray(T), List(Transform))

    //Data Structures
    data(DataWrangler, List(), ("_transforms", ArrayTransform), ("_numTransforms", MInt))
    data(Table, List(), ("_columns", ArrayColumn), ("_numColumns", MInt)) // todo - by Name
    data(Column, List(T), ("_data", MArray(T)), ("length", MInt))

    //Ops
    val stream = ForgePrinter()

    // -- DataWrangler
    val dw_new = op (DataWrangler) ("apply", static, List(), List(MInt), DataWrangler, codegenerated, effect = mutable)
    val dw_apply_transforms = op (DataWrangler) ("apply_transforms", infix, List(), List(DataWrangler, Table), Table, codegenerated)
    val dw_apply_to_file = op (DataWrangler) ("apply_to_file", infix, List(), List(DataWrangler, MString), Table, codegenerated)
    // todo - T withBound Transform
    val dw_add = op (DataWrangler) ("add", infix, List(), List(DataWrangler, Transform, MInt), MUnit, codegenerated, effect = write(0))  
  
    // -- Table  
    // todo - clean name
    val table_new = op (Table) ("apply", static, List(), List(MInt), Table, codegenerated, effect = mutable)
    val table_insert_column = op (Table) ("insert_column", infix, List(), List(Table, Column, MInt), MUnit, codegenerated, effect = write(0))

    // -- Column
    val column_new = op (Column) ("apply", static, List(T), List(MInt), Column, codegenerated, effect = mutable)
    val column_append = op (Column) ("append", infix, List(T), List(Column, T, MInt), MUnit, codegenerated, effect = write(0))

    // -- Transform
    // todo
    val transform_new = op (Transform) ("applyTransform", static, List(), List(), Transform, codegenerated, effect = mutable)

    //Code Generators - these operations are likely not supported by Forge
    // -- Datawrangler 
    codegen (dw_new) ($cala, "new "+dw_new.tpeName+"(new Array[Transform]("+dw_new.quotedArg(0)+"), "+dw_new.quotedArg(0)+")")
    
    codegen (dw_apply_transforms) ($cala, stream.printLines(
      "var cur : Table = " + dw_apply_transforms.quotedArg(1),
      dw_apply_transforms.quotedArg(0) + "._transforms filter (t => t.status = \"active\") foreach (t=> cur = t.apply_transform(cur))",
      "cur"
    ), isSimple = false)

    codegen (dw_apply_to_file) ($cala, stream.printLines(
      "import java.io.File",
      "val table = new Table(new Array[Column](1), 1)",  // TODO - temporary for POC
      "val column = new Column[String](new Array[String](1024), 1024)",
      "val file = new File("+dw_apply_to_file.quotedArg(0)+")",
      "if (!file.isFile) throw new RuntimeException(\" Could not find input file\")",
      "val input_file = scala.io.Source.fromFile(file)",
      "val firstTransform = transforms(0)",
      "if(firstTransform.name == \"split \") {", // TODO - other checks
      "val lines = input_file.getLines()", // TODO - scalalize
      "for(i <- 0 until lines.length) {column.append(column, lines(i), i)}",
      "firstTransform.status = \"inactive\"",
      "} else {",
      "column.set_data(input_file.mkString)",
      "}",
      "table.insert_column(table, column)",
      "apply_transforms(this, table)"
    ), isSimple = false)

    // todo - eliminate quotedArg(2) with Var instance variable
    codegen (dw_add) ($cala, dw_add.quotedArg(0) + "._transforms("+dw_add.quotedArg(2)+") = " + dw_add.quotedArg(1))

    // -- Table
    codegen (table_new) ($cala, "new "+table_new.tpeName+"(new Array[Column]("+table_new.quotedArg(0)+"), "+table_new.quotedArg(0)+")")
    codegen (table_insert_column) ($cala, "")    

    // -- Column
    // not sure about this working as a parametrized column
    codegen (column_new) ($cala, "new "+column_new.tpeName+"["+column_new.tpeInstance(0)+"](new Array[Column]("+column_new.quotedArg(0)+"), "+column_new.quotedArg(0)+")")
    codegen (column_append) ($cala, "")     

    // -- Transform
    // todo
    codegen (transform_new) ($cala, "new "+transform_new.tpeName+"()")   

    ()
  }
}


