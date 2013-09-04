package ppl.dsl.forge
package templates
package shared

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import core._

import Utilities._

// should we try to make use of LMS platform-specific generators here? anything we can gain?
trait BaseGenDataStructures extends ForgeCodeGenBase {
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def makeFieldArgs(s: Exp[DSLData]) = {
    var argStr = ""
    for ((name,tpe) <- s.fields) {
      // val arg = "__" + name + ": " + quotePrimitive(tpe) + ", "
      val arg = "__" + name + ": " + quote(tpe) + ", "
      argStr += arg
    }
    argStr.dropRight(2) // final comma
  }

  def makeFieldsWithInitArgs(s: Exp[DSLData]) = {
    var fieldStr = ""
    for ((name,tpe) <- s.fields) {
      val arg = name + " = __" + name
      fieldStr += "  var " + arg + nl
    }
    fieldStr
  }

  def emitScalaStruct(s: Exp[DSLData], stream: PrintWriter) {
    stream.print("class " + s.tpe.name)
    stream.print(makeTpeParsWithBounds(s.tpe.tpePars))
    stream.print("(")
    stream.print(makeFieldArgs(s))
    stream.print(") {")
    stream.println()
    stream.println(makeFieldsWithInitArgs(s))
    stream.println("}")
  }
}
