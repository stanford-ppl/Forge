package ppl.dsl.forge
package templates
package shared

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import core._
import Utilities._

trait BaseGenOps extends ForgeCodeGenBase {  
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def baseOpsCls(tpe: Rep[DSLType]) = if (lmsAppOps contains variableOps) "Variables" else "Base"
  
  def makeOpArgs(o: Rep[DSLOp]) = "(" + o.args.zipWithIndex.map(t => opArgPrefix + t._2).mkString(",") + ")"
  def makeOpArgsWithType(o: Rep[DSLOp]) = "(" + o.args.zipWithIndex.map(t => opArgPrefix + t._2 + ": " + repify(t._1)).mkString(",") + ")"
  def makeOpImplicitArgs(o: Rep[DSLOp]) = 
    if (o.implicitArgs.length > 0) "(" + o.implicitArgs.zipWithIndex.map(t => implicitOpArgPrefix + t._2).mkString(",") + ")"
    else ""
  def makeOpImplicitArgsWithType(o: Rep[DSLOp], asVals: Boolean = false) = {
    val prefix = if (asVals == true) "val " else ""
    if (o.implicitArgs.length > 0) "(implicit " + o.implicitArgs.zipWithIndex.map(t => prefix + implicitOpArgPrefix + t._2 + ": " + t._1.name).mkString(",") + ")"
    else ""
  }
  
  def makeOpMethodName(tpe: Rep[DSLType], o: Rep[DSLOp]) = o.style match {
    case `static` => tpe.name.toLowerCase + "_object_" + o.name.toLowerCase
    case _ => tpe.name.toLowerCase + "_" + o.name.toLowerCase
  }
  def makeOpMethodNameWithArgs(tpe: Rep[DSLType], o: Rep[DSLOp]) = makeOpMethodName(tpe,o) + makeTpeArgs(o.tpeArgs) + makeOpArgs(o)
  def makeOpNodeName(tpe: Rep[DSLType], o: Rep[DSLOp]) = o.style match {
    case `static` => tpe.name + "Object_" + o.name.capitalize
    case _ => tpe.name + "_" + o.name.capitalize
  }    
  def makeOpNodeNameWithArgs(tpe: Rep[DSLType], o: Rep[DSLOp]) = makeOpNodeName(tpe,o) + makeOpArgs(o)
  
  def emitOpSyntax(tpe: Rep[DSLType], ops: DSLOps, stream: PrintWriter) {
    emitBlockComment("Operations", stream)
    stream.println()
    stream.println("trait " + tpe.name + "Ops extends " + baseOpsCls(tpe) + " {")
    stream.println("  this: " + dsl + " => ")
    stream.println()
    
    // static ops
    val staticOps = ops.ops.filter(e=>e.style==static)
    if (staticOps.length > 0) {
      stream.println("  object " + tpe.name + " {")
      for (o <- staticOps) {
        stream.print("    def " + o.name + makeTpeArgsWithBounds(o.tpeArgs))
        stream.print(makeOpArgsWithType(o))
        stream.print(makeOpImplicitArgsWithType(o))
        stream.print(" = " + makeOpMethodNameWithArgs(tpe,o))
        stream.println()
      }
      stream.println("  }")
    }
    stream.println()
    
    // infix ops
    val infixOps = ops.ops.filter(e=>e.style==infix)
    
    // certain ops (e.g. "apply" cannot be expressed with infix notation right now), so we use implicits as a workaround
    val needPimpClass = (noInfixList intersect infixOps.map(_.name)).nonEmpty
    val pimpStream = new StringBuilder()
    def infix_appendLine(x: StringBuilder, y: String) = x.append(y + System.getProperty("line.separator"))
    if (needPimpClass) {
      // set up a pimp-my-library style promotion
      pimpStream.appendLine("  implicit def repTo" + tpe.name + "Ops" + makeTpeArgsWithBounds(tpe.tpeArgs) + "(x: " + repify(tpe) + ") = new " + tpe.name + "OpsCls(x)")
      if (lmsAppOps.contains(variableOps)) {
        pimpStream.appendLine("  implicit def varTo" + tpe.name + "Ops" + makeTpeArgsWithBounds(tpe.tpeArgs) + "(x: " + varify(tpe) + ") = new " + tpe.name + "OpsCls(readVar(x))")
      }
      pimpStream.appendLine("")
      pimpStream.appendLine("  class " + tpe.name + "OpsCls" + makeTpeArgsWithBounds(tpe.tpeArgs) + "(val " + opArgPrefix + "0: " + repify(tpe) + ") {")
    }
    
    for (o <- infixOps) {
      if (noInfixList.contains(o.name)) {
        val otherArgs = "(" + o.args.drop(1).zipWithIndex.map(t => opArgPrefix + (t._2+1) + ": " + repify(t._1)).mkString(",") + ")"
        pimpStream.appendLine("    def " + o.name + makeTpeArgsWithBounds(o.tpeArgs.diff(tpe.tpeArgs)) + otherArgs + makeOpImplicitArgsWithType(o) + " = " + makeOpMethodNameWithArgs(tpe,o))
      }
      else {
        stream.print("  def infix_" + o.name + makeTpeArgsWithBounds(o.tpeArgs))
        stream.print(makeOpArgsWithType(o))
        stream.print(makeOpImplicitArgsWithType(o))
        stream.println(" = " + makeOpMethodNameWithArgs(tpe,o))        
      }
    }
    stream.println()      
    
    if (needPimpClass) {
      pimpStream.appendLine("  }")
      stream.println(pimpStream)
    }
    
    // abstract methods
    for (o <- ops.ops) {
      stream.print("  def " + makeOpMethodName(tpe,o) + makeTpeArgsWithBounds(o.tpeArgs))
      stream.print(makeOpArgsWithType(o))
      stream.print(makeOpImplicitArgsWithType(o) + ": " + repify(o.retTpe))
      stream.println()
    }
    
    stream.println("}")
  }
}
