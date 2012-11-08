package ppl.dsl.forge
package templates
package compiler

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import core._

import Utilities._

// should we try to make use of LMS platform-specific generators here? anything we can gain?
trait DeliteGenDataStructures extends ForgeCodeGenBase { 
  this: ForgeCodeGenDelite =>
   
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._
    
  def emitScalaStructs(path: String) {
    val scalaPath = path+File.separator+"scala"+File.separator
    Directory(Path(scalaPath)).createDirectory()
    for (s <- DataStructs) {        
      val stream = new PrintWriter(new FileWriter(scalaPath+s.tpe.name+".scala"))
      stream.println("package " + packageName + ".datastruct.scala")
      stream.println()
      emitBlockComment("Back-end data structure class; gets instantiated in generated code.", stream)
      stream.println()
      stream.print("class " + s.tpe.name)
      stream.print(makeTpeArgsWithBounds(s.tpeArgs))
      stream.print("(")  
      var argStr = ""          
      for (f <- s.fields) {
        val (name, tpe) = f
        val arg = "__" + name + ": " + quote(tpe) + ", "
        argStr += arg
      }    
      stream.print(argStr.dropRight(2)) // final comma        
      stream.print(") {")
      stream.println()
      for (f <- s.fields) {
        val (name, tpe) = f
        val arg = name + " = __" + name
        stream.println("  var " + arg)
      }
      stream.println("}")
      stream.close()
    }
  }
  
  def emitStructs(path: String) {
    for (g <- generators) {
      g match {
        case `$cala` => emitScalaStructs(path)
        case _ => // not implemented yet
      }        
    }
  }
}