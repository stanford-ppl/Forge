package ppl.dsl.forge
package templates
package compiler

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import core._

import shared.BaseGenDataStructures
import Utilities._

/**
 * This file is deprecated in favor of using Delite structs. Is there any reason
 * we should keep it around? (Do we ever need concrete platform-specific data structures?)
 */

// should we try to make use of LMS platform-specific generators here? anything we can gain?
trait DeliteGenDataStructures extends BaseGenDataStructures {
  this: ForgeCodeGenDelite =>

  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def emitScalaStructs(path: String) {
    val scalaPath = path+File.separator+"scala"+File.separator
    Directory(Path(scalaPath)).createDirectory()
    for ((t,s) <- DataStructs) {
      val stream = new PrintWriter(new FileWriter(scalaPath+s.tpe.name+".scala"))
      stream.println("package " + packageName + ".datastruct.scala")
      stream.println()
      emitBlockComment("Back-end data structure class; gets instantiated in generated code.", stream)
      stream.println()
      emitScalaStruct(s,stream)
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
