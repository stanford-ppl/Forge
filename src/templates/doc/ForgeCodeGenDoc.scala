package ppl.dsl.forge
package templates
package doc

import java.io.{File,PrintWriter,FileWriter}
import scala.tools.nsc.io.{Directory,Path}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import core._
import shared._
import Utilities._

trait ForgeCodeGenDoc extends ForgeCodeGenBackend {
  val IR: ForgeApplicationRunner with ForgeExp
  import IR._

  lazy val targetName = "doc"

  def emitDSLDocumentation() = {
    Directory(Path(dslDir)).createDirectory()
    emitDSLDefinition()
    // emitDataStructures()
    emitOps()
  }

}
