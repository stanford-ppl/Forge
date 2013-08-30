package ppl.dsl.forge
package templates
package ident

import java.io.{File,PrintWriter,FileWriter}
import scala.tools.nsc.io.{Directory,Path}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import core._
import shared._
import Utilities._

trait ForgeCodeGenIdent extends ForgeCodeGenBackend with BaseGenOps with IdentGenOps {
  val IR: ForgeApplicationRunner with ForgeExp
  import IR._

  lazy val targetName = "ident"

  def emitDSLImplementation() {
    Directory(Path(dslDir)).createDirectory()
    emitDSLSpecification()
  }

  def emitDSLSpecification() {
    val dslStream = new PrintWriter(new FileWriter(dslDir+dsl+"DSLI.scala"))
    dslStream.println("package " + packageName)
    dslStream.println()
    dslStream.println("import ppl.dsl.forge.core.{ForgeApplication,ForgeApplicationRunner}")
    dslStream.println()
    dslStream.println("object " + dsl + "DSLRunner extends ForgeApplicationRunner with " + dsl + "DSL")
    dslStream.println()
    dslStream.println("trait " + dsl + "DSL extends ForgeApplication { this: ppl.dsl.forge.core.ForgeOpsExp => ")
    emitBlockComment("The name of your DSL. This is the name that will be used in generated files, package declarations, etc.", dslStream, indent=2)
    emitWithIndent("def dslName = \""+dslName+"\"", dslStream, indent=2)
    dslStream.println()
    emitBlockComment("The specification is the DSL definition (types, data structures, ops)", dslStream, indent=2)
    emitWithIndent("def specification() = {", dslStream, indent=2)

    // internals go here
    emitClasses(dslStream)

    emitWithIndent("()", dslStream, indent=2)
    emitWithIndent("}", dslStream, indent=2)
    dslStream.println("}")
    dslStream.close()
  }

  def emitClasses(stream: PrintWriter) {
    for ((grp,opsGrp) <- OpsGrp) {
      emitWithIndent("// "+grp.name + " / " + opsGrp.name, stream, indent=4)
      emitWithIndent("def spec" + grp.name + "() = {", stream, indent=4)
      emitClass(opsGrp, stream)
      emitWithIndent("}", stream, indent=4)
      emitWithIndent("spec" + grp.name + "()", stream, indent=4)
      stream.println()
      stream.println()
    }
  }
}

