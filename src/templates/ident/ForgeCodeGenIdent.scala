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

trait ForgeCodeGenIdent extends ForgeCodeGenBackend with BaseGenOps with IdentGenOps {  
  val IR: ForgeApplicationRunner with ForgeExp  
  import IR._
  
  lazy val packageName = dsl.toLowerCase() + ".ident"
    
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
    dslStream.println("/**")
    dslStream.println(" * The name of your DSL. This is the name that will be used in generated files,")
    dslStream.println(" * package declarations, etc.")
    dslStream.println(" */")
    dslStream.println("def dslName = \"SimpleVector\"")
    dslStream.println("")
    dslStream.println("/**")
    dslStream.println(" * The specification is the DSL definition (types, data structures, ops)")
    dslStream.println(" */")
    dslStream.println("def specification() = {")

    // internals go here
    emitClasses(dslStream)

    dslStream.println("()")

    dslStream.println("}")
    dslStream.println("}")
    dslStream.close()
  }  
    
  def emitClasses(stream: PrintWriter) {
    for ((grp,opsGrp) <- OpsGrp) {

      stream.println("// "+grp.name + " / " + opsGrp.name)

      stream.println("def spec" + grp.name + "() = {")

      emitClass(opsGrp, stream)

      stream.println("}")
      stream.println("spec" + grp.name + "()")

      stream.println()
      stream.println()
    }
  }
}

