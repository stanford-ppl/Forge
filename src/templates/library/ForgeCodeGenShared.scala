package ppl.dsl.forge
package templates
package shared

import java.io.{File,PrintWriter,FileWriter}
import scala.tools.nsc.io.{Directory,Path}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import core._

trait ForgeCodeGenShared extends ForgeCodeGenBackend with BaseGenPackages with BaseGenOps with BaseGenImports {  
  val IR: ForgeApplicationRunner with ForgeExp  
  import IR._
  
  lazy val dslStream = new PrintWriter(new FileWriter(dslDir+"Frontend.scala"))    
  lazy val packageName = dsl.toLowerCase() + ".shared"
    
  def emitDSLImplementation() {
    Directory(Path(dslDir)).createDirectory()
    emitDSLDefinition()
    emitOps()
  }
  
  def emitDSLDefinition() {
    dslStream.println("package " + packageName)
    dslStream.println()
    emitAllImports(dslStream)
    dslStream.println()
    emitApplicationRunnerBase(dslStream)
    dslStream.println()    
    emitScalaPackageDefinitionsBase(lmsAppOps, lmsCompOps, dslStream)
    dslStream.println()    
    emitDSLPackageDefinitionsBase(OpsGrp.values.toList, lmsCompOps, dslStream)
    dslStream.println()
    dslStream.close()
  }  
    
  def emitOps() {
    val opsDir = dslDir + File.separator + "ops"
    Directory(Path(opsDir)).createDirectory()
    
    // 1 file per tpe, includes only abstract Ops
    for ((tpe,ops) <- OpsGrp) {
      val stream = new PrintWriter(new FileWriter(opsDir+File.separator+tpe.name+"Ops"+".scala"))
      stream.println("package " + packageName + ".ops")
      stream.println()
      emitScalaReflectImports(stream)
      emitLMSImports(stream)
      emitDSLImports(stream)
      stream.println()
      emitOpSyntax(tpe, ops, stream)        
      stream.close()
    }                
  }  
  
}
