package ppl.dsl.forge
package templates
package shared

import java.io.{File,PrintWriter,FileWriter}
import scala.tools.nsc.io.{Directory,Path}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import core._

trait ForgeCodeGenShared extends ForgeCodeGenBackend with BaseGenPackages with BaseGenLifts with BaseGenOps with BaseGenImports {  
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
    emitDSLPackageDefinitionsBase(OpsGrp.values.toList, dslStream)
    dslStream.println()
    dslStream.close()
  }  
    
  def emitOpsHeader(stream: PrintWriter) {
    stream.println("package " + packageName + ".ops")
    stream.println()
    emitScalaReflectImports(stream)
    emitLMSImports(stream)
    emitDSLImports(stream)
    stream.println()
  }
  
  def emitOps() {
    val opsDir = dslDir + File.separator + "ops"
    Directory(Path(opsDir)).createDirectory()
    
    // 1 file per grp, includes only abstract Ops
    for ((grp,ops) <- OpsGrp) {
      checkOps(ops)
      
      val stream = new PrintWriter(new FileWriter(opsDir+File.separator+grp.name+"Ops"+".scala"))
      emitOpsHeader(stream)
      if (Lifts.contains(grp)) {
        emitLifts(grp, Lifts(grp), stream)
        stream.println()
      }
      emitOpSyntax(ops, stream)        
      stream.close()
    }
    
    // emit any lifts that did not have a corresponding ops
    for ((grp,a) <- Lifts.filterNot(p => OpsGrp.contains(p._1))) {
      val stream = new PrintWriter(new FileWriter(opsDir+File.separator+"Lift"+grp.name+".scala"))
      emitOpsHeader(stream)
      emitLifts(grp, Lifts(grp), stream)
      stream.println()
      stream.close()
    }                
  }  
  
}
