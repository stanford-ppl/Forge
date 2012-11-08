package ppl.dsl.forge
package templates
package compiler

import java.io.{File,PrintWriter,FileWriter}
import scala.tools.nsc.io.{Directory,Path}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import core._
import shared._

trait ForgeCodeGenDelite extends ForgeCodeGenBackend with DeliteGenPackages with DeliteGenDataStructures with DeliteGenOps with DeliteGenImports {  
  val IR: ForgeApplicationRunner with ForgeExp  
  import IR._
  
  lazy val packageName = dsl.toLowerCase() + ".compiler"
  
  def makeEffectAnnotation(effect: EffectType) = effect match {
    case `pure` => "reflectPure"
    case `mutable` => "reflectMutable"
    case write(args @ _*) => "reflectWrite(" + args.map(a => opArgPrefix + a).mkString(",") + ")"
  }
  
  def emitDSLImplementation() = {
    Directory(Path(dslDir)).createDirectory()    
    emitDSLDefinition()
    emitDataStructures()
    emitOps()
  }
  
  def emitDSLDefinition() {    
    val dslStream = new PrintWriter(new FileWriter(dslDir+dsl+".scala"))    
    dslStream.println("package " + packageName)
    dslStream.println()
    emitAllImports(dslStream)
    dslStream.println()
    emitApplicationRunner(dslStream)
    dslStream.println()    
    emitScalaPackageDefinitions(lmsAppOps, lmsCompOps, dslStream)
    dslStream.println()    
    emitDSLPackageDefinitions(OpsGrp.values.toList, lmsCompOps, dslStream)
    dslStream.println()
    emitDSLCodeGeneratorPackageDefinitions(OpsGrp.values.toList, dslStream)
    dslStream.close()    
  }
  
  def emitDataStructures() {
    val dataDir = dslDir + File.separator + "datastruct"
    Directory(Path(dataDir)).createDirectory()
    emitStructs(dataDir)    
  }
  
  def emitOps() {
    val opsDir = dslDir + File.separator + "ops"
    Directory(Path(opsDir)).createDirectory()
    
    // 1 file per tpe, includes Ops, OpsExp, and Gen      
    for ((tpe,ops) <- OpsGrp) {
      val stream = new PrintWriter(new FileWriter(opsDir+File.separator+tpe.name+"OpsExp"+".scala"))
      stream.println("package " + packageName + ".ops")
      stream.println()
      emitScalaReflectImports(stream)
      emitLMSImports(stream)
      emitDeliteOpsImports(stream)
      emitDSLImports(stream)
      stream.println()
      emitOpExp(tpe, ops, stream)
      stream.println()
      emitOpCodegen(tpe, ops, stream)        
      stream.close()
    }                
  }    
}

