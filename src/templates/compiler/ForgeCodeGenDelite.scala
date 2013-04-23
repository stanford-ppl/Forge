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
import Utilities._

trait ForgeCodeGenDelite extends ForgeCodeGenBackend with DeliteGenPackages with DeliteGenDataStructures with DeliteGenOps with DeliteGenImports {  
  val IR: ForgeApplicationRunner with ForgeExp  
  import IR._
  
  lazy val packageName = dsl.toLowerCase() + ".compiler"
  
  def isDelitePrimitiveType(t: Rep[DSLType]) = t match {
    case Def(Tpe("DeliteArray",_,_)) => true
    case _ => false
  }
  
  def makeEffectAnnotation(effect: EffectType, o: Rep[DSLOp]) = effect match {
    case `pure` => "reflectPure"
    case `mutable` => "reflectMutable"
    case `simple` => "reflectEffect"
    case write(args @ _*) => "reflectWrite(" + args.map(i => o.args.apply(i).name).mkString(",") + ")"
  }
  
  def makeFrequencyAnnotation(freq: Frequency) = freq match {
    case `normal` => "freqNormal"
    case `hot` => "freqHot"
    case `cold` => "freqCold"
  }
  
  def blockify(a: Exp[Any]): String = a match {
    case Def(Arg(name, tpe, default)) => blockify(tpe)
    case Def(FTpe(args,ret,freq)) => "Block[" + quote(ret) + "]"
    case _ => repify(a)
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
    emitDSLPackageDefinitions(OpsGrp.values.toList, dslStream)
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
    
    // 1 file per tpe, includes Ops, OpsExp, and Gen, plus an additional Impl file if the group contains SingleTask ops      
    for ((grp,opsGrp) <- OpsGrp) {
      val stream = new PrintWriter(new FileWriter(opsDir+File.separator+grp.name+"OpsExp"+".scala"))
      stream.println("package " + packageName + ".ops")
      stream.println()
      emitScalaReflectImports(stream)
      emitLMSImports(stream)
      emitDeliteOpsImports(stream)
      emitDSLImports(stream)
      stream.println()
      emitOpExp(opsGrp, stream)
      stream.println()
      if (opsGrp.ops.exists(_.opTpe.isInstanceOf[SingleTask])) {
        val implStream = new PrintWriter(new FileWriter(opsDir+File.separator+grp.name+"OpsImpl"+".scala"))
        implStream.println("package " + packageName + ".ops")        
        implStream.println()
        emitScalaReflectImports(implStream)
        implStream.println("import " + packageName + "." + dsl + "Compiler")
        implStream.println("import " + dsl.toLowerCase() + ".shared." + dsl + "Lift") // TODO: not encapsulated        
        implStream.println()
        emitSingleTaskImpls(opsGrp, implStream)
        implStream.close()
      }
      emitOpCodegen(opsGrp, stream)        
      stream.close()
    }                
  }    
}

