package ppl.dsl.forge
package templates
package library

import java.io.{File,PrintWriter,FileWriter}
import scala.tools.nsc.io.{Directory,Path}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import core._
import shared._

trait ForgeCodeGenInterpreter extends ForgeCodeGenBackend with LibGenPackages with BaseGenOps with LibGenImports {  
  val IR: ForgeApplicationRunner with ForgeExp  
  import IR._
  
  lazy val packageName = dsl.toLowerCase() + ".library"
    
  def emitDSLImplementation() {
    Directory(Path(dslDir)).createDirectory()
    emitDSLDefinition()
    emitClasses()
  }
  
  def emitDSLDefinition() {
    val dslStream = new PrintWriter(new FileWriter(dslDir+dsl+".scala"))    
    dslStream.println("package " + packageName)
    dslStream.println()
    emitAllImports(dslStream)
    dslStream.println()
    emitApplicationRunner(dslStream)
    dslStream.println()    
    emitDSLPackageDefinitions(lmsAppOps, lmsCompOps, dslStream)
    dslStream.println()    
    dslStream.close()
  }  
    
  def emitClasses() {
    val clsDir = dslDir + File.separator + "classes"
    Directory(Path(clsDir)).createDirectory()
    
    // trait to group all of the ops together
    val grpStream = new PrintWriter(new FileWriter(clsDir+File.separator+dsl+"Classes.scala"))    
    grpStream.println("package " + packageName + ".classes")
    emitScalaReflectImports(grpStream)
    emitLMSImports(grpStream)
    emitDSLImports(grpStream)    
    grpStream.println()
    grpStream.print("trait " + dsl + "Classes extends ")
    var first = true
    for ((tpe,ops) <- OpsGrp) {
      val wrapper = tpe.name + "Wrapper"
      if (first) grpStream.print(wrapper) else grpStream.print(" with " + wrapper)
      
      val stream = new PrintWriter(new FileWriter(clsDir+File.separator+tpe.name+".scala"))            
      stream.println("package " + packageName + ".classes")
      emitScalaReflectImports(stream)
      emitLMSImports(stream)
      emitDSLImports(stream)      
      stream.println()
      stream.println("trait " + wrapper + " {")
      // TODO: call template to emit class definition for op, including methods
      stream.println("}")
      stream.println()
      stream.close()
    }                
    grpStream.println()
    grpStream.close()
  }    
}

