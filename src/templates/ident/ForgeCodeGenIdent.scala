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

trait ForgeCodeGenIdent extends ForgeCodeGenBackend with LibGenPackages with BaseGenOps with LibGenImports with LibGenOps {  
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
/*  emitAllImports(dslStream)
    dslStream.println()
    emitApplicationRunner(OpsGrp.values.toList, dslStream)
    dslStream.println()    
    emitDSLPackageDefinitions(OpsGrp.values.toList, dslStream)
    dslStream.println()    
*/
  }  
    
  def emitClasses(grpStream: PrintWriter) {
    //grpStream.println("package " + packageName + ".classes")
    //grpStream.println()
    //emitScalaReflectImports(grpStream)
    //emitLMSImports(grpStream)
    //emitDSLImports(grpStream)    
    //grpStream.println()
    //grpStream.print("trait " + dsl + "Classes extends ")
    var first = true
    for ((grp,opsGrp) <- OpsGrp) {

      grpStream.println("// "+grp.name + " / " + opsGrp.name)

      if (isTpeClass(grp)) {
        //if (first) grpStream.print(opsGrp.name) else grpStream.print(" with " + opsGrp.name)
        first = false
      }
      else if (!isTpeClass(grp) && !isTpeClassInst(grp)) {        
        val wrapper = grp.name + "Wrapper"
        //if (first) grpStream.print(wrapper) else grpStream.print(" with " + wrapper)
        first = false
      
        val stream = grpStream
        //stream.println("package " + packageName + ".classes")
        //stream.println()
        //emitScalaReflectImports(stream)
        //emitScalaMathImports(stream)      
        //emitLMSImports(stream)
        //emitDSLImports(stream)      
        //stream.println()
        //stream.println("trait " + wrapper + " {")
        // stream.println("trait " + wrapper + " extends " + grp.name + "Ops with " + dsl + "Base {")
        //stream.println( "  this: " + dsl + "Base with " + dsl + "Classes => ")
        //stream.println()

        stream.println("def spec" + grp.name + "() = {")

        emitClass(opsGrp, stream)

        stream.println("}")
        stream.println("spec" + grp.name + "()")

        stream.println()
        stream.println()
        
        //stream.println("}")
        //stream.println()
        //stream.close()
      
        // because front-end types are not in scope (to prevent unintentional recursive calls and ambiguities),
        // we need to factor almost all tasks out to a separate trait in the library version also
        if (opsGrp.ops.exists(requiresImpl)) {
          val implStream = grpStream
          //implStream.println("package " + packageName + ".classes")       
          //implStream.println()
          //emitScalaReflectImports(implStream) 
          //emitScalaMathImports(implStream)
          //emitDSLImports(implStream)
          //implStream.println()
          emitImpls(opsGrp, implStream)
          //implStream.close()      
        
          //grpStream.print(" with " + grp.name + "WrapperImpl")
        }
      }
    }


    for (d <- DataStructs.keys.toSeq diff OpsGrp.keys.filter(grpIsTpe).map(grpAsTpe).toSeq) {
      warn("(ident) ignoring data definition for " + d.name + " since it cannot be instantiated in app code (it has no accompanying ops)")
    }

    /*for (e <- Externs) {
      grpStream.print(" with " + e.opsGrp.grp.name + "Wrapper")
    }    
    grpStream.println("{")
    grpStream.println("  this: " + dsl + "Lib with " + dsl + "Application => ")
    grpStream.println("}")
    grpStream.println()*/
    //grpStream.close()
  }    
}

