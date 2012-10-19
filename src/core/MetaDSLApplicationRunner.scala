package ppl.dsl.meta
package core

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import templates.Imports._

trait MetaDSLApplicationRunner extends MetaDSLApplication with MetaDSLExp {
  private val dsl = dslName.filterNot(_ == ' ').capitalize
  private val dslDir = "build" + File.separator + dsl.toLowerCase() + File.separator + "src" + File.separator + dsl.toLowerCase() + File.separator
  
  def emitDSLDefinition(codegen: MetaDSLCodeGenScala{val IR: MetaDSLApplicationRunner.this.type}) {    
    Directory(Path(dslDir)).createDirectory()
    val dslStream = new PrintWriter(new FileWriter(dslDir+dsl+".scala"))    
    dslStream.println("package " + dsl.toLowerCase)
    dslStream.println()
    emitAllImports(dsl, dslStream)
    dslStream.println()
    codegen.emitApplicationRunner(dsl, dslStream)
    dslStream.println()    
    codegen.emitScalaPackageDefinitions(dsl, lmsAppOps, lmsCompOps, dslStream)
    dslStream.println()    
    codegen.emitDSLPackageDefinitions(dsl, OpsGrp.values.toList, lmsCompOps, dslStream)
    dslStream.println()
    codegen.emitDSLCodeGeneratorPackageDefinitions(dsl, OpsGrp.values.toList, dslStream)
    dslStream.close()    
  }
  
  def emitTypes(codegen: MetaDSLCodeGenScala{val IR: MetaDSLApplicationRunner.this.type}) {
    val typeStream = new PrintWriter(new FileWriter(dslDir+"Types.scala"))
    typeStream.println("package " + dsl.toLowerCase)    
    typeStream.println()
    emitDeliteCollectionImport(typeStream)
    typeStream.println()    
    for ((tpe,ops) <- OpsGrp) {
      typeStream.print("abstract class " + codegen.quote(tpe))
      if (DeliteCollections.contains(tpe)) typeStream.println(" extends DeliteCollection[" + DeliteCollections(tpe).tpeArg.name + "]") else typeStream.println()
    }
    typeStream.close()
  }  
  
  def emitDataStructures(codegen: MetaDSLCodeGenScala{val IR: MetaDSLApplicationRunner.this.type}) {
    val dataDir = dslDir + File.separator + "datastruct"
    Directory(Path(dataDir)).createDirectory()
    codegen.emitStructs(dsl, dataDir)    
  }
  
  def emitOps(codegen: MetaDSLCodeGenScala{val IR: MetaDSLApplicationRunner.this.type}) {
    val opsDir = dslDir + File.separator + "ops"
    Directory(Path(opsDir)).createDirectory()
    codegen.emitOps(dsl, opsDir)    
  }
  
  final def main(args: Array[String]) {
    info("DSL Being Staged:[" + this.getClass.getName + "] (" + dsl + ")")

    // -- stage the specification to build the MetaDSL IR
    // this has the side effect of populating all of the internal MetaDSL collections
    val y = specification()
    
    // -- run sanity checkers
    // TODO: implement these!
    //  1) all ops that are declared as delite ops have delite collection input/outputs
    // check(y)
    
    // -- run code generators
    val codegen = new MetaDSLCodeGenScala{val IR: MetaDSLApplicationRunner.this.type = MetaDSLApplicationRunner.this}    
    Directory(Path("build")).deleteRecursively() // TODO: make configurable
    
    // different components use different streams
    emitDSLDefinition(codegen)
    emitTypes(codegen)
    emitDataStructures(codegen)
    emitOps(codegen)
    
    info("DSL generation complete. Please run ... to compile the generated files against Delite and check for errors.")    
  }  
}
