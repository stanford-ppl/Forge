package ppl.dsl.forge
package templates
package compiler

import java.io.PrintWriter
import scala.virtualization.lms.common._
import core._
import shared.BaseGenPackages
import Utilities._

trait DeliteGenPackages extends BaseGenPackages {  
  this: ForgeCodeGenDelite =>
  
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._
 
  def emitApplicationRunner(stream: PrintWriter) {
    stream.println("trait " + dsl + "ApplicationCompiler extends " + dsl + "Application with DeliteApplication with " + dsl+"Exp")
  }

  def emitScalaPackageDefinitions(appOps: List[LMSOps], compOps: List[LMSOps], stream: PrintWriter) {
    val allOps = appOps union compOps  
    emitBlockComment("compiler packages", stream)
    
    val opsPkg = dsl + "ScalaOpsPkg"    
    val opsPkgExp = opsPkg + "Exp"
    stream.println("trait " + opsPkgExp + " extends " + opsPkg + " with BaseExp")
    for (op <- allOps) {
      stream.print(" with " + op.opt)
    }
    stream.println()
    stream.println()
  
    // Code generators for Scala ops
    for (g <- generators) {
      val selectedOps = allOps.filter(_.targets.contains(g))
      stream.println("trait " + dsl + g.name + "CodeGenPkg extends " + g.name + "GenBase")
      for (op <- selectedOps) {
        stream.print(" with " + g.name + "Gen" + op.name)
      }
      stream.println()
      stream.println("  { val IR: " + opsPkgExp + " }")  
      stream.println()
    }
  }  

  def emitDSLPackageDefinitions(dslOps: List[Ops], lmsCompOps: List[LMSOps], stream: PrintWriter) {
    emitBlockComment("dsl compiler definition", stream)
  
    // compiler
    stream.println("trait " + dsl + "Compiler extends " + dsl)
    for (op <- lmsCompOps) {
      stream.print(" with " + op.name)
    }
    stream.println(" { this: " + dsl + "Application with " + dsl + "Exp => }")
    stream.println()
  
    // exp
    stream.println("trait " + dsl + "Exp extends " + dsl + "Compiler with " + dsl + "ScalaOpsPkgExp")
    for (op <- dslOps) {
      stream.print(" with " + op.exp)
    }
    stream.println(" with DeliteOpsExp with DeliteAllOverridesExp {")
    stream.println(" this: DeliteApplication with " + dsl + "Application => ")
    stream.println()
    for ((tpe,ops) <- OpsGrp) {
      stream.print("  abstract class " + quote(tpe))
      if (DeliteCollections.contains(tpe)) stream.println(" extends DeliteCollection[" + DeliteCollections(tpe).tpeArg.name + "]") else stream.println()
      stream.println("  def m_" + tpe.name + makeTpeArgsWithBounds(tpe.tpeArgs) + " = manifest[" + quote(tpe) + "]")      
    }
    stream.println()
    stream.println("  def getCodeGenPkg(t: Target{val IR: " + dsl + "Exp.this.type}): GenericFatCodegen{val IR: " + dsl + "Exp.this.type} = {")
    stream.println("    t match {")  
    for (g <- generators) {
      stream.println("      case _:Target" + g.name + " => new " + dsl + "Codegen" + g.name + "{val IR: " + dsl + "Exp.this.type = " + dsl + "Exp.this}")      
    }
    stream.println("      case _ => throw new RuntimeException(\"" + dsl + " does not support this target\")")
    stream.println("    }")
    stream.println("  }")
    stream.println("}")
  }

  private def emitBaseCodegen(stream: PrintWriter) {
    stream.println("trait " + dsl + "CodegenBase extends GenericFatCodegen {")
    stream.println("  val IR: DeliteApplication with " + dsl + "Exp")
    stream.println("  override def initialDefs = IR.deliteGenerator.availableDefs")
    stream.println()
    stream.println("  def dsmap(line: String) = line")
    stream.println("  override def emitDataStructures(path: String) {")
    stream.println("    val s = File.separator")
    stream.println("    val dsRoot = Config.homeDir + s+\"dsls\"+s+\""+dsl.toLowerCase()+"\"+s+\"compiler\"+s+\"src\"+s+\""+dsl.toLowerCase()+"\"+s+\"datastruct\"+s+this.toString")
    stream.println()
    stream.println("    val dsDir = Directory(Path(dsRoot))")
    stream.println("    val outDir = Directory(Path(path))")
    stream.println("    outDir.createDirectory()")
    stream.println()
    stream.println("    for(f <- dsDir.files) {")
    stream.println("      val outFile = path + s + f.name")
    stream.println("      val out = new BufferedWriter(new FileWriter(outFile))")
    stream.println("      for (line <- scala.io.Source.fromFile(f.jfile).getLines) {")
    stream.println("        out.write(dsmap(line) + \"\\n\")")
    stream.println("      }")
    stream.println("      out.close()")
    stream.println("    }")
    stream.println("  }")
    stream.println()
    stream.println("  override def remap[A](m: Manifest[A]): String = {")
    stream.println("    var res = super.remap(m)")
    stream.println("    if (res.contains(\"$\")) {")
    stream.println("      res = res.slice(res.indexOf(\"#\")+1,res.length)")
    stream.println("      res = res.slice(0, res.lastIndexOf(\".\")+1) + res.slice(res.lastIndexOf(\"$\")+1, res.length)")
    stream.println("    }")
    stream.println("    res")
    stream.println("  }")
    stream.println("}")
  }

  def emitDSLCodeGeneratorPackageDefinitions(dslOps: List[Ops], stream: PrintWriter) {
    emitBlockComment("code generators", stream)    
    emitBaseCodegen(stream)
    stream.println()
    for (g <- generators) {
      stream.println("trait " + dsl + "Codegen" + g.name + " extends " + dsl + "CodegenBase with " + dsl + g.name + "CodeGenPkg")
      for (op <- dslOps) {
        if (op.targets.contains(g))
          stream.print(" with " + g.name + "Gen" + op.name)
      }
      stream.println(" with " + g.name + "GenDeliteOps with Delite" + g.name + "GenAllOverrides {" )
      stream.println("  val IR: DeliteApplication with " + dsl + "Exp")
      // TODO: generalize, other targets can have remaps too, need to be encoded somehow
      // store dsmap and remap inside the generator itself?
      if (g == $cala) {
        stream.println()
        emitComment("these methods translate types in the compiler to typed in the generated code", stream, indent=1)
        stream.println("  override def dsmap(line: String): String = {")
        stream.println("    var res = line.replaceAll(\"" + packageName + ".datastruct\", \"generated\")")
        stream.println("    res = res.replaceAll(\"ppl.delite.framework.datastruct\", \"generated\")")
        stream.println("    res = res.replaceAll(\"" + packageName + "\", \"generated.scala\")")
        stream.println("    res")
        stream.println("  }")
        stream.println()
        stream.println("  override def remap[A](m: Manifest[A]): String = {")
        stream.println("    var res = super.remap(m)")
        stream.println("    res = res.replaceAllLiterally(\"package$\", \"\")")
        stream.println("    dsmap(res)")
        stream.println("  }")
        stream.println()
      }
      stream.println("}")
    }
  }  
  
}