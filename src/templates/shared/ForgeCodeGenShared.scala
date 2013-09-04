package ppl.dsl.forge
package templates
package shared

import java.io.{File,PrintWriter,FileWriter}
import scala.tools.nsc.io.{Directory,Path}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import core._

trait ForgeCodeGenShared extends ForgeCodeGenBackend with BaseGenPackages with BaseGenLifts with BaseGenOps with BaseGenImports with BaseGenTypeClasses {
  val IR: ForgeApplicationRunner with ForgeExp
  import IR._

  lazy val dslStream = new PrintWriter(new FileWriter(dslDir+"Frontend.scala"))
  lazy val targetName = "shared"

  def emitDSLImplementation() {
    Directory(Path(dslDir)).createDirectory()
    emitDSLDefinition()
    emitOps()
    emitTypeClasses()
    emitOverloadHack()
  }

  def emitDSLDefinition() {
    val genOps = OpsGrp.filterNot(t => isTpeClassInst(t._1)).values.toList
    dslStream.println("package " + packageName)
    dslStream.println()
    emitAllImports(dslStream)
    dslStream.println()
    emitApplicationRunnerBase(dslStream)
    dslStream.println()
    emitDSLPackageDefinitionsBase(genOps, dslStream)
    dslStream.println()
    dslStream.close()
  }

  def emitHeader(pkgName: String, stream: PrintWriter) {
    stream.println("package " + pkgName)
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
    for ((grp,ops) <- OpsGrp if !isTpeClass(grp) && !isTpeClassInst(grp)) {
      checkOps(ops)

      val stream = new PrintWriter(new FileWriter(opsDir+File.separator+grp.name+"Ops"+".scala"))
      emitHeader(packageName + ".ops", stream)
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
      emitHeader(packageName + ".ops", stream)
      emitLifts(grp, Lifts(grp), stream)
      stream.println()
      stream.close()
    }
  }

  def emitTypeClasses() {
    val typeClassDir = dslDir + File.separator + "typeclass"
    Directory(Path(typeClassDir)).createDirectory()

    for ((grp,ops) <- OpsGrp if isTpeClass(grp)) {
      val stream = new PrintWriter(new FileWriter(typeClassDir+File.separator+grp.name+"Ops"+".scala"))
      emitHeader(packageName + ".typeclass", stream)
      emitTpeClass(grp, ops, stream)
      stream.close()
    }
  }

  def emitOverloadHack() {
    val stream = new PrintWriter(new FileWriter(dslDir+"GenOverloadHack"+".scala"))

    var numOverloaded = 0
    var numRedirectOverloaded = 0
    for ((grp,opsGrp) <- OpsGrp) {
      for (o <- opsGrp.ops) {
        val clashes = nameClashesUniversal(o)
        if (isRedirect(o)) {
          if (clashes.length > numRedirectOverloaded) numRedirectOverloaded = clashes.length
        }
        else {
          if (clashes.length > numOverloaded) numOverloaded = clashes.length
        }
      }
    }

    stream.println("package " + packageName)
    stream.println()
    stream.println("trait GenOverloadHack {")
    // slightly different name to avoid conflicts with LMS
    for (i <- 0 to numOverloaded) {
      stream.println("  class Overload"+i)
    }
    for (i <- 0 to numRedirectOverloaded) {
      stream.println("  class ROverload"+i)
    }
    stream.println()
    // seperated for generated code readability
    for (i <- 0 to numOverloaded) {
      stream.println("  implicit val overload" + i + " = new Overload" + i)
    }
    for (i <- 0 to numRedirectOverloaded) {
      stream.println("  implicit val roverload" + i + " = new ROverload" + i)
    }
    stream.println("}")
    stream.close()
  }

}
