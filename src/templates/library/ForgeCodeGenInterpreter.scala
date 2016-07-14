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

trait ForgeCodeGenInterpreter extends ForgeCodeGenBackend with LibGenPackages with BaseGenOps with BaseGenMetadata with LibGenImports with LibGenOps {
  val IR: ForgeApplicationRunner with ForgeExp
  import IR._

  lazy val targetName = "library"

  /**
   * Emit the entire library backend
   */
  def emitDSLImplementation() {
    Directory(Path(dslDir)).createDirectory()
    emitDSLDefinition()
    emitClasses()
    emitDSLMetadata()
  }

  /**
   * Emit header files which mix in all library ops and which the user can extend
   */
  def emitDSLDefinition() {
    val dslStream = new PrintWriter(new FileWriter(dslDir+dsl+".scala"))
    dslStream.println("package " + packageName)
    dslStream.println()
    emitAllImports(dslStream)
    dslStream.println()
    emitApplicationRunner(OpsGrp.values.toList, dslStream)
    dslStream.println()
    emitDSLPackageDefinitions(OpsGrp.values.toList, dslStream)
    dslStream.println()
    dslStream.close()
  }

  // Note that Wrapper must not have a view of lifts, as this causes ambiguous implicit calls
  // for the library class implementations (specifically their var fields)
  def emitWrapper(traitName: String, opsGrps: List[DSLOps], stream: PrintWriter) {
    stream.println("package " + packageName + ".classes")
    stream.println()
    emitScalaReflectImports(stream)
    emitScalaMathImports(stream)
    emitLMSImports(stream)
    emitDSLImports(stream)
    stream.println()
    stream.println("trait " + traitName + "Wrapper {")
    stream.println( "  this: " + dsl + "Base with " + dsl + "Classes => ")
    stream.println()
    for (opsGrp <- opsGrps) { emitGrpClasses(opsGrp, stream) }
    stream.println("}")
    stream.println()
  }

  // Because front-end types are not in scope (to prevent unintentional recursive calls and ambiguities),
  // we need to factor almost all tasks out to a separate trait in the library version also
  def emitWrapperImpl(traitName: String, opsGrps: List[DSLOps], stream: PrintWriter, metatype: Boolean = false) {
    stream.println("package " + packageName + ".classes")
    stream.println()
    emitScalaReflectImports(stream)
    emitScalaMathImports(stream)
    emitDSLImports(stream)
    stream.println()
    stream.println("trait " + traitName + "WrapperImpl {")
    stream.print("  this: " + dsl + "Application with " + dsl + "LibraryOps")
    // HACK: Metadata currently must have a view of all types and Rep[T] = T in order to interact with metadata classes
    if (metatype) stream.print(" with " + dsl + "Base with " + dsl + "Classes")
    stream.println(" => ")
    stream.println()
    for (opsGrp <- opsGrps) { emitImpls(opsGrp, stream) }
    stream.println("}")
    stream.println()
  }


  /**
   * Emit the library version of all methods and data structures in the DSL.
   */
  def emitClasses() {
    val clsDir = dslDir + File.separator + "classes"
    Directory(Path(clsDir)).createDirectory()

    for ((grp,opsGrp) <- OpsGrp if !isTpeClass(grp) && !isTpeClassInst(grp) && !isMetahelp(grp)) {
      val stream = new PrintWriter(new FileWriter(clsDir+File.separator+grp.name+".scala"))
      emitWrapper(grp.name, List(opsGrp), stream)
      stream.close()

      if (opsGrp.ops.exists(requiresImpl)) {
        val implStream = new PrintWriter(new FileWriter(clsDir+File.separator+grp.name+"WrapperImpl"+".scala"))
        emitWrapperImpl(grp.name, List(opsGrp), implStream, isMetaType(grp))
        implStream.close()
      }
    }

    val metahelpers = OpsGrp.toList.filter{case (grp,opsGrp) => isMetahelp(grp) }.map(_._2)
    if (!metahelpers.isEmpty) {
      val stream = new PrintWriter(new FileWriter(clsDir+File.separator+dsl+"MetadataWrapper.scala"))
      emitWrapper(dsl+"Metadata", metahelpers, stream)
      stream.close()

      val implStream = new PrintWriter(new FileWriter(clsDir+File.separator+dsl+"MetadataWrapperImpl.scala"))
      emitWrapperImpl(dsl+"Metadata", metahelpers, implStream, true)
      implStream.close()
    }

    // --- DSLClasses - One trait to group all of the ops together
    val stream = new PrintWriter(new FileWriter(clsDir+File.separator+dsl+"Classes.scala"))
    stream.println("package " + packageName + ".classes")
    stream.println()
    emitScalaReflectImports(stream)
    emitLMSImports(stream)
    emitDSLImports(stream)
    stream.println()
    stream.print("trait " + dsl + "Classes extends " + dsl + "TypeClasses")
    if (hasMetatype) stream.println(" with " + dsl + "MetaOps")

    for ((grp,opsGrp) <- OpsGrp if !isTpeClass(grp) && !isTpeClassInst(grp) && !isMetahelp(grp)) {
      stream.print(" with " + grp.name + "Wrapper")
      if (opsGrp.ops.exists(requiresImpl))
        stream.print(" with " + grp.name + "WrapperImpl")
    }
    for (e <- Externs) { stream.print(" with " + e.opsGrp.grp.name + "Wrapper") }
    if (hasMetadata)   { stream.print(" with " + dsl + "MetadataClasses") }
    if (hasMetahelp)   { stream.print(" with " + dsl + "MetadataWrapper") }
    if (hasMetahelp)   { stream.print(" with " + dsl + "MetadataWrapperImpl") }
    stream.println(" {")
    stream.println("  this: " + dsl + "Library => ")
    stream.println()
    stream.println("  def ntype[A,B](n:Numeric[A]): Numeric[B] = n.asInstanceOf[Numeric[B]]")
    stream.println("  def otype[A,B](o:Ordering[A]): Ordering[B] = o.asInstanceOf[Ordering[B]]")
    stream.println("  def frtype[A,B](o:Fractional[A]): Fractional[B] = o.asInstanceOf[Fractional[B]]")
    // Normally we would have many small versions of these methods, but that would require every Wrapper
    // trait to extend ForgeMetadataWrapper explicitly. Which version is better?
    emitTypeMetadata(stream)
    stream.println("}")
    stream.println()
    stream.close()

    // TODO: Is this warning still necessary? Could potentially have an op outside of that group that allocates
    for (d <- DataStructs.keys.toSeq diff OpsGrp.values.flatMap(_.ops).map(_.grp).filter(grpIsTpe).map(grpAsTpe).toSeq if !isMetaType(d)) {
      warn("(library) ignoring data definition for " + d.name + " since it cannot be instantiated in app code (it has no accompanying ops)")
    }
  }

  // --- DSLMetadataClasses (library backend)
  def emitDSLMetadata() {
    val MetaTpes = Tpes.filter(t => !isForgePrimitiveType(t) && DataStructs.contains(t) && isMetaType(t))
    if (MetaTpes.nonEmpty) {
      val stream = new PrintWriter(new FileWriter(dslDir+File.separator+dsl+"MetadataClasses.scala"))
      stream.println("package " + packageName)
      emitDSLImports(stream)
      emitLMSImports(stream)
      stream.println("import scala.virtualization.lms.common.MetadataOps")
      stream.println()
      emitMetadata(dsl + "Base with " + dsl + "Classes", "MetadataOps", stream, quote)
      stream.close()
    }
  }

}

