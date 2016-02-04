package ppl.dsl.forge
package templates
package shared

import java.io.PrintWriter
import scala.virtualization.lms.common._
import core._
import Utilities._

trait BaseGenPackages extends ForgeCodeGenBase {
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  // DSLApplication extends DSL with DSLLift
  def emitApplicationRunnerBase(stream: PrintWriter) {
    emitComment("the trait that all " + dsl + " applications must extend", stream)
    stream.println("trait " + dsl + "Application extends " + dsl + " with " + dsl + "Lift {")
    stream.println("  var args: Rep[Array[String]]")
    stream.println("  var stagingArgs: Array[String]")
    stream.println("  def main()")
    stream.println("}")
  }

  def emitDSLPackageDefinitionsBase(opsGrps: List[DSLOps], stream: PrintWriter) {
    emitBlockComment("DSL definition", stream)

    // --- DSLLift
    stream.println("trait " + dsl + "Lift")
    val liftOps = Lifts.keys.toList
    if (liftOps.length > 0) {
      stream.print("  extends Lift" + liftOps.head.name)
      for (grp <- liftOps.drop(1)) stream.print(" with Lift" + grp.name)
      for (e <- Externs if e.withLift) stream.print(" with Lift" + e.opsGrp.grp.name)
    }
    else {
      val liftExterns = Externs.filter(_.withLift)
      stream.print("  extends Lift" + liftExterns.head.opsGrp.grp.name)
      for (e <- liftExterns.drop(1)) stream.print(" with Lift" + e.opsGrp.grp.name)
    }
    stream.println(" { \n    this: " + dsl + " => \n}")
    stream.println()

    // --- DSLInterface
    val NonStructTpes = Tpes.filter(t => !isForgePrimitiveType(t) && !DataStructs.contains(t) && !isMetaType(t))

    stream.println("trait " + dsl + "Identifiers extends Base with GenOverloadHack {")
    if (Identifiers.length > 0) {
      emitBlockComment("Singleton identifiers", stream, indent=2)
      for (id <- Identifiers) {
        stream.println("  object " + id.name + " extends " + quote(id.tpe))
      }
    }
    if (NonStructTpes.length > 0) {
      stream.println()
      emitBlockComment("types with no associated data structure", stream, indent=2)
      for (tpe <- NonStructTpes) {
        stream.println("  abstract class " + quote(tpe) + TpeParents.get(tpe).map(p => " extends " + quote(p)).getOrElse(""))
      }
      for (tpe <- NonStructTpes) {
        stream.println("  implicit def m_" + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + " = manifest[" + quote(tpe) + "]") // needed?
      }
    }
    stream.println("}")
    stream.println()

    // --- DSL extends DSLIdentifiers
    // abstract types are not included in the identifiers trait above because they can clash with either the class definitions or Scala
    // types in the lib implementation, causing a nasty scalac typer crash. (occurs, for example, if we declare an abstract Vector type)
    val StructTpes = Tpes.filter(t => !isForgePrimitiveType(t) && DataStructs.contains(t) && !isMetaType(t))

    stream.println("trait " + dsl + " extends " + dsl + "Identifiers")
    for (opsGrp <- opsGrps) {
      stream.print(" with " + opsGrp.name)
    }
    for (e <- Externs) {
      stream.print(" with " + e.opsGrp.name)
    }
    stream.println(" {")
    stream.println("  this: " + dsl + "Application => ")
    stream.println()

    if (StructTpes.length > 0) {
      emitBlockComment("Abstract types", stream, indent=2)
      for (tpe <- StructTpes) stream.println("  type " + quote(tpe))
      for (tpe <- StructTpes) stream.println("  implicit def m_" + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + ": Manifest[" + quote(tpe) + "]")
    }

    if (TpeAliases.length > 0) {
      stream.println()
      emitBlockComment("Type aliases", stream, indent=2)
      for (alias <- TpeAliases) {
        stream.println("  type " + alias.name + makeTpePars(alias.tpe.tpePars) + " = " + quote(alias.tpe))
      }
    }

    if (TpeParents.size > 0) {
      stream.println()
      emitBlockComment("Figment type inheritance", stream, indent=2)
      for ((tpe,parent) <- TpeParents) {
        stream.println("  implicit def m_" + tpe.name + "_to_" + parent.name + makeTpeParsWithBounds(tpe.tpePars) + "(__arg0: Rep[" + tpe.name + makeTpePars(tpe.tpePars) + "]): Rep[" + parent.name + makeTpePars(parent.tpePars) + "]")
      }
    }

    stream.println()
    emitBlockComment("hacks for Scala-Virtualized", stream, indent=2)
      // HACK -- bug in scala-virtualized (copied from LMS IfThenElse.scala)
    stream.println("  override def __ifThenElse[T](cond: => Boolean, thenp: => T, elsep: => T) = cond match {")
    stream.println("    case true => thenp")
    stream.println("    case false => elsep")
    stream.println("  }")
    stream.println()
    stream.println("}")
  }

}
