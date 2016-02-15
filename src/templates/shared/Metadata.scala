package ppl.dsl.forge
package templates
package shared

import java.io.PrintWriter
import scala.virtualization.lms.common._
import core._
import Utilities._

trait BaseGenMetadata extends ForgeCodeGenBase {
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def quoteFunc(f: MetaMeet) = f match {
    case `metaUpdate` => "MetaOverwrite"
    case `branch` => "BranchAlias"
    case `mutate` => "UpdateAlias"
    case `any` => "_"
  }

  // --- Metadata
  def emitMetadataClasses(base: String, stream: PrintWriter) {
    val MetaTpes = Tpes.filter(t => !isForgePrimitiveType(t) && DataStructs.contains(t) && isMetaType(t))

    val meetFuncs = List(metaUpdate,branch,mutate,any)

    stream.println("trait " + dsl + "Metadata extends MetadataOps {")
    stream.println("  this: " + dsl + base + " =>")
    stream.println()
    emitBlockComment("DSL metadata types", stream, 2)
    for (tpe <- MetaTpes) {
      val m = tpe.asInstanceOf[Rep[DSLMetadata]]

      if (!MetaImpls.contains(m))
        err("No generic meet function defined on metadata type " + m.name + ". Define one using 'alias = any'")

      val struct = DataStructs(tpe)
      val meetRules = MetaImpls(m).meet
      val matchesRule = MetaImpls(m).matches
      val canMeetRules = MetaImpls(m).canMeet
      val completeRule = MetaImpls(m).complete

      stream.print("  case class " + m.name)
      stream.print(makeTpeParsWithBounds(struct.tpe.tpePars))
      stream.print("(")
      stream.print(struct.fields.map{case (name,tpe) => name + ": " + quote(tpe) }.mkString(", "))
      stream.println(") extends Metadata { self =>")
      stream.println("    def _meet(that: self.type)(implicit t: MeetFunc): Metadata = t match {")
      if (meetRules.contains(metaUpdate)) {
        stream.println("      case " + quoteFunc(metaUpdate) + " =>")
        quoteLiteral(meetRules(metaUpdate)).split(nl).foreach{ line => emitWithIndent(line, stream, 8) }
      }
      else stream.println("      case " + quoteFunc(metaUpdate) + " => that")

      if (!meetRules.contains(any))
        err("No generic meet function defined on metadata type " + m.name + ". Define one using 'alias = any'")

      for (func <- meetFuncs.drop(1) if meetRules.contains(func)) {
        stream.println("      case " + quoteFunc(func) + " =>")
        quoteLiteral(meetRules(func)).split(nl).foreach{ line => emitWithIndent(line, stream, 8) }
      }

      stream.println("    }")

      // Matching tests
      stream.println("    def _matches(that: self.type): Boolean = {")
      if (matchesRule.isEmpty)
        stream.println("      this == that")
      else
        quoteLiteral(matchesRule.get).split(nl).foreach{ line => emitWithIndent(line, stream, 6) }
      stream.println("    }")

      // Compatibility tests
      stream.println("    override def _canMeet(that: self.type)(implicit t: MeetFunc): Boolean = t match {")
      for (func <- meetFuncs if canMeetRules.contains(func)) {
        stream.println("      case " + quoteFunc(func) + " => ")
        quoteLiteral(canMeetRules(func)).split(nl).foreach{ line => emitWithIndent(line, stream, 8) }
      }
      if (!canMeetRules.contains(any)) {
        stream.println("      case _ => true")
      }
      stream.println("    }")
      // Incompatibility error strings
      stream.println("    def _incompatibilities(that: self.type)(implicit t: MeetFunc): List[String] = {")
      stream.println("      if (_canMeet(that)) Nil else List(\"Incompatible metadata\")")
      stream.println("    }")

      // Completeness check
      if (completeRule.isDefined) {
        stream.println("    def _isComplete: Boolean = {")
        quoteLiteral(completeRule.get).split(nl).foreach{ line => emitWithIndent(line, stream, 6) }
        stream.println("    }")
      }

      stream.println("  }")
    }
    stream.println("}")
  }


}