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
    case `metaInit` => "MetaTypeInit"
    case `metaAlias` => "MetaAlias"
    case `any` => "_"
  }

  // --- Metadata
  def emitMetadataClasses(base: String, stream: PrintWriter, typify: Rep[DSLType] => String = repify) {
    val MetaTpes = Tpes.filter(t => !isForgePrimitiveType(t) && DataStructs.contains(t) && isMetaType(t))

    val meetFuncs = List(metaUpdate,metaInit,metaAlias,any)

    stream.println("trait " + dsl + "MetadataClasses extends MetadataOps {")
    stream.println("  this: " + base + " =>")
    stream.println()
    emitBlockComment("DSL metadata types", stream, 2)
    for (tpe <- MetaTpes) {
      val m = tpe.asInstanceOf[Rep[DSLMetadata]]

      val impl = if (!MetaImpls.contains(m)) MetaOps.empty else MetaImpls(m)

      val struct = DataStructs(tpe)
      val meetRules = impl.meet
      val matchesRule = impl.matches
      val canMeetRules = impl.canMeet
      val completeRule = impl.complete

      stream.print("  case class " + m.name)
      stream.print(makeTpeParsWithBounds(struct.tpe.tpePars))
      stream.print("(")
      stream.print(struct.fields.map{case (name,tpe) => name + ": " + typify(tpe) }.mkString(", "))
      stream.println(") extends Metadata { self =>")

      stream.println("    override def _meet(that: self.type)(implicit t: MeetFunc): Metadata = t match {")
      for (func <- meetFuncs if meetRules.contains(func)) {
        stream.println("      case " + quoteFunc(func) + " =>")
        quoteLiteral(meetRules(func)).split(nl).foreach{ line => emitWithIndent(line, stream, 8) }
      }
      if (!meetRules.contains(any)) {
        stream.println("      case _ => this")
      }
      stream.println("    }")

      // Matching tests
      if (!matchesRule.isEmpty) {
        stream.println("    override def _matches(that: self.type): Boolean = {")
        quoteLiteral(matchesRule.get).split(nl).foreach{ line => emitWithIndent(line, stream, 6) }
        stream.println("    }")
      }

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
      stream.println("    override def _incompatibilities(that: self.type)(implicit t: MeetFunc): List[String] = {")
      stream.println("      if (_canMeet(that)) Nil else List(\"Incompatible metadata\")")
      stream.println("    }")

      // Completeness check
      if (completeRule.isDefined) {
        stream.println("    override def _isComplete: Boolean = {")
        quoteLiteral(completeRule.get).split(nl).foreach{ line => emitWithIndent(line, stream, 6) }
        stream.println("    }")
      }

      stream.println("  }")
    }
    stream.println("}")
  }


}
