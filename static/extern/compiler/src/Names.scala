package LOWERCASE_DSL_NAME.compiler

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{Traversal, FatExpressions}
import scala.virtualization.lms.common.EffectExp

import LOWERCASE_DSL_NAME.shared.NameOps

trait NameTypesExp extends NameOps

// Candidate for being pushed into Delite
trait NameOpsExp extends NameTypesExp with EffectExp with FatExpressions {

  override def getSymName(e: Rep[Any]) = meta[MName](e) match {
    case Some(mname) => Some(mname.name)
    case None =>
      val name = e.pos.headOption.flatMap{ctx => allContexts(ctx).last.assignedVariable}
      name.foreach{n => nameOf(e) = n}
      name
  }
}

// Optional. Pre-populates name metadata
trait NameAnalyzer extends Traversal {
  val IR: NameOpsExp
  import IR._

  override val name = "Name Analyzer"
  override val recurse = Always

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) { nameOf(lhs) }
}
