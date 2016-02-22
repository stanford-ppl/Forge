package dadl.compiler.ops

import scala.virtualization.lms.common.BaseExp
import scala.reflect.{Manifest,SourceContext}

import dadl.shared._
import dadl.shared.ops._
import dadl.compiler._
import dadl.compiler.ops._

trait ModuleIOOpsExp extends ModuleIOOps with BaseExp {
  this: DADLExp =>

  type Feedback[+T] = FeedbackWire[T]

  case class FeedbackWire[+T](val e: Exp[Feedback[T]])

  case class FreshFeedback[T](mT: Manifest[T]) extends Def[Feedback[T]]
  case class Link[T:Manifest](lhs: Feedback[T], rhs: Exp[T]) extends Def[Unit]
  case class ReadFeedback[T:Manifest](f: Feedback[T]) extends Def[T]

  def feedback_new[T:Manifest](implicit ctx: SourceContext): Feedback[T] = {
    FeedbackWire(reflectMutable(FreshFeedback(manifest[T])))
  }
  def feedback_assign[T:Manifest](lhs: Feedback[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[Unit] = {
    reflectWrite(lhs.e)(Link(lhs, rhs))
  }
  def feedback_read[T:Manifest](f: Feedback[T])(implicit ctx: SourceContext): Rep[T] = ReadFeedback(f)
}

