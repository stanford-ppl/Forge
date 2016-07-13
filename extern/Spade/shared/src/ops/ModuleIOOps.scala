package spade.shared.ops

import scala.virtualization.lms.common.{Base, Record}
import scala.reflect.{Manifest,SourceContext}

import spade.shared._
import spade.shared.ops._

trait FeedbackImplicits {
  this: ModuleIOOps =>

  implicit def probe[T:Manifest](v: Feedback[T])(implicit ctx: SourceContext): Rep[T] = feedback_read(v)
}

trait ModuleIOOps extends Base with FeedbackImplicits {
  this: Spade =>

  abstract class Port extends Record

  type Feedback[+T]

  def feedback[T:Manifest](implicit ctx: SourceContext): Feedback[T] = feedback_new[T]
  def __assign[T](lhs: Feedback[T], rhs: Rep[T])(implicit o: Overload1, mT: Manifest[T], ctx: SourceContext) = feedback_assign(lhs, rhs)
  def __assign[T](lhs: Feedback[T], rhs: Feedback[T])(implicit o: Overload2, mT: Manifest[T], ctx: SourceContext) = feedback_assign(lhs, probe(rhs))

  def feedback_new[T:Manifest](implicit ctx: SourceContext): Feedback[T]
  def feedback_assign[T:Manifest](lhs: Feedback[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def feedback_read[T:Manifest](f: Feedback[T])(implicit ctx: SourceContext): Rep[T]
}
trait ModuleIOCompilerOps extends ModuleIOOps { this: Spade => }
