package spade.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}

import spade.shared._
import spade.shared.ops._
import spade.library._
import spade.library.classes._

trait ModuleIOWrapper {
  this: SpadeBase with SpadeClasses =>

  type Feedback[+T] = FeedbackWire[T @uncheckedVariance]

  class FeedbackWire[T] {
    private var _driver: Option[T] = None
    def read() = _driver.getOrElse(this.asInstanceOf[Rep[T]]) // This will probably cause problems. Oh well!
    def link(x: T) = { _driver = Some(x) }
  }

  def feedback_new[T:Manifest](implicit ctx: SourceContext): Feedback[T] = new FeedbackWire[T]
  def feedback_assign[T:Manifest](lhs: Feedback[T], rhs: Rep[T])(implicit ctx: SourceContext): Rep[Unit] = lhs.link(rhs)
  def feedback_read[T:Manifest](f: Feedback[T])(implicit ctx: SourceContext): Rep[T] = f.read()
}
