package dadl.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}

import dadl.shared._
import dadl.shared.ops._
import dadl.library._
import dadl.library.classes._

trait ModuleIOWrapper {
  this: DADLBase with DADLClasses =>

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
