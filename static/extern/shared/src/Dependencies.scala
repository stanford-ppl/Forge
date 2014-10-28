package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

trait DependenciesOps extends Base {
  def infix_after[A,B](a: => Rep[A], b: => Rep[B]): Rep[A] = forge_after(a,b)

  def forge_after[A,B](a: => Rep[A], b: => Rep[B]): Rep[A]
}
trait DependenciesCompilerOps extends DependenciesOps
