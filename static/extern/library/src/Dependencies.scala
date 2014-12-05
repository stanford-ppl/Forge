package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

trait DependenciesWrapper extends HUMAN_DSL_NAMEBase {
  def forge_after[A,B](a: => Rep[A], b: => Rep[B]): Rep[A] = {
    b
    a
  }
}





