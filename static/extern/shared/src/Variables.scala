package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._


/**
 * Although here we are currently simply aliasing existing LMS implementations,
 * a typical external implementation would be fully self-contained in this file.
 */

// Front-end
trait LiftVar extends LiftVariables {
  this: VarOps =>
}
trait VarOps extends Variables {
  this: ReppableOps =>

  implicit def canReppableVariable[A:Manifest]: Reppable[Var[A],A] = new Reppable[Var[A],A] {
    def view(__arg0: Var[A])(implicit __pos: SourceContext) = {
      readVar(__arg0)
    }
  }
}
trait VarCompilerOps extends VarOps {
  this: ReppableOps =>
}
