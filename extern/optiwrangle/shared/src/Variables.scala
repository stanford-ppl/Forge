package optiwrangle.shared.extern

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
trait VarOps extends Variables
