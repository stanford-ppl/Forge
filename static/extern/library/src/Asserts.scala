package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

trait AssertsWrapper extends HUMAN_DSL_NAMEBase {
  def forge_assert(cond: Rep[Boolean], err: Rep[String])(implicit ctx: SourceContext): Rep[Unit] = {
    // always assert in interpreter mode
    // println("assertion failed at " + quotepos(fresh[Nothing].withPos(List(ctx))) + ": ") // quotepos not in scope
    assert(cond, err)
  }
}





