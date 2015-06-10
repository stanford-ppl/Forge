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

  // TODO: Way to turn off some warnings?
  def forge_warn(cond: Rep[Boolean], msg: Rep[String], lvl: Int)(implicit ctx: SourceContext): Rep[Unit] = {
    if (!cond) System.err.println("[\u001B[33mwarn\u001B[0m] " + msg)
  }
}





