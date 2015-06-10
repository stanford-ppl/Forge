package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

trait AssertsOps extends Base {
  def fassert(cond: Rep[Boolean], err: Rep[String])(implicit ctx: SourceContext) = forge_assert(cond, err)
  
  def forge_assert(cond: Rep[Boolean], err: Rep[String])(implicit ctx: SourceContext): Rep[Unit]  

  def fwarn(cond: Rep[Boolean], msg: Rep[String], lvl: Int = 1)(implicit ctx: SourceContext) = forge_warn(cond, err, lvl)
  def fwarn(msg: Rep[String], lvl: Int = 1)(implicit ctx: SourceContext) = forge_warn(unit(false), err, lvl)

  def forge_warn(cond: Rep[Boolean], msg: Rep[String], lvl: Int)(implicit ctx: SourceContext): Rep[Unit]

}
trait AssertsCompilerOps extends AssertsOps
