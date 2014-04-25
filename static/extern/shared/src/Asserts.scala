package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

trait AssertsOps extends Base {
  def fassert(cond: Rep[Boolean], err: Rep[String])(implicit ctx: SourceContext) = forge_assert(cond, err)
  
  def forge_assert(cond: Rep[Boolean], err: Rep[String])(implicit ctx: SourceContext): Rep[Unit]  
}
trait AssertsCompilerOps extends AssertsOps
