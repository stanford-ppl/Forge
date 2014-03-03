package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

trait ProfilingOps extends Base {
  def tic(deps: Rep[Any]*)(implicit ctx: SourceContext) = forge_profile_start(unit("app"),deps)
  def tic(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = forge_profile_start(component, deps)
  def toc(deps: Rep[Any]*)(implicit ctx: SourceContext) = forge_profile_stop(unit("app"),deps)
  def toc(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = forge_profile_stop(component, deps)
  def time(deps: Rep[Any]*)(implicit ctx: SourceContext) = forge_profile_time(deps)

  def forge_profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
  def forge_profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
  def forge_profile_time(deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Long]
}
trait ProfilingCompilerOps extends ProfilingOps
