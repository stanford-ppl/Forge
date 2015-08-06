package optiml.direct

import scala.annotation.unchecked.uncheckedVariance
//import scala.reflect.{Manifest,SourceContext}

trait AssertsOps extends Base {
  def fassert(cond: Rep[Boolean], err: Rep[String])(implicit ctx: SourceContext) = forge_assert(cond, err)
  
  def forge_assert(cond: Rep[Boolean], err: Rep[String])(implicit ctx: SourceContext): Rep[Unit]  
}
trait AssertsCompilerOps extends AssertsOps
