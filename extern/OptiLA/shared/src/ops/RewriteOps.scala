package optila.shared.ops

import scala.virtualization.lms.common.StructOps
import scala.reflect.{Manifest,SourceContext}
import optila.shared._
import optila.shared.ops._

trait RewriteOps
trait RewriteCompilerOps extends RewriteOps {
  this: OptiLA =>
}
