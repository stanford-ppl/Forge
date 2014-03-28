package optila.library.classes

import scala.reflect.{Manifest,SourceContext}
import optila.shared.ops._
import optila.library._
import optila.library.classes._

trait RewriteWrapper extends RewriteOps {
  this: OptiLABase with OptiLAClasses =>
}
