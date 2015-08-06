package optila.direct.ops

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import optila.direct._
import optila.direct.ops._
import optila.direct.typeclass._

trait BLASOps {
  lazy val useBLAS = System.getProperty("optila.use.blas", "true").toBoolean
}
trait BLASCompilerOps extends BLASOps
