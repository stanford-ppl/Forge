package optiml.direct.ops

import scala.tools.nsc.io._
//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

trait BLASOps {
  lazy val useBLAS = System.getProperty("optila.use.blas", "true").toBoolean
}
trait BLASCompilerOps extends BLASOps
