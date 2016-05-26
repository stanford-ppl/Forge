package dhdl.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._

trait LoweredPipeOps extends Base { this: DHDL => }
trait LoweredPipeCompilerOps extends LoweredPipeOps { this: DHDL => }
