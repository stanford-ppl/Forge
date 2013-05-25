package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp

/**
 * Although here we are currently simply aliasing existing LMS implementations,
 * a typical external implementation would be fully self-contained in this file.
 */

// For compiler (LMS) implementation
trait VarOpsExp extends VariablesExp
trait ScalaGenVarOps extends ScalaGenVariables with DeliteScalaGenVariables { val IR: VariablesExp with DeliteOpsExp }
trait CudaGenVarOps extends CudaGenVariables
trait OpenCLGenVarOps extends OpenCLGenVariables
trait CGenVarOps extends CGenVariables
