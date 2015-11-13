package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import reflect.Manifest;
import org.scala_lang.virtualized.SourceContext
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp

// For compiler (LMS) implementation
trait LambdaOpsExp extends FunctionsExp
trait ScalaGenLambdaOps extends ScalaGenFunctions
trait CGenLambdaOps extends CGenFunctions
trait CudaGenLambdaOps extends CudaGenFunctions
trait OpenCLGenLambdaOps extends OpenCLGenFunctions
