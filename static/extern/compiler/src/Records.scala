package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import reflect.Manifest;
import org.scala_lang.virtualized.SourceContext
import scala.virtualization.lms.common._ //also contains RecordOpsExp
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

trait RecordOpsExp extends DeliteStructsExp { //with scala.virtualization.lms.common.RecordOpsExp
  this: DeliteOpsExp =>
}
trait ScalaGenRecordOps extends ScalaGenDeliteStruct { val IR: RecordOpsExp with DeliteOpsExp }
trait CudaGenRecordOps extends CudaGenDeliteStruct { val IR: RecordOpsExp with DeliteOpsExp }
trait OpenCLGenRecordOps extends OpenCLGenDeliteStruct { val IR: RecordOpsExp with DeliteOpsExp }
trait CGenRecordOps extends CGenDeliteStruct { val IR: RecordOpsExp with DeliteOpsExp }
