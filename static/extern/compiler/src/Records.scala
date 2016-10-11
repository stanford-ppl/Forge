package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

import LOWERCASE_DSL_NAME.shared.FigmentStruct

trait RecordOpsExp extends DeliteStructsExp with StructExpOpt {
  this: DeliteOpsExp =>

  // Don't immediately throw an exception if a field doesn't exist for a figment type
  override def fieldLookup[T](struct: Exp[Any], index: String): Option[Exp[T]] = {
    def lookup(elems: Seq[(String, Exp[Any])]) = elems.find(_._1 == index).map(_._2.asInstanceOf[Exp[T]])
    if (isSubtype(struct.tp.erasure, classOf[FigmentStruct])) {
      struct match {
        case Def(Struct(tag, elems)) => lookup(elems)
        case Def(Reflect(Struct(tag, elems), u, es)) => lookup(elems)
        case _ => None
      }
    }
    else super.fieldLookup[T](struct, index)
  }
}
trait ScalaGenRecordOps extends ScalaGenDeliteStruct { val IR: RecordOpsExp with DeliteOpsExp }
trait CudaGenRecordOps extends CudaGenDeliteStruct { val IR: RecordOpsExp with DeliteOpsExp }
trait OpenCLGenRecordOps extends OpenCLGenDeliteStruct { val IR: RecordOpsExp with DeliteOpsExp }
trait CGenRecordOps extends CGenDeliteStruct { val IR: RecordOpsExp with DeliteOpsExp }
trait DotGenRecordOps
trait MaxJGenRecordOps
trait ChiselGenRecordOps
