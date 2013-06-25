package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import scala.collection.mutable.HashMap

trait RecordWrapper extends HUMAN_DSL_NAMEBase {
  this: StructOps =>
  
  class RecordImpl extends Record {
    val fields: HashMap[String,Any] = new HashMap()
  }

  def field[T:Manifest](struct: Rep[Any],index: String)(implicit pos: SourceContext): Rep[T] = record_select(struct.asInstanceOf[Rep[Record]], index)

  def record_new[T:Manifest](fields: Seq[(String, Boolean, Rep[T] => Rep[_])]): Rep[T] = {
    val recordImpl = (new RecordImpl).asInstanceOf[RecordImpl]
    val recordRep = recordImpl.asInstanceOf[Rep[T]]
    for ((name, isVar, rhs) <- fields) {
      val value = rhs(recordRep)
      //println(value, value.getClass.getSimpleName)
      recordImpl.fields.asInstanceOf[HashMap[String,Any]] += Pair(name,value.asInstanceOf[Any])
    }
    recordRep
  }

  def record_select[T:Manifest](record: Rep[Record],field: String): Rep[T] = {
    record.asInstanceOf[RecordImpl].fields.get(field) match {
      case Some(f) =>
        //println("found field " + f)
        f.asInstanceOf[Rep[T]]
      case None => 
        sys.error("field " + field + " does not exist")
    }
  }
}




