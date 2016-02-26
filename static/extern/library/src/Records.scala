package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import scala.collection.mutable.{ArrayBuffer, HashMap}

// Parent class for all generated structs in library (primarily for metadata)
trait LibStruct { def getFields: Map[String,Any] }

trait RecordWrapper extends HUMAN_DSL_NAMEBase with ForgeMetadataWrapper {
  this: StructOps =>

  //case class extends Record appears to be broken (scalac bug)
  class RecordImpl extends Record with LibStruct {
    val fields: HashMap[String,Any] = new HashMap()
    val fieldNames: ArrayBuffer[String] = new ArrayBuffer() //maintains declared field order

    //records have structural equality
    override def equals(other: Any): Boolean = other match {
      case that: RecordImpl => this.fields == that.fields
      case _ => false
    }
    def getFields = Map[String,Any]() ++ fields // Technically should be immutable or a copy

    override def hashCode: Int = fields.##
  }

  def field[T:Manifest](struct: Rep[Any],index: String)(implicit pos: SourceContext): Rep[T] = record_select(struct.asInstanceOf[Rep[Record]], index)

  def record_new[T:Manifest](fields: Seq[(String, Boolean, Rep[T] => Rep[_])]): Rep[T] = {
    val recordImpl = (new RecordImpl).asInstanceOf[RecordImpl]
    val recordRep = recordImpl.asInstanceOf[Rep[T]]
    for ((name, isVar, rhs) <- fields) {
      val value = rhs(recordRep)
      //println(value, value.getClass.getSimpleName)
      recordImpl.fields.asInstanceOf[HashMap[String,Any]] += Pair(name,value.asInstanceOf[Any])
      recordImpl.fieldNames.asInstanceOf[ArrayBuffer[String]] += name
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

  // Extremely similar to the way Structs are initialized, but using actual fields instead of type manifests
  // Issue if try to meet PropMap[String,Option[SymbolProperties]] since need to find implicit evidence of
  // Meetable for SymbolProperties, Option, and PropMap
  override def unapplyStructLike(e: Rep[Any]) = e match {
    case e: LibStruct => Some(e.getFields.toList)
    case _ => super.unapplyStructLike(e)
  }
}




