package optiql.library.classes

import optiql.shared.ops._
import optiql.library._
import optiql.library.classes._
import scala.reflect.{Manifest,SourceContext}
import scala.collection.mutable.HashMap

trait OptiQLRecordWrapper extends OptiQLRecordOps {
  this: OptiQLBase with OptiQLClasses => 

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

  def upgradeInt[R:Manifest](value: Rep[Int]): Rep[R] = value.toDouble.asInstanceOf[Rep[R]]

  def groupByHackImpl[K:Manifest,V:Manifest](self: Rep[Table[V]], keySelector: Rep[V] => Rep[K])(implicit pos: SourceContext): Rep[Table[Tup2[K,Table[V]]]] = {
    val map = self.data.take(self.size).groupBy(keySelector)
    
    val pairs = new scala.collection.mutable.ArrayBuffer[Tup2[K,Table[V]]]
    for ((key,v) <- map) {
      val values = v.toArray
      pairs += new Tup2(key, new Table(v.length, v))
    }

    new Table(pairs.length, pairs.toArray)
  }

  def zeroType[T:Manifest]: Rep[T] = null.asInstanceOf[Rep[T]]

}
