package optiql.shared.ops

import scala.virtualization.lms.common.{Base,StructOps}
import optiql.shared._
import optiql.shared.ops._
import scala.reflect.{RefinedManifest,SourceContext}

//TODO: this trait is basically a misc. grab bag of features, but most of it should be pushed directly into Forge
trait RewriteOps extends Base {
  this: OptiQL => 

  def infix_printAsTable[A:Manifest](self: Rep[Table[A]],maxRows: Rep[Int] = unit(100))(implicit __pos: SourceContext) = table_printastable[A](self,maxRows)(implicitly[Manifest[A]],__pos)
  def infix_writeAsJSON[A:Manifest](self: Rep[Table[A]],path: Rep[String])(implicit __pos: SourceContext) = table_writeasjson[A](self,path)(implicitly[Manifest[A]],__pos)
  //def infix_writeAsCSV

  def table_printastable[A:Manifest](self: Rep[Table[A]],maxRows: Rep[Int] = unit(100))(implicit __pos: SourceContext): Rep[Unit]
  def table_writeasjson[A:Manifest](self: Rep[Table[A]],path: Rep[String])(implicit __pos: SourceContext): Rep[Unit]
}

trait RewriteCompilerOps extends RewriteOps {
  this: OptiQL =>

  def groupByHackImpl[K:Manifest,V:Manifest](self: Rep[Table[V]], keySelector: Rep[V] => Rep[K])(implicit pos: SourceContext): Rep[Table[Tup2[K,Table[V]]]]

  def sortHackImpl[A:Manifest](self: Rep[Table[A]], comparator: (Rep[A],Rep[A]) => Rep[Int])(implicit pos: SourceContext): Rep[Table[A]]

  def compareHackImpl[A:Manifest:Ordering](lhs: Rep[A], rhs: Rep[A]): Rep[Int]

  def zeroType[T:Manifest]: Rep[T] = (manifest[T] match { //need a more robust solution, e.g. type class
    //case StructType(tag,elems) => struct[T](tag, elems.map(e => (e._1, zeroType(e._2))))
    case v if v == manifest[Int] => unit(0)
    case v if v == manifest[Long] => unit(0L)
    case v if v == manifest[Double] => unit(0.0)
    case v if v == manifest[Float] => unit(0.0f)
    case _ => cast_asinstanceof[Null,T](unit(null))
  }).asInstanceOf[Rep[T]]

  def minValue[T:Manifest]: Rep[T] = (manifest[T] match {
    case v if v == manifest[Int] => unit(scala.Int.MinValue)
    case v if v == manifest[Long] => unit(scala.Long.MinValue)
    case v if v == manifest[Double] => unit(scala.Double.MinValue)
    case v if v == manifest[Float] => unit(scala.Float.MinValue)
    case v if v == manifest[Char] => unit(scala.Char.MinValue)
    case _ => cast_asinstanceof[Null,T](unit(null)) //shouldn't be used for reference types
  }).asInstanceOf[Rep[T]]

  def maxValue[T:Manifest]: Rep[T] = (manifest[T] match {
    case v if v == manifest[Int] => unit(scala.Int.MaxValue)
    case v if v == manifest[Long] => unit(scala.Long.MaxValue)
    case v if v == manifest[Double] => unit(scala.Double.MaxValue)
    case v if v == manifest[Float] => unit(scala.Float.MaxValue)
    case v if v == manifest[Char] => unit(scala.Char.MaxValue)
    case _ => cast_asinstanceof[Null,T](unit(null)) //shouldn't be used for reference types
  }).asInstanceOf[Rep[T]]

  def upgradeInt[T:Manifest](value: Rep[Int]): Rep[T] = (manifest[T] match {
    case v if v == manifest[Int] => value
    case v if v == manifest[Long] => value.toLong
    case v if v == manifest[Double] => value.toDouble
    case v if v == manifest[Float] => value.toFloat
    case _ => throw new RuntimeException("ERROR: don't know how to average type " + manifest[T].toString)
  }).asInstanceOf[Rep[T]]

  def createRecord[T:Manifest](record: Rep[ForgeArray[String]]) = {
    val rm = manifest[T] match {
      case rm: RefinedManifest[T] => rm
      case m => throw new RuntimeException("Don't know hot to automatically parse type " + m.toString + ". Try passing in your own parsing function instead.")
    }
    val elems = rm.fields

    val fields = Range(0,elems.length) map { i =>
      val (field, tp) = elems(i)
      tp.toString match {
        case s if s.contains("String") => (field, false, (r:Rep[T]) => record(unit(i)))
        case "Double" => (field, false, (r:Rep[T]) => record(unit(i)).toDouble)
        case "Float" => (field, false, (r:Rep[T]) => record(unit(i)).toFloat)
        case "Boolean" => (field, false, (r:Rep[T]) => record(unit(i)) == "true")
        case "Int" => (field, false, (r:Rep[T]) => record(unit(i)).toInt)
        case "Long" => (field, false, (r:Rep[T]) => record(unit(i)).toLong)
        case "Char" => (field, false, (r:Rep[T]) => infix_fcharAt(record(unit(i)), unit(0)))
        case d if d.contains("Date") => (field, false, (r:Rep[T]) => Date(record(unit(i))))
        case _ => throw new RuntimeException("Unsupported record field type: " + tp.toString)
      }
    }
    
    record_new[T](fields)
  }
}
