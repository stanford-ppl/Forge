package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DateFeatureOps extends Base {
  this: OptiML => 
/*
  object DateFeature {
    def apply(__arg0: Rep[String])(implicit __pos: SourceContext,__imp1: Overload3) = datefeature_object_apply(__arg0)(__pos)
    def year(__arg0: Rep[Double])(implicit __pos: SourceContext) = datefeature_object_year(__arg0)(__pos)
    def month(__arg0: Rep[Double])(implicit __pos: SourceContext) = datefeature_object_month(__arg0)(__pos)
    def hour(__arg0: Rep[Double])(implicit __pos: SourceContext) = datefeature_object_hour(__arg0)(__pos)
    def day(__arg0: Rep[Double])(implicit __pos: SourceContext) = datefeature_object_day(__arg0)(__pos)
    def weekday(__arg0: Rep[Double])(implicit __pos: SourceContext) = datefeature_object_weekday(__arg0)(__pos)
  }

  implicit def repToDateFeatureorgjodatimeformatDateTimeFormatterOpsCls(x: Rep[org.joda.time.format.DateTimeFormatter])(implicit __pos: SourceContext) = new DateFeatureorgjodatimeformatDateTimeFormatterOpsCls(x)(__pos)
  implicit def varToDateFeatureorgjodatimeformatDateTimeFormatterOpsCls(x: Var[org.joda.time.format.DateTimeFormatter])(implicit __pos: SourceContext) = new DateFeatureorgjodatimeformatDateTimeFormatterOpsCls(readVar(x))(__pos)

  class DateFeatureorgjodatimeformatDateTimeFormatterOpsCls(val self: Rep[org.joda.time.format.DateTimeFormatter])(implicit __pos: SourceContext) {
    def apply(__arg1: Rep[String])(implicit __pos: SourceContext,__imp1: Overload2) = datefeature_apply(self,__arg1)(__pos)
  }



  def datefeature_object_apply(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[org.joda.time.format.DateTimeFormatter]
  def datefeature_apply(__arg0: Rep[org.joda.time.format.DateTimeFormatter],__arg1: Rep[String])(implicit __pos: SourceContext): Rep[Double]
  def datefeature_object_year(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Int]
  def datefeature_object_month(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Int]
  def datefeature_object_hour(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Int]
  def datefeature_object_day(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Int]
  def datefeature_object_weekday(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Int]
*/  
}
trait DateFeatureCompilerOps extends DateFeatureOps {
  this: OptiML => 
/*
  def dt_internal(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[org.joda.time.DateTime]
  def dt_internal_year(__arg0: Rep[org.joda.time.DateTime])(implicit __pos: SourceContext): Rep[Int]
  def dt_internal_month(__arg0: Rep[org.joda.time.DateTime])(implicit __pos: SourceContext): Rep[Int]
  def dt_internal_hour(__arg0: Rep[org.joda.time.DateTime])(implicit __pos: SourceContext): Rep[Int]
  def dt_internal_day(__arg0: Rep[org.joda.time.DateTime])(implicit __pos: SourceContext): Rep[Int]
  def dt_internal_weekday(__arg0: Rep[org.joda.time.DateTime])(implicit __pos: SourceContext): Rep[Int]
*/  
}

