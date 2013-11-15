package optiql.library.classes

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import optiql.shared._
import optiql.shared.ops._
import optiql.library._
import optiql.library.classes._

trait DateWrapper extends DateOps {
  this: OptiQLBase with OptiQLClasses =>

  class DateImpl(__value: Int) extends Date {
    var value = __value
  }

  def date_object_apply(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4) = {
    def __imp1 = ()
    new DateImpl(__arg0)
  }
  def date_object_apply(__arg0: Rep[String])(implicit __pos: SourceContext,__imp1: Overload5) = {
    date_object_apply_impl5(__arg0)(__pos)
  }
  def date_value(self: Rep[Date])(implicit __pos: SourceContext) = {
    self.asInstanceOf[DateImpl].value
  }
  def date_lt(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = {
    date_lt_impl1(self,__arg0)(__pos)
  }
  def date_lteq(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = {
    date_lteq_impl1(self,__arg0)(__pos)
  }
  def date_gt(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = {
    date_gt_impl1(self,__arg0)(__pos)
  }
  def date_gteq(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = {
    date_gteq_impl1(self,__arg0)(__pos)
  }

  def date_object_apply_impl5(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Date] = {
    val tokens = fstring_fsplit(__arg0,unit("-"))
    val year = fstring_toint(array_apply(tokens,unit(0)))
    val month = fstring_toint(array_apply(tokens,unit(1)))
    val day = fstring_toint(array_apply(tokens,unit(2)))
    Date(primitive2_forge_int_plus(primitive2_forge_int_shift_left(year, unit(9)), primitive2_forge_int_plus(primitive2_forge_int_shift_left(month, unit(5)), day)))
  }

  def date_lt_impl1(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext): Rep[Boolean] = {
    ordering2_lt(date_value(self), date_value(__arg0))
  }

  def date_lteq_impl1(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext): Rep[Boolean] = {
    ordering2_lteq(date_value(self), date_value(__arg0))
  }

  def date_gt_impl1(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext): Rep[Boolean] = {
    ordering2_gt(date_value(self), date_value(__arg0))
  }

  def date_gteq_impl1(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext): Rep[Boolean] = {
    ordering2_gteq(date_value(self), date_value(__arg0))
  }

}

