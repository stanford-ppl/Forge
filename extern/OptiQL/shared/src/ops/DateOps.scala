package optiql.shared.ops

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import optiql.shared._
import optiql.shared.ops._

/**
 * Operations
 */

trait DateOps extends Base with GenOverloadHack {

  trait Date

  object Date {
    def apply(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4) = date_object_apply(__arg0)(__pos,__imp1)
    def apply(__arg0: Rep[String])(implicit __pos: SourceContext,__imp1: Overload5) = date_object_apply(__arg0)(__pos,__imp1)
  }

  implicit def repToDateDateOpsCls(x: Rep[Date]) = new DateDateOpsCls(x)
  //implicit def varToDateDateOpsCls(x: Var[Date]) = new DateDateOpsCls(readVar(x))

  class DateDateOpsCls(val self: Rep[Date]) {
    def <(__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = date_lt(self,__arg0)(__pos,__imp1)
    def <=(__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = date_lteq(self,__arg0)(__pos,__imp1)
    def >(__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = date_gt(self,__arg0)(__pos,__imp1)
    def >=(__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = date_gteq(self,__arg0)(__pos,__imp1)
  }


  def date_object_apply(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4): Rep[Date]
  def date_object_apply(__arg0: Rep[String])(implicit __pos: SourceContext,__imp1: Overload5): Rep[Date]
  def date_lt(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Boolean]
  def date_lteq(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Boolean]
  def date_gt(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Boolean]
  def date_gteq(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Boolean]
}
trait DateCompilerOps extends DateOps {
  this: OptiQL =>

  def date_value(self: Rep[Date])(implicit __pos: SourceContext): Rep[Int]
}
