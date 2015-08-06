package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Lift
 */

trait LiftFString {
  this: OptiML => 

  implicit def FStringStringToRep(x: String) = unit(x)
}

/**
 * Operations
 */

trait FStringOps extends Base {
  this: OptiML => 

  def forge_string_plus[T:Typ,B:Typ](__arg0: Rep[T],__arg1: Rep[B])(implicit __pos: SourceContext) = fstring_forge_string_plus[T,B](__arg0,__arg1)(implicitly[Typ[T]],implicitly[Typ[B]],__pos)
  def optila_fmt_str[T:Typ](__arg0: Rep[T])(implicit __pos: SourceContext) = fstring_optila_fmt_str[T](__arg0)(implicitly[Typ[T]],__pos)

  implicit def repToFStringStringOpsCls(x: Rep[String])(implicit __pos: SourceContext) = new FStringStringOpsCls(x)(__pos)
  implicit def liftToFStringStringOpsCls(x: String)(implicit __pos: SourceContext) = new FStringStringOpsCls(unit(x))(__pos)
  implicit def varToFStringStringOpsCls(x: Var[String])(implicit __pos: SourceContext) = new FStringStringOpsCls(readVar(x))(__pos)

  class FStringStringOpsCls(val self: Rep[String])(implicit __pos: SourceContext) {
    def toInt(implicit __pos: SourceContext,__imp1: Overload3) = fstring_toint(self)(__pos)
    def toLong(implicit __pos: SourceContext,__imp1: Overload2) = fstring_tolong(self)(__pos)
    def toFloat(implicit __pos: SourceContext,__imp1: Overload3) = fstring_tofloat(self)(__pos)
    def toDouble(implicit __pos: SourceContext,__imp1: Overload3) = fstring_todouble(self)(__pos)
    def toBoolean(implicit __pos: SourceContext,__imp1: Overload2) = fstring_toboolean(self)(__pos)
    def trim(implicit __pos: SourceContext,__imp1: Overload2) = fstring_trim(self)(__pos)
    def fcharAt(__arg1: Rep[Int])(implicit __pos: SourceContext) = fstring_fcharat(self,__arg1)(__pos)
    def length(implicit __pos: SourceContext,__imp1: Overload1) = fstring_length(self)(__pos)
    def startsWith(__arg1: Rep[String])(implicit __pos: SourceContext) = fstring_startswith(self,__arg1)(__pos)
    def slice(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload2) = { fstring_substring(self,__arg1,__arg2) }
    def endsWith(__arg1: Rep[String])(implicit __pos: SourceContext) = fstring_endswith(self,__arg1)(__pos)
    def contains(__arg1: Rep[String])(implicit __pos: SourceContext,__imp1: Overload2) = fstring_contains(self,__arg1)(__pos)
    def substring(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = fstring_substring(self,__arg1)(__pos,overload1)
    def substring(__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2) = fstring_substring(self,__arg1,__arg2)(__pos,overload2)
    def toLowerCase(implicit __pos: SourceContext) = fstring_tolowercase(self)(__pos)
    def toUpperCase(implicit __pos: SourceContext) = fstring_touppercase(self)(__pos)
    def getBytes(implicit __pos: SourceContext) = fstring_getbytes(self)(__pos)
    def fsplit(__arg1: Rep[String],numSplits: Rep[Int] = unit(0))(implicit __pos: SourceContext) = { self.split(__arg1, numSplits) }
    def split(__arg1: Rep[String],numSplits: Rep[Int] = unit(0))(implicit __pos: SourceContext) = fstring_split(self,__arg1,numSplits)(__pos)
    def +[T:Typ](__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: ROverload164) = { forge_string_plus(self, __arg1) }
    def +[T:Typ](__arg1: Var[T])(implicit __pos: SourceContext,__imp1: ROverload166) = { forge_string_plus(self, readVar(__arg1)) }
    def +(__arg1: Rep[String])(implicit __pos: SourceContext,__imp1: ROverload175) = { forge_string_plus(self, __arg1) }
    def +(__arg1: Var[String])(implicit __pos: SourceContext,__imp1: ROverload176) = { forge_string_plus(self, readVar(__arg1)) }
  }

  implicit def repToFStringTOpsCls[T:Typ](x: Rep[T])(implicit __pos: SourceContext) = new FStringTOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToFStringTOpsCls[T:Typ](x: Var[T])(implicit __pos: SourceContext) = new FStringTOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class FStringTOpsCls[T:Typ](val self: Rep[T])(implicit __pos: SourceContext) {
    def +(__arg1: String)(implicit __pos: SourceContext,__imp1: ROverload169) = { forge_string_plus(self, unit(__arg1)) }
    def +(__arg1: Rep[String])(implicit __pos: SourceContext,__imp1: ROverload170) = { forge_string_plus(self, __arg1) }
  }



  def fstring_toint(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Int]
  def fstring_tolong(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Long]
  def fstring_tofloat(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Float]
  def fstring_todouble(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Double]
  def fstring_toboolean(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Boolean]
  def fstring_trim(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[String]
  def fstring_fcharat(__arg0: Rep[String],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Char]
  def fstring_length(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Int]
  def fstring_startswith(__arg0: Rep[String],__arg1: Rep[String])(implicit __pos: SourceContext): Rep[Boolean]
  def fstring_endswith(__arg0: Rep[String],__arg1: Rep[String])(implicit __pos: SourceContext): Rep[Boolean]
  def fstring_contains(__arg0: Rep[String],__arg1: Rep[String])(implicit __pos: SourceContext): Rep[Boolean]
  def fstring_substring(__arg0: Rep[String],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1): Rep[String]
  def fstring_substring(__arg0: Rep[String],__arg1: Rep[Int],__arg2: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload2): Rep[String]
  def fstring_tolowercase(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[String]
  def fstring_touppercase(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[String]
  def fstring_getbytes(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[ForgeArray[Byte]]
  def fstring_split(__arg0: Rep[String],__arg1: Rep[String],numSplits: Rep[Int] = unit(0))(implicit __pos: SourceContext): Rep[ForgeArray[String]]
  def fstring_forge_string_plus[T:Typ,B:Typ](__arg0: Rep[T],__arg1: Rep[B])(implicit __pos: SourceContext): Rep[String]
  def fstring_optila_fmt_str[T:Typ](__arg0: Rep[T])(implicit __pos: SourceContext): Rep[String]
}
trait FStringCompilerOps extends FStringOps {
  this: OptiML => 

  def optila_padspace(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[String]
}

