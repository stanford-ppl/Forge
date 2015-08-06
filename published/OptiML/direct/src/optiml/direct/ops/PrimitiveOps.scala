package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Lift
 */

trait LiftPrimitive {
  this: OptiML => 

  implicit def PrimitiveBooleanToRep(x: Boolean) = unit(x)
  implicit def PrimitiveShortToRep(x: Short) = unit(x)
  implicit def PrimitiveIntToRep(x: Int) = unit(x)
  implicit def PrimitiveLongToRep(x: Long) = unit(x)
  implicit def PrimitiveFloatToRep(x: Float) = unit(x)
  implicit def PrimitiveDoubleToRep(x: Double) = unit(x)
}

/**
 * Operations
 */

trait PrimitiveOpsBase extends Base {
  this: OptiML => 

  implicit def repInt2ToRepDouble(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[Double] = primitive_repint2torepdouble(__arg0)(__pos)
  implicit def repInt2ToRepFloat(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[Float] = primitive_repint2torepfloat(__arg0)(__pos)
  implicit def repInt2ToRepLong(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[Long] = primitive_repint2toreplong(__arg0)(__pos)
  implicit def repFloat2ToRepDouble(__arg0: Rep[Float])(implicit __pos: SourceContext): Rep[Double] = primitive_repfloat2torepdouble(__arg0)(__pos)
  implicit def dist(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Double] = primitive_dist(__arg0,__arg1)(__pos,overload1)
  implicit def dist(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload2): Rep[Double] = primitive_dist(__arg0,__arg1)(__pos,overload2)

  def primitive_repint2torepdouble(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[Double]
  def primitive_repint2torepfloat(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[Float]
  def primitive_repint2toreplong(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[Long]
  def primitive_repfloat2torepdouble(__arg0: Rep[Float])(implicit __pos: SourceContext): Rep[Double]
  def primitive_dist(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Double]
  def primitive_dist(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload2): Rep[Double]
}

trait PrimitiveOps extends PrimitiveOpsBase {
  this: OptiML => 

  def forge_int_plus(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_int_plus(__arg0,__arg1)(__pos)
  def forge_int_minus(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_int_minus(__arg0,__arg1)(__pos)
  def forge_int_times(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_int_times(__arg0,__arg1)(__pos)
  def forge_int_divide(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_int_divide(__arg0,__arg1)(__pos)
  def forge_int_shift_left(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_int_shift_left(__arg0,__arg1)(__pos)
  def forge_int_shift_right_unsigned(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_int_shift_right_unsigned(__arg0,__arg1)(__pos)
  def forge_int_and(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_int_and(__arg0,__arg1)(__pos)
  def forge_int_or(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_int_or(__arg0,__arg1)(__pos)
  def forge_int_shift_right(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_int_shift_right(__arg0,__arg1)(__pos)
  def forge_float_plus(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext) = primitive_forge_float_plus(__arg0,__arg1)(__pos)
  def forge_float_minus(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext) = primitive_forge_float_minus(__arg0,__arg1)(__pos)
  def forge_float_times(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext) = primitive_forge_float_times(__arg0,__arg1)(__pos)
  def forge_float_divide(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext) = primitive_forge_float_divide(__arg0,__arg1)(__pos)
  def forge_double_plus(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = primitive_forge_double_plus(__arg0,__arg1)(__pos)
  def forge_double_minus(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = primitive_forge_double_minus(__arg0,__arg1)(__pos)
  def forge_double_times(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = primitive_forge_double_times(__arg0,__arg1)(__pos)
  def forge_double_divide(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = primitive_forge_double_divide(__arg0,__arg1)(__pos)
  def forge_long_plus(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext) = primitive_forge_long_plus(__arg0,__arg1)(__pos)
  def forge_long_minus(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext) = primitive_forge_long_minus(__arg0,__arg1)(__pos)
  def forge_long_times(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext) = primitive_forge_long_times(__arg0,__arg1)(__pos)
  def forge_long_divide(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext) = primitive_forge_long_divide(__arg0,__arg1)(__pos)
  def forge_long_divide_double(__arg0: Rep[Long],__arg1: Rep[Double])(implicit __pos: SourceContext) = primitive_forge_long_divide_double(__arg0,__arg1)(__pos)
  def forge_long_and(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext) = primitive_forge_long_and(__arg0,__arg1)(__pos)
  def forge_long_or(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext) = primitive_forge_long_or(__arg0,__arg1)(__pos)
  def forge_long_xor(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext) = primitive_forge_long_xor(__arg0,__arg1)(__pos)
  def forge_long_shift_right_unsigned(__arg0: Rep[Long],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_long_shift_right_unsigned(__arg0,__arg1)(__pos)
  def forge_long_shift_right(__arg0: Rep[Long],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_long_shift_right(__arg0,__arg1)(__pos)
  def forge_long_shift_left(__arg0: Rep[Long],__arg1: Rep[Int])(implicit __pos: SourceContext) = primitive_forge_long_shift_left(__arg0,__arg1)(__pos)

  implicit def repToPrimitiveBooleanOpsCls(x: Rep[Boolean])(implicit __pos: SourceContext) = new PrimitiveBooleanOpsCls(x)(__pos)
  implicit def varToPrimitiveBooleanOpsCls(x: Var[Boolean])(implicit __pos: SourceContext) = new PrimitiveBooleanOpsCls(readVar(x))(__pos)

  class PrimitiveBooleanOpsCls(val self: Rep[Boolean])(implicit __pos: SourceContext) {
    def unary_!(implicit __pos: SourceContext) = primitive_unary_bang(self)(__pos)
    def ||(__arg1: Rep[Boolean])(implicit __pos: SourceContext) = primitive_oror(self,__arg1)(__pos)
    def &&(__arg1: Rep[Boolean])(implicit __pos: SourceContext) = primitive_andand(self,__arg1)(__pos)
  }

  implicit def repToPrimitiveIntOpsCls(x: Rep[Int])(implicit __pos: SourceContext) = new PrimitiveIntOpsCls(x)(__pos)
  implicit def liftToPrimitiveIntOpsCls(x: Int)(implicit __pos: SourceContext) = new PrimitiveIntOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveIntOpsCls(x: Var[Int])(implicit __pos: SourceContext) = new PrimitiveIntOpsCls(readVar(x))(__pos)

  class PrimitiveIntOpsCls(val self: Rep[Int])(implicit __pos: SourceContext) {
    def unary_-(implicit __pos: SourceContext,__imp1: ROverload1) = { unit(-1)*self }
    def %(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1) = primitive_%(self,__arg1)(__pos,overload1)
    def unary_~(implicit __pos: SourceContext,__imp1: Overload1) = primitive_unary_~(self)(__pos,overload1)
    def <<(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload1) = { forge_int_shift_left(self,__arg1) }
    def >>(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload1) = { forge_int_shift_right(self,__arg1) }
    def >>>(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload1) = { forge_int_shift_right_unsigned(self,__arg1) }
    def &(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload1) = { forge_int_and(self,__arg1) }
    def |(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload1) = { forge_int_or(self,__arg1) }
    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload75) = { forge_double_plus(self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload75) = { forge_double_minus(self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload75) = { forge_double_times(self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload75) = { forge_double_divide(self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload76) = { forge_double_plus(self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload76) = { forge_double_minus(self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload76) = { forge_double_times(self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload76) = { forge_double_divide(self.toDouble, rhs) }
    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload77) = { forge_double_plus(self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload77) = { forge_double_minus(self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload77) = { forge_double_times(self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload77) = { forge_double_divide(self.toDouble, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload78) = { forge_float_plus(self.toFloat, unit(rhs)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload78) = { forge_float_minus(self.toFloat, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload78) = { forge_float_times(self.toFloat, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload78) = { forge_float_divide(self.toFloat, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload79) = { forge_float_plus(self.toFloat, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload79) = { forge_float_minus(self.toFloat, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload79) = { forge_float_times(self.toFloat, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload79) = { forge_float_divide(self.toFloat, rhs) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload80) = { forge_float_plus(self.toFloat, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload80) = { forge_float_minus(self.toFloat, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload80) = { forge_float_times(self.toFloat, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload80) = { forge_float_divide(self.toFloat, readVar(rhs)) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload81) = { forge_int_plus(self, unit(rhs)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload81) = { forge_int_minus(self, unit(rhs)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload81) = { forge_int_times(self, unit(rhs)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload81) = { forge_int_divide(self, unit(rhs)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload82) = { forge_int_plus(self, rhs) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload82) = { forge_int_minus(self, rhs) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload82) = { forge_int_times(self, rhs) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload82) = { forge_int_divide(self, rhs) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload83) = { forge_int_plus(self, readVar(rhs)) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload83) = { forge_int_minus(self, readVar(rhs)) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload83) = { forge_int_times(self, readVar(rhs)) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload83) = { forge_int_divide(self, readVar(rhs)) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload84) = { forge_long_plus(self.toLong, unit(rhs)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload84) = { forge_long_minus(self.toLong, unit(rhs)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload84) = { forge_long_times(self.toLong, unit(rhs)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload84) = { forge_long_divide(self.toLong, unit(rhs)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload85) = { forge_long_plus(self.toLong, rhs) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload85) = { forge_long_minus(self.toLong, rhs) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload85) = { forge_long_times(self.toLong, rhs) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload85) = { forge_long_divide(self.toLong, rhs) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload86) = { forge_long_plus(self.toLong, readVar(rhs)) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload86) = { forge_long_minus(self.toLong, readVar(rhs)) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload86) = { forge_long_times(self.toLong, readVar(rhs)) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload86) = { forge_long_divide(self.toLong, readVar(rhs)) }
  }

  implicit def repToPrimitiveLongOpsCls(x: Rep[Long])(implicit __pos: SourceContext) = new PrimitiveLongOpsCls(x)(__pos)
  implicit def liftToPrimitiveLongOpsCls(x: Long)(implicit __pos: SourceContext) = new PrimitiveLongOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveLongOpsCls(x: Var[Long])(implicit __pos: SourceContext) = new PrimitiveLongOpsCls(readVar(x))(__pos)

  class PrimitiveLongOpsCls(val self: Rep[Long])(implicit __pos: SourceContext) {
    def unary_-(implicit __pos: SourceContext,__imp1: ROverload2) = { unit(-1L)*self }
    def %(__arg1: Rep[Long])(implicit __pos: SourceContext,__imp1: Overload2) = primitive_%(self,__arg1)(__pos,overload2)
    def unary_~(implicit __pos: SourceContext,__imp1: Overload2) = primitive_unary_~(self)(__pos,overload2)
    def &(__arg1: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload2) = { forge_long_and(self,__arg1) }
    def |(__arg1: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload2) = { forge_long_or(self,__arg1) }
    def ^(__arg1: Rep[Long])(implicit __pos: SourceContext) = { forge_long_xor(self,__arg1) }
    def >>>(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload2) = { forge_long_shift_right_unsigned(self,__arg1) }
    def <<(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload2) = { forge_long_shift_left(self,__arg1) }
    def >>(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload2) = { forge_long_shift_right(self,__arg1) }
    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload107) = { forge_double_plus(self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload107) = { forge_double_minus(self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload107) = { forge_double_times(self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload107) = { forge_double_divide(self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload108) = { forge_double_plus(self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload108) = { forge_double_minus(self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload108) = { forge_double_times(self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload108) = { forge_double_divide(self.toDouble, rhs) }
    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload109) = { forge_double_plus(self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload109) = { forge_double_minus(self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload109) = { forge_double_times(self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload109) = { forge_double_divide(self.toDouble, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload110) = { forge_float_plus(self.toFloat, unit(rhs)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload110) = { forge_float_minus(self.toFloat, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload110) = { forge_float_times(self.toFloat, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload110) = { forge_float_divide(self.toFloat, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload111) = { forge_float_plus(self.toFloat, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload111) = { forge_float_minus(self.toFloat, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload111) = { forge_float_times(self.toFloat, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload111) = { forge_float_divide(self.toFloat, rhs) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload112) = { forge_float_plus(self.toFloat, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload112) = { forge_float_minus(self.toFloat, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload112) = { forge_float_times(self.toFloat, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload112) = { forge_float_divide(self.toFloat, readVar(rhs)) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload113) = { forge_long_plus(self, unit(rhs.toLong)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload113) = { forge_long_minus(self, unit(rhs.toLong)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload113) = { forge_long_times(self, unit(rhs.toLong)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload113) = { forge_long_divide(self, unit(rhs.toLong)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload114) = { forge_long_plus(self, rhs.toLong) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload114) = { forge_long_minus(self, rhs.toLong) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload114) = { forge_long_times(self, rhs.toLong) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload114) = { forge_long_divide(self, rhs.toLong) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload115) = { forge_long_plus(self, readVar(rhs).toLong) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload115) = { forge_long_minus(self, readVar(rhs).toLong) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload115) = { forge_long_times(self, readVar(rhs).toLong) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload115) = { forge_long_divide(self, readVar(rhs).toLong) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload116) = { forge_long_plus(self, unit(rhs)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload116) = { forge_long_minus(self, unit(rhs)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload116) = { forge_long_times(self, unit(rhs)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload116) = { forge_long_divide(self, unit(rhs)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload117) = { forge_long_plus(self, rhs) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload117) = { forge_long_minus(self, rhs) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload117) = { forge_long_times(self, rhs) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload117) = { forge_long_divide(self, rhs) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload118) = { forge_long_plus(self, readVar(rhs)) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload118) = { forge_long_minus(self, readVar(rhs)) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload118) = { forge_long_times(self, readVar(rhs)) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload118) = { forge_long_divide(self, readVar(rhs)) }
  }

  implicit def repToPrimitiveFloatOpsCls(x: Rep[Float])(implicit __pos: SourceContext) = new PrimitiveFloatOpsCls(x)(__pos)
  implicit def liftToPrimitiveFloatOpsCls(x: Float)(implicit __pos: SourceContext) = new PrimitiveFloatOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveFloatOpsCls(x: Var[Float])(implicit __pos: SourceContext) = new PrimitiveFloatOpsCls(readVar(x))(__pos)

  class PrimitiveFloatOpsCls(val self: Rep[Float])(implicit __pos: SourceContext) {
    def unary_-(implicit __pos: SourceContext,__imp1: ROverload3) = { unit(-1f)*self }
    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload43) = { forge_double_plus(self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload43) = { forge_double_minus(self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload43) = { forge_double_times(self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload43) = { forge_double_divide(self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload44) = { forge_double_plus(self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload44) = { forge_double_minus(self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload44) = { forge_double_times(self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload44) = { forge_double_divide(self.toDouble, rhs) }
    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload45) = { forge_double_plus(self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload45) = { forge_double_minus(self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload45) = { forge_double_times(self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload45) = { forge_double_divide(self.toDouble, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload46) = { forge_float_plus(self, unit(rhs)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload46) = { forge_float_minus(self, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload46) = { forge_float_times(self, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload46) = { forge_float_divide(self, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload47) = { forge_float_plus(self, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload47) = { forge_float_minus(self, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload47) = { forge_float_times(self, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload47) = { forge_float_divide(self, rhs) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload48) = { forge_float_plus(self, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload48) = { forge_float_minus(self, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload48) = { forge_float_times(self, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload48) = { forge_float_divide(self, readVar(rhs)) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload49) = { forge_float_plus(self, unit(rhs.toFloat)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload49) = { forge_float_minus(self, unit(rhs.toFloat)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload49) = { forge_float_times(self, unit(rhs.toFloat)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload49) = { forge_float_divide(self, unit(rhs.toFloat)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload50) = { forge_float_plus(self, rhs.toFloat) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload50) = { forge_float_minus(self, rhs.toFloat) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload50) = { forge_float_times(self, rhs.toFloat) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload50) = { forge_float_divide(self, rhs.toFloat) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload51) = { forge_float_plus(self, readVar(rhs).toFloat) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload51) = { forge_float_minus(self, readVar(rhs).toFloat) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload51) = { forge_float_times(self, readVar(rhs).toFloat) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload51) = { forge_float_divide(self, readVar(rhs).toFloat) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload52) = { forge_float_plus(self, unit(rhs.toFloat)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload52) = { forge_float_minus(self, unit(rhs.toFloat)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload52) = { forge_float_times(self, unit(rhs.toFloat)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload52) = { forge_float_divide(self, unit(rhs.toFloat)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload53) = { forge_float_plus(self, rhs.toFloat) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload53) = { forge_float_minus(self, rhs.toFloat) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload53) = { forge_float_times(self, rhs.toFloat) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload53) = { forge_float_divide(self, rhs.toFloat) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload54) = { forge_float_plus(self, readVar(rhs).toFloat) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload54) = { forge_float_minus(self, readVar(rhs).toFloat) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload54) = { forge_float_times(self, readVar(rhs).toFloat) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload54) = { forge_float_divide(self, readVar(rhs).toFloat) }
  }

  implicit def repToPrimitiveDoubleOpsCls(x: Rep[Double])(implicit __pos: SourceContext) = new PrimitiveDoubleOpsCls(x)(__pos)
  implicit def liftToPrimitiveDoubleOpsCls(x: Double)(implicit __pos: SourceContext) = new PrimitiveDoubleOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveDoubleOpsCls(x: Var[Double])(implicit __pos: SourceContext) = new PrimitiveDoubleOpsCls(readVar(x))(__pos)

  class PrimitiveDoubleOpsCls(val self: Rep[Double])(implicit __pos: SourceContext) {
    def unary_-(implicit __pos: SourceContext,__imp1: ROverload4) = { unit(-1)*self }
    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload11) = { forge_double_plus(self, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload11) = { forge_double_minus(self, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload11) = { forge_double_times(self, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: ROverload11) = { forge_double_divide(self, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload12) = { forge_double_plus(self, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload12) = { forge_double_minus(self, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload12) = { forge_double_times(self, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload12) = { forge_double_divide(self, rhs) }
    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload13) = { forge_double_plus(self, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload13) = { forge_double_minus(self, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload13) = { forge_double_times(self, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: ROverload13) = { forge_double_divide(self, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload14) = { forge_double_plus(self, unit(rhs.toDouble)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload14) = { forge_double_minus(self, unit(rhs.toDouble)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload14) = { forge_double_times(self, unit(rhs.toDouble)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: ROverload14) = { forge_double_divide(self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload15) = { forge_double_plus(self, rhs.toDouble) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload15) = { forge_double_minus(self, rhs.toDouble) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload15) = { forge_double_times(self, rhs.toDouble) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: ROverload15) = { forge_double_divide(self, rhs.toDouble) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload16) = { forge_double_plus(self, readVar(rhs).toDouble) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload16) = { forge_double_minus(self, readVar(rhs).toDouble) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload16) = { forge_double_times(self, readVar(rhs).toDouble) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: ROverload16) = { forge_double_divide(self, readVar(rhs).toDouble) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload17) = { forge_double_plus(self, unit(rhs.toDouble)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload17) = { forge_double_minus(self, unit(rhs.toDouble)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload17) = { forge_double_times(self, unit(rhs.toDouble)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: ROverload17) = { forge_double_divide(self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload18) = { forge_double_plus(self, rhs.toDouble) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload18) = { forge_double_minus(self, rhs.toDouble) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload18) = { forge_double_times(self, rhs.toDouble) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: ROverload18) = { forge_double_divide(self, rhs.toDouble) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload19) = { forge_double_plus(self, readVar(rhs).toDouble) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload19) = { forge_double_minus(self, readVar(rhs).toDouble) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload19) = { forge_double_times(self, readVar(rhs).toDouble) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: ROverload19) = { forge_double_divide(self, readVar(rhs).toDouble) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload20) = { forge_double_plus(self, unit(rhs.toDouble)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload20) = { forge_double_minus(self, unit(rhs.toDouble)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload20) = { forge_double_times(self, unit(rhs.toDouble)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: ROverload20) = { forge_double_divide(self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload21) = { forge_double_plus(self, rhs.toDouble) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload21) = { forge_double_minus(self, rhs.toDouble) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload21) = { forge_double_times(self, rhs.toDouble) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: ROverload21) = { forge_double_divide(self, rhs.toDouble) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload22) = { forge_double_plus(self, readVar(rhs).toDouble) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload22) = { forge_double_minus(self, readVar(rhs).toDouble) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload22) = { forge_double_times(self, readVar(rhs).toDouble) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: ROverload22) = { forge_double_divide(self, readVar(rhs).toDouble) }
  }

  implicit def repToPrimitiveTOpsCls[T:Typ](x: Rep[T])(implicit __pos: SourceContext) = new PrimitiveTOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToPrimitiveTOpsCls[T:Typ](x: Var[T])(implicit __pos: SourceContext) = new PrimitiveTOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class PrimitiveTOpsCls[T:Typ](val self: Rep[T])(implicit __pos: SourceContext) {
    def toInt(implicit __cb0: Numeric[T],__pos: SourceContext,__imp1: Overload1) = primitive_toint[T](self)(implicitly[Numeric[T]],implicitly[Typ[T]],__pos)
    def toFloat(implicit __cb0: Numeric[T],__pos: SourceContext,__imp1: Overload1) = primitive_tofloat[T](self)(implicitly[Numeric[T]],implicitly[Typ[T]],__pos)
    def toDouble(implicit __cb0: Numeric[T],__pos: SourceContext,__imp1: Overload1) = primitive_todouble[T](self)(implicitly[Numeric[T]],implicitly[Typ[T]],__pos)
    def toLong(implicit __cb0: Numeric[T],__pos: SourceContext,__imp1: Overload1) = primitive_tolong[T](self)(implicitly[Numeric[T]],implicitly[Typ[T]],__pos)
  }



  def primitive_unary_bang(__arg0: Rep[Boolean])(implicit __pos: SourceContext): Rep[Boolean]
  def primitive_oror(__arg0: Rep[Boolean],__arg1: Rep[Boolean])(implicit __pos: SourceContext): Rep[Boolean]
  def primitive_andand(__arg0: Rep[Boolean],__arg1: Rep[Boolean])(implicit __pos: SourceContext): Rep[Boolean]
  def primitive_toint[T:Numeric:Typ](__arg0: Rep[T])(implicit __pos: SourceContext): Rep[Int]
  def primitive_tofloat[T:Numeric:Typ](__arg0: Rep[T])(implicit __pos: SourceContext): Rep[Float]
  def primitive_todouble[T:Numeric:Typ](__arg0: Rep[T])(implicit __pos: SourceContext): Rep[Double]
  def primitive_tolong[T:Numeric:Typ](__arg0: Rep[T])(implicit __pos: SourceContext): Rep[Long]
  def primitive_forge_int_plus(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def primitive_forge_int_minus(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def primitive_forge_int_times(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def primitive_forge_int_divide(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def primitive_forge_int_shift_left(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def primitive_forge_int_shift_right_unsigned(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def primitive_forge_int_and(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def primitive_forge_int_or(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def primitive_forge_int_shift_right(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def primitive_%(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Int]
  def primitive_unary_~(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Int]
  def primitive_forge_float_plus(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext): Rep[Float]
  def primitive_forge_float_minus(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext): Rep[Float]
  def primitive_forge_float_times(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext): Rep[Float]
  def primitive_forge_float_divide(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext): Rep[Float]
  def primitive_forge_double_plus(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def primitive_forge_double_minus(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def primitive_forge_double_times(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def primitive_forge_double_divide(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def primitive_forge_long_plus(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext): Rep[Long]
  def primitive_forge_long_minus(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext): Rep[Long]
  def primitive_forge_long_times(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext): Rep[Long]
  def primitive_forge_long_divide(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext): Rep[Long]
  def primitive_forge_long_divide_double(__arg0: Rep[Long],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def primitive_forge_long_and(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext): Rep[Long]
  def primitive_forge_long_or(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext): Rep[Long]
  def primitive_forge_long_xor(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext): Rep[Long]
  def primitive_forge_long_shift_right_unsigned(__arg0: Rep[Long],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Long]
  def primitive_forge_long_shift_right(__arg0: Rep[Long],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Long]
  def primitive_forge_long_shift_left(__arg0: Rep[Long],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Long]
  def primitive_%(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext,__imp1: Overload2): Rep[Long]
  def primitive_unary_~(__arg0: Rep[Long])(implicit __pos: SourceContext,__imp1: Overload2): Rep[Long]
}
