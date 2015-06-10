package optila.compiler.ops

import scala.reflect.{Manifest,RefinedManifest,SourceContext}
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp,StructOps}
import ppl.delite.framework.ops.{DeliteCollection, DeliteOpsExp, DeliteCollectionOpsExp}

import optila.shared._
import optila.shared.ops._
import optila.shared.typeclass._
import optila.compiler._
import optila.compiler.ops._


trait RewriteOpsExp extends RewriteOps with Primitive2OpsExp {
  this: OptiLAExp =>

  /* Primitive math */

  def unbox[T:Manifest](n: java.lang.Number): T = {
    val mD = manifest[Double]
    val mF = manifest[Float]
    val mI = manifest[Int]
    val mL = manifest[Long]
    manifest[T] match {
      case `mD` => n.doubleValue().asInstanceOf[T]
      case `mF` => n.floatValue().asInstanceOf[T]
      case `mI` => n.intValue().asInstanceOf[T]
      case `mL` => n.longValue().asInstanceOf[T]
    }
  }

  def simplify_plus[T:Manifest:Numeric](a: Exp[T], b: Exp[T])(implicit __pos: SourceContext): Option[Exp[T]] = (a,b) match {
    case (Const(x), Const(y)) => 
      // weird java class cast exception inside numeric while unboxing java.lang.Integer
      val a = if (x.isInstanceOf[java.lang.Number]) unbox[T](x.asInstanceOf[java.lang.Number]) else x
      val b = if (y.isInstanceOf[java.lang.Number]) unbox[T](y.asInstanceOf[java.lang.Number]) else y    
      Some(unit(implicitly[Numeric[T]].plus(a,b)))
    case (Const(0 | 0.0 | 0.0f | -0.0 | -0.0f), y) => Some(y)
    case (y, Const(0 | 0.0 | 0.0f | -0.0 | -0.0f)) => Some(y)
    case _ => None
  }

  def simplify_minus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T])(implicit __pos: SourceContext): Option[Exp[T]] = (lhs,rhs) match {
    case (Const(x), Const(y)) => 
      val a = if (x.isInstanceOf[java.lang.Number]) unbox[T](x.asInstanceOf[java.lang.Number]) else x
      val b = if (y.isInstanceOf[java.lang.Number]) unbox[T](y.asInstanceOf[java.lang.Number]) else y    
      Some(unit(implicitly[Numeric[T]].minus(a,b)))
    // case (Const(0 | 0.0 | 0.0f | -0.0 | -0.0f), y) => unit(-1.asInstanceOf[T])*y
    case (y, Const(0 | 0.0 | 0.0f | -0.0 | -0.0f)) => Some(y)
    case _ => None
  }
  
  def simplify_times[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T])(implicit __pos: SourceContext): Option[Exp[T]] = (lhs,rhs) match {
    case (Const(x), Const(y)) => 
      val a = if (x.isInstanceOf[java.lang.Number]) unbox[T](x.asInstanceOf[java.lang.Number]) else x
      val b = if (y.isInstanceOf[java.lang.Number]) unbox[T](y.asInstanceOf[java.lang.Number]) else y
      Some(unit(implicitly[Numeric[T]].times(a,b)))
    case (Const(0 | 0.0 | 0.0f | -0.0 | -0.0f), _) => Some(lhs)
    case (_, Const(0 | 0.0 | 0.0f | -0.0 | -0.0f)) => Some(rhs)
//    case (Const(1 | 1.0 | 1.0f), y) => y //TODO: careful about type promotion!
//    case (y, Const(1 | 1.0 | 1.0f)) => y
    case _ => None
  }
  
  def simplify_numeric_divide[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T])(implicit __pos: SourceContext): Option[Exp[T]] = (lhs,rhs) match {
    case (Const(0 | 0L), _) => Some(lhs)  // TODO: shouldn't match on 0 / 0 ?
    case _ => None
  }

  def simplify_fractional_divide[T:Manifest:Fractional](lhs: Exp[T], rhs: Exp[T])(implicit __pos: SourceContext): Option[Exp[T]] = (lhs,rhs) match {
    case (Const(0 | 0.0 | 0.0f | -0.0 | -0.0f), _) => Some(lhs)  // TODO: shouldn't match on 0 / 0 ?
    case _ => None
  }  

  override def primitive2_forge_int_plus(__arg0: Exp[Int],__arg1: Exp[Int])(implicit __pos: SourceContext) = simplify_plus(__arg0, __arg1).getOrElse(super.primitive2_forge_int_plus(__arg0,__arg1))
  override def primitive2_forge_int_minus(__arg0: Exp[Int],__arg1: Exp[Int])(implicit __pos: SourceContext) = simplify_minus(__arg0, __arg1).getOrElse(super.primitive2_forge_int_minus(__arg0,__arg1))
  override def primitive2_forge_int_times(__arg0: Exp[Int],__arg1: Exp[Int])(implicit __pos: SourceContext) = simplify_times(__arg0, __arg1).getOrElse(super.primitive2_forge_int_times(__arg0,__arg1))
  override def primitive2_forge_int_divide(__arg0: Exp[Int],__arg1: Exp[Int])(implicit __pos: SourceContext) = simplify_numeric_divide(__arg0, __arg1).getOrElse(super.primitive2_forge_int_divide(__arg0,__arg1))
  override def primitive2_forge_float_plus(__arg0: Exp[Float],__arg1: Exp[Float])(implicit __pos: SourceContext) = simplify_plus(__arg0, __arg1).getOrElse(super.primitive2_forge_float_plus(__arg0,__arg1))
  override def primitive2_forge_float_minus(__arg0: Exp[Float],__arg1: Exp[Float])(implicit __pos: SourceContext) = simplify_minus(__arg0, __arg1).getOrElse(super.primitive2_forge_float_minus(__arg0,__arg1))
  override def primitive2_forge_float_times(__arg0: Exp[Float],__arg1: Exp[Float])(implicit __pos: SourceContext) = simplify_times(__arg0, __arg1).getOrElse(super.primitive2_forge_float_times(__arg0,__arg1))
  override def primitive2_forge_float_divide(__arg0: Exp[Float],__arg1: Exp[Float])(implicit __pos: SourceContext) = simplify_fractional_divide(__arg0, __arg1).getOrElse(super.primitive2_forge_float_divide(__arg0,__arg1))
  override def primitive2_forge_double_plus(__arg0: Exp[Double],__arg1: Exp[Double])(implicit __pos: SourceContext) = simplify_plus(__arg0, __arg1).getOrElse(super.primitive2_forge_double_plus(__arg0,__arg1))
  override def primitive2_forge_double_minus(__arg0: Exp[Double],__arg1: Exp[Double])(implicit __pos: SourceContext) = simplify_minus(__arg0, __arg1).getOrElse(super.primitive2_forge_double_minus(__arg0,__arg1))
  override def primitive2_forge_double_times(__arg0: Exp[Double],__arg1: Exp[Double])(implicit __pos: SourceContext) = simplify_times(__arg0, __arg1).getOrElse(super.primitive2_forge_double_times(__arg0,__arg1))
  override def primitive2_forge_double_divide(__arg0: Exp[Double],__arg1: Exp[Double])(implicit __pos: SourceContext) = simplify_fractional_divide(__arg0, __arg1).getOrElse(super.primitive2_forge_double_divide(__arg0,__arg1))
  override def primitive2_forge_long_plus(__arg0: Exp[Long],__arg1: Exp[Long])(implicit __pos: SourceContext) = simplify_plus(__arg0, __arg1).getOrElse(super.primitive2_forge_long_plus(__arg0,__arg1))
  override def primitive2_forge_long_minus(__arg0: Exp[Long],__arg1: Exp[Long])(implicit __pos: SourceContext) = simplify_minus(__arg0, __arg1).getOrElse(super.primitive2_forge_long_minus(__arg0,__arg1))
  override def primitive2_forge_long_times(__arg0: Exp[Long],__arg1: Exp[Long])(implicit __pos: SourceContext) = simplify_times(__arg0, __arg1).getOrElse(super.primitive2_forge_long_times(__arg0,__arg1))
  override def primitive2_forge_long_divide(__arg0: Exp[Long],__arg1: Exp[Long])(implicit __pos: SourceContext) = simplify_numeric_divide(__arg0, __arg1).getOrElse(super.primitive2_forge_long_divide(__arg0,__arg1))

  // const promotions

  override def primitive2_toint[T:Numeric:Manifest](__arg0: Rep[T])(implicit __pos: SourceContext) = __arg0 match {
    case Const(x) => Const(x.toInt)
    case _ => super.primitive2_toint(__arg0)
  }
  override def primitive2_tofloat[T:Numeric:Manifest](__arg0: Rep[T])(implicit __pos: SourceContext) = __arg0 match {
    case Const(x) => Const(x.toFloat)
    case _ => super.primitive2_tofloat(__arg0)
  }
  override def primitive2_todouble[T:Numeric:Manifest](__arg0: Rep[T])(implicit __pos: SourceContext) = __arg0 match {
    case Const(x) => Const(x.toDouble)
    case _ => super.primitive2_todouble(__arg0)
  }
  override def primitive2_tolong[T:Numeric:Manifest](__arg0: Rep[T])(implicit __pos: SourceContext) = __arg0 match {
    case Const(x) => Const(implicitly[Numeric[T]].toLong(x))
    case _ => super.primitive2_tolong(__arg0)
  }
  override def primitive2_repint2torepdouble(__arg0: Rep[Int])(implicit __pos: SourceContext) = __arg0 match {
    case Const(x) => Const(x.toDouble)
    case _ => super.primitive2_repint2torepdouble(__arg0)
  }
  override def primitive2_repint2torepfloat(__arg0: Rep[Int])(implicit __pos: SourceContext) = __arg0 match {
    case Const(x) => Const(x.toFloat)
    case _ => super.primitive2_repint2torepfloat(__arg0)
  }
  override def primitive2_repfloat2torepdouble(__arg0: Rep[Float])(implicit __pos: SourceContext) = __arg0 match {
    case Const(x) => Const(x.toDouble)
    case _ => super.primitive2_repfloat2torepdouble(__arg0)
  }

}
