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


trait RewriteOpsExp extends RewriteOps with Primitive2OpsExp with DenseVectorOpsExp with DenseVectorViewOpsExp {
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


  // Vector

  override def densevector_length[T:Manifest](self: Rep[DenseVector[T]])(implicit __pos: SourceContext) = self match {
    /* propagate output size information */
    // is this still necessary given the new struct rewrites?
    case Def(e: DeliteOpMap[_,_,_]) => e.size
    case Def(e: DeliteOpZipWith[_,_,_,_]) => e.size
    case _ => super.densevector_length(self)      
  }

  case class VectorDotProduct[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVectorView[A]])
    extends DeliteOpZipWithReduce[A,A,A] {

    def zip = (a,b) => a*b
    def reduce = (a,b) => a+b
    val size = copyTransformedOrElse(_.size)(inA.length)
    val zero = implicitly[Arith[A]].empty
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
  }

  // This rewrite is added to remove temporary allocations on vector dot product when one of them is mutable. (e.g., Hogwild)
  // Remove this when the fusion can be automatically applied (probably by enabling struct unwrapping for read effect loops)
  override def densevector_dot_densevectorview[T:Manifest](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T]) = {
    reflectPure(VectorDotProduct(self,__arg1))
  }

  
  // VectorView
  // TEST: do we still need these even with struct field rewrites?
  override def densevectorview_length[A:Manifest](x: Exp[DenseVectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(DenseMatrix_Vview(m, start, stride, l, r)) => l
    case Def(s@Reflect(DenseMatrix_Vview(m, start, stride, l, r), u, es)) if context.contains(s) => l
    case _ => super.densevectorview_length(x)
  }   
  
  override def densevectorview_isrow[A:Manifest](x: Exp[DenseVectorView[A]])(implicit ctx: SourceContext) = x match {
    case Def(DenseMatrix_Vview(m, start, stride, l, r)) => r
    case Def(s@Reflect(DenseMatrix_Vview(m, start, stride, l, r), u, es)) if context.contains(s) => r
    case _ => super.densevectorview_isrow(x)  
  }
  
  def dense_vectorview_optimize_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext): Option[Exp[A]] = x match {
    case Def(DenseMatrix_Vview(m, start, stride, l, r)) => Some(densematrix_raw_apply(m,start + n*stride))
    case Def(s@Reflect(DenseMatrix_Vview(m, start, stride, l, r), u, es)) if context.contains(s) => Some(densematrix_raw_apply(m,start + n*stride))
    case _ => None
  }
  
  override def densevectorview_apply[A:Manifest](x: Exp[DenseVectorView[A]], n: Exp[Int])(implicit ctx: SourceContext) = { 
    dense_vectorview_optimize_apply(x.asInstanceOf[Exp[DeliteCollection[A]]],n) getOrElse super.densevectorview_apply(x,n)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = { 
    dense_vectorview_optimize_apply(x,n) getOrElse super.dc_apply(x,n)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@VectorDotProduct(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDotProduct(f(x),f(y))(e.mA, e.a))(mtype(manifest[A]),implicitly[SourceContext])
    //case Reflect(e@VectorDotProduct(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDotProduct(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorDotProduct(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDotProduct(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}
