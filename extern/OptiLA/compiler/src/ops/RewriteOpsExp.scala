package optila.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import ppl.delite.framework.ops.{DeliteCollection, DeliteOpsExp, DeliteCollectionOpsExp}

import optila.shared._
import optila.shared.ops._
import optila.shared.typeclass._
import optila.compiler._
import optila.compiler.ops._

trait RewriteOpsExp extends RewriteOps with DenseVectorViewOpsExp {
  this: OptiLAExp =>

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
  // Need to make all the combinations of densevector / densevectorview?
  // The implicit parameter OverloadXX changes as the DSL changes..
  override def densevector_mulclnmul[T:Manifest](self: Rep[DenseVector[T]],__arg1: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload21) = {
    reflectPure(VectorDotProduct(self,__arg1))
  }

  // rewrites for vector view 
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
	case Reflect(e@VectorDotProduct(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDotProduct(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
	case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenRewriteOps
trait CudaGenRewriteOps
trait OpenCLGenRewriteOps
trait CGenRewriteOps
