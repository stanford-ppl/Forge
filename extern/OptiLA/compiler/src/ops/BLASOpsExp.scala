package optila.compiler.ops

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import ppl.delite.framework.ops.DeliteCollection
import ppl.delite.framework.datastructures._
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.Util._

import ppl.delite.framework.Config
import ppl.delite.framework.extern.codegen.scala.ScalaGenExternalBase
import ppl.delite.framework.extern.lib.MKL

import optila.shared._
import optila.shared.ops._
import optila.shared.typeclass._
import optila.compiler._
import optila.compiler.ops._

/**
 * Currently, native operations are entirely specified in external files. This has more boilerplate compared to using
 * a 'native' keyword in Forge to generate the DeliteOpExternal and mirror definitions, but allows us to call native
 * libraries essentially like macros, without changing the spec at all, or worrying about specifying things like guards
 * (if useBLAS(...)) or default implementations. Instead of distinguishing 'native' calls in the spec, any call can
 * be made native by adding the external override after-the-fact.
 *
 * This also provides us the freedom to use different external implementation strategies for each back-end target, without
 * changing Forge. However, once we have settled on a native lib generation plan for each target (and know the common components
 * that must be specified for the op), we probably want to use Forge to generate the boilerplate for all targets.
 */

trait BLASOpsExp extends BLASOps with DenseMatrixOpsExp {
  this: OptiLAExp =>

  /**
   * Intercept BLAS-able operations
   */
  case class Native_matMult[T:Manifest](self: Rep[DenseMatrix[T]],__arg0: Rep[DenseMatrix[T]])(implicit val __pos: SourceContext,val __imp0: Arith[T]) extends DeliteOpExternal[DenseMatrix[T]] {
    val _mT = implicitly[Manifest[T]]

    override def inputs = scala.List(self,__arg0)
    def alloc = DenseMatrix[T](self.numRows, __arg0.numCols)
    val funcName = "matMult"
  }

  case class Native_matTimesVec[T:Manifest](self: Rep[DenseMatrix[T]],__arg0: Rep[DenseVector[T]])(implicit val __pos: SourceContext,val __imp0: Arith[T]) extends DeliteOpExternal[DenseVector[T]] {
    val _mT = implicitly[Manifest[T]]

    override def inputs = scala.List(self,__arg0)
    def alloc = DenseVector[T](self.numRows, unit(false))
    val funcName = "matTimesVec"
  }

  override def densematrix_matmult[T:Manifest](self: Rep[DenseMatrix[T]],__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]] = {
    if (useBLAS && Config.useBlas && (manifest[T] == manifest[Double] || manifest[T] == manifest[Float])) reflectPure(Native_matMult(self,__arg0))
    else super.densematrix_matmult(self,__arg0)
  }
  override def densematrix_matvecmult[T:Manifest](self: Rep[DenseMatrix[T]],__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]] = {
    if (useBLAS && Config.useBlas && (manifest[T] == manifest[Double] || manifest[T] == manifest[Float])) reflectPure(Native_matTimesVec(self,__arg0))
    else super.densematrix_matvecmult(self,__arg0)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Native_matMult(a,b) => reflectPure(new { override val original = Some(f,e) } with Native_matMult(f(a),f(b))(e._mT,pos,e.__imp0))(mtype(manifest[A]),pos)
    case e@Native_matTimesVec(a,b) => reflectPure(new { override val original = Some(f,e) } with Native_matTimesVec(f(a),f(b))(e._mT,pos,e.__imp0))(mtype(manifest[A]),pos)

    case Reflect(e@Native_matMult(a,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Native_matMult(f(a),f(b))(e._mT,pos,e.__imp0), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@Native_matTimesVec(a,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Native_matTimesVec(f(a),f(b))(e._mT,pos,e.__imp0), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait CudaGenBLASOps
trait OpenCLGenBLASOps
trait CGenBLASOps

trait ScalaGenBLASOps extends ScalaGenExternalBase {
  val IR: BLASOpsExp with OptiLAExp
  import IR._

  // TODO: 'BLAS' is a Delite object right now, but to use it in the library impl, this needs to be refactored.
  // much of this is boilerplate that we can generate. what do we really need?
  //   -- type signature of lib call (Array*,Array*,Array*,int,int,int)
  //   -- actual invocation method
  //   -- mapping between op inputs and c call arguments?


  /**
   * JNI implementation
   */
  override def emitExternalNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Native_matMult(x,y) =>
      val args = scala.List("%1$s._data", "%2$s._data", "%3$s._data", "%1$s._numRows", "%1$s._numCols", "%2$s._numCols")
                 .map { _.format(quote(x), quote(y), quote(sym)) }
      emitMethodCall(sym, e, MKL, args)

    case e@Native_matTimesVec(x,y) =>
      val args = scala.List("%1$s._data", "%2$s._data", "%3$s._data", "%1$s._numRows", "%1$s._numCols", "0", "1")
                 .map { _.format(quote(x), quote(y), quote(sym)) }
      emitMethodCall(sym, e, MKL, args)
    case _ => super.emitExternalNode(sym,rhs)
  }

  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {

    case e@Native_matMult(x,y) =>
      val tp = e._mT.toString
      val func = tp match {
        case "Double" => "cblas_dgemm"
        case "Float" => "cblas_sgemm"
      }
      emitInterfaceAndMethod(MKL, e.funcName,
        scala.List("mat1:Array[%1$s]", "mat2:Array[%1$s]", "mat3:Array[%1$s]", "mat1_r:Int", "mat1_c:Int", "mat2_c:Int") map { _.format(tp) },
        scala.List("j%1$sArray mat1", "j%1$sArray mat2", "j%1$sArray mat3", "jint mat1_r", "jint mat1_c", "jint mat2_c") map { _.format(tp.toLowerCase) },
        """
        {
          jboolean copy;
          j%1$s *mat1_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy);
          j%1$s *mat2_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat2, &copy);
          j%1$s *mat3_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat3, &copy);

          %2$s(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1_r, mat2_c, mat1_c, 1.0, mat1_ptr, mat1_c, mat2_ptr, mat2_c, 0.0, mat3_ptr, mat2_c);

          (*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
          (*env)->ReleasePrimitiveArrayCritical(env, mat2, mat2_ptr, 0);
          (*env)->ReleasePrimitiveArrayCritical(env, mat3, mat3_ptr, 0);
        }""".format(tp.toLowerCase, func))


    case e@Native_matTimesVec(x,y) =>
      val tp = e._mT.toString
      val func = tp match {
        case "Double" => "cblas_dgemv"
        case "Float" => "cblas_sgemv"
      }
      emitInterfaceAndMethod(MKL, e.funcName,
        scala.List("mat1:Array[%1$s]", "vec2:Array[%1$s]", "vec3:Array[%1$s]", "mat_row:Int", "mat_col:Int", "vec_offset:Int", "vec_stride:Int") map { _.format(tp) },
        scala.List("j%1$sArray mat1", "j%1$sArray vec2", "j%1$sArray vec3", "jint mat_row", "jint mat_col", "jint vec_offset", "jint vec_stride") map { _.format(tp.toLowerCase) },
        """
        {
          jboolean copy;

          j%1$s *mat1_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
          j%1$s *vec2_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
          j%1$s *vec3_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec3, &copy));

          vec2_ptr += vec_offset;

          %2$s(CblasRowMajor, CblasNoTrans, mat_row, mat_col, 1.0, mat1_ptr, mat_col, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1);

          (*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
          (*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
          (*env)->ReleasePrimitiveArrayCritical(env, vec3, vec3_ptr, 0);
        }""".format(tp.toLowerCase, func))

    case _ => super.emitExternalLib(rhs)
  }
}

