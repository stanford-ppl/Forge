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
import ppl.delite.framework.extern.codegen.cuda.CudaGenExternalBase
import ppl.delite.framework.extern.lib.MKL
import ppl.delite.framework.extern.lib.cuBLAS

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

  def MIN_SIZE = unit(100)

  /**
   * Intercept BLAS-able operations
   */

  case class Native_matMult[T:Manifest](xR: Rep[Int], xC: Rep[Int], x: Rep[ForgeArray[T]], yR: Rep[Int], yC: Rep[Int], y: Rep[ForgeArray[T]])(implicit val __pos: SourceContext,val __imp0: Arith[T]) extends DeliteOpExternal[ForgeArray[T]] {
    val _mT = implicitly[Manifest[T]]

    override def inputs = scala.List(xR,xC,x,yR,yC,y)
    def alloc = Array.empty[T](xR * yC)
    val funcName = "matMult"
  }

  case class Native_matTimesVec[T:Manifest](xR: Rep[Int], xC: Rep[Int], x: Rep[ForgeArray[T]],y: Rep[ForgeArray[T]])(implicit val __pos: SourceContext,val __imp0: Arith[T]) extends DeliteOpExternal[ForgeArray[T]] {
    val _mT = implicitly[Manifest[T]]

    override def inputs = scala.List(xR,xC,x,y)
    def alloc = Array.empty[T](xR)
    val funcName = "matTimesVec"
  }

  override def densematrix_matmult[T:Manifest](self: Rep[DenseMatrix[T]],__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseMatrix[T]] = {
    if (useBLAS && Config.useBlas && (manifest[T] == manifest[Double] || manifest[T] == manifest[Float])) {
      if (densematrix_size(self) > MIN_SIZE && densematrix_size(__arg0) > MIN_SIZE) {
        densematrix_fromarray(Native_matMult(self.numRows,self.numCols,densematrix_raw_data(self),__arg0.numRows,__arg0.numCols,densematrix_raw_data(__arg0)),self.numRows,__arg0.numCols)
      }
      else {
        super.densematrix_matmult(self,__arg0)
      }
    }
    else super.densematrix_matmult(self,__arg0)
  }

  override def densematrix_matvecmult[T:Manifest](self: Rep[DenseMatrix[T]],__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T]): Rep[DenseVector[T]] = {
    if (useBLAS && Config.useBlas && (manifest[T] == manifest[Double] || manifest[T] == manifest[Float])) densevector_fromarray(Native_matTimesVec(self.numRows,self.numCols,densematrix_raw_data(self),densevector_raw_data(__arg0)),unit(false))
    else super.densematrix_matvecmult(self,__arg0)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Native_matMult(xR,xC,x,yR,yC,y) => reflectPure(new { override val original = Some(f,e) } with Native_matMult(f(xR),f(xC),f(x),f(yR),f(yC),f(y))(e._mT,pos,e.__imp0))(mtype(manifest[A]),pos)
    case e@Native_matTimesVec(xR,xC,x,y) => reflectPure(new { override val original = Some(f,e) } with Native_matTimesVec(f(xR),f(xC),f(x),f(y))(e._mT,pos,e.__imp0))(mtype(manifest[A]),pos)

    case Reflect(e@Native_matMult(xR,xC,x,yR,yC,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Native_matMult(f(xR),f(xC),f(x),f(yR),f(yC),f(y))(e._mT,pos,e.__imp0), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@Native_matTimesVec(xR,xC,x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Native_matTimesVec(f(xR),f(xC),f(x),f(y))(e._mT,pos,e.__imp0), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait CudaGenBLASOps extends CudaGenExternalBase {
  val IR: BLASOpsExp with OptiLAExp
  import IR._

  override def emitExternalNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Native_matMult(xR,xC,x,yR,yC,y) =>
      val args = scala.List("'n'", "'n'", "%1$s", "%2$s", "%3$s", "1.0", "%4$s->data", "%1$s", "%5$s->data", "%6$s", "0.0", "%7$s->data", "%1$s")
                 .map { _.format(quote(yC), quote(xR), quote(yR), quote(y), quote(x), quote(xC), quote(sym)) }
      emitMethodCall(sym, e, cuBLAS, args)
      registerKernel(scala.List(sym))

    case e@Native_matTimesVec(xR,xC,x,y) =>
      val args = scala.List("'t'", "%1$s", "%2$s", "%3$s->data", "%4$s->data", "%5$s->data")
                 .map { _.format(quote(xC), quote(xR), quote(x), quote(y), quote(sym)) }
      emitMethodCall(sym, e, cuBLAS, args)
      registerKernel(scala.List(sym))


    case _ => super.emitExternalNode(sym, rhs)
  }

  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
    case e@Native_matMult(xR,xC,x,yR,yC,y) =>
      val tp = remap(e._mT)
      val func = tp match {
        case "double" => "cublasDgemm"
        case "float" => "cublasSgemm"
      }
      emitInterfaceAndMethod(cuBLAS, e.funcName, scala.List("char n1", "char n2", "int mat2_col", "int mat1_row", "int mat2_row", tp+" a", "%1$s* mat2".format(tp), "int mat2_col_b", "%1$s* mat1".format(tp), "int mat1_col", tp+" b", "%1$s* mat3".format(tp), "int mat3_col"), "",
"""
{
  // HJ TODO: use a real stream
  //cublasSetKernelStream(stream);
  %1$s(n1, n2, mat2_col, mat1_row, mat2_row, a, mat2, mat2_col_b, mat1, mat1_col, b, mat3, mat3_col);
}""".format(func))

    case e@Native_matTimesVec(xR,xC,x,y) =>
      val tp = remap(e._mT)
      val func = tp match {
        case "double" => "cublasDgemv"
        case "float" => "cublasSgemv"
      }
      emitInterfaceAndMethod(cuBLAS, e.funcName, scala.List("char transpose", "int mat_col", "int mat_row", "%1$s* mat1".format(tp), "%1$s* vec2".format(tp), "%1$s* vec3".format(tp)), "",
"""
{
  // HJ TODO: use a real stream
  //cublasSetKernelStream(stream);
  %1$s(transpose, mat_col, mat_row, 1.0, mat1, mat_col, vec2, 1, 0.0, vec3, 1);
}""".format(func))


    case _ => super.emitExternalLib(rhs)
  }

}

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
    case e@Native_matMult(xR,xC,x,yR,yC,y) =>
      val args = scala.List("%1$s", "%2$s", "%3$s", "%4$s", "%5$s", "%6$s")
                 .map { _.format(quote(x), quote(y), quote(sym), quote(xR), quote(xC), quote(yC)) }
      emitMethodCall(sym, e, MKL, args)

    case e@Native_matTimesVec(xR,xC,x,y) =>
      val args = scala.List("%1$s", "%2$s", "%3$s", "%4$s", "%5$s", "0", "1")
                 .map { _.format(quote(x), quote(y), quote(sym), quote(xR), quote(xC)) }
      emitMethodCall(sym, e, MKL, args)
    case _ => super.emitExternalNode(sym,rhs)
  }

  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {

    case e@Native_matMult(xR,xC,x,yR,yC,y) =>
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


    case e@Native_matTimesVec(xR,xC,x,y) =>
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

