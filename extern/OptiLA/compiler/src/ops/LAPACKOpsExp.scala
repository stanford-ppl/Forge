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
import ppl.delite.framework.extern.lib.LAPACK

import optila.shared._
import optila.shared.ops._
import optila.shared.typeclass._
import optila.compiler._
import optila.compiler.ops._


trait LAPACKOpsExp extends LAPACKOps with LinAlgOpsExp {
  this: OptiLAExp with LAPACKHelperOps =>

  /**
   * Intercept LAPACK-able operations
   */

  case class Native_linsolve(a: Rep[DenseMatrix[Double]], b: Rep[DenseVector[Double]])(implicit val __pos: SourceContext) extends DeliteOpExternal[Unit] {
    override def inputs = scala.List(a,b)
    def alloc = Const(())
    val funcName = "linsolve"
  }

  case class Native_lu(a: Rep[DenseMatrix[Double]], ipiv: Rep[DenseVector[Int]])(implicit val __pos: SourceContext) extends DeliteOpExternal[Unit] {
    override def inputs = scala.List(a,ipiv)
    def alloc = Const(())
    val funcName = "lu"
  }

  case class Native_chol(a: Rep[DenseMatrix[Double]], tri: Rep[Char])(implicit val __pos: SourceContext) extends DeliteOpExternal[Unit] {
    override def inputs = scala.List(a,tri)
    def alloc = Const(())
    val funcName = "chol"
  }

  // TODO: we also want an AX = B version where X and B are matrices
  override def linsolve(a: Rep[DenseMatrix[Double]], b: Rep[DenseVector[Double]])(implicit __pos: SourceContext): Rep[DenseVector[Double]] = {
    if (useLAPACK) {
      // a, b will be overwritten with answers
      val outB = DenseVector[Double](max(a.numRows,a.numCols), unit(false))
      outB(unit(0)::b.length) = b(unit(0)::b.length)
      val alloc = a.mutable
      reflectWrite(alloc,outB)(Native_linsolve(alloc, outB))
      outB.take(a.numCols) // answer is written in the first n entries of b
    }
    else super.linsolve(a,b)
  }

  override def linalg_lu(a: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext): (Rep[DenseMatrix[Double]],Rep[DenseMatrix[Double]],Rep[DenseMatrix[Int]]) = {
    if (useLAPACK) {
      val piv_len = if (a.numRows < a.numCols) a.numRows else a.numCols
      val ipiv = DenseVector[Int](piv_len, unit(false))
      val alloc = a.mutable
      reflectWrite(alloc,ipiv)(Native_lu(alloc, ipiv))
      postprocess_lu(alloc, ipiv)
    }
    else super.linalg_lu(a)
  }

  override def linalg_chol(a: Rep[DenseMatrix[Double]], tri: Rep[String])(implicit __pos: SourceContext): Rep[DenseMatrix[Double]] = {
    if (useLAPACK) {
      check_chol(a,tri)

      val t = if (ordering2___equal(tri, unit("upper"))) unit('U') else unit('L')
      val alloc = a.mutable
      reflectWrite(alloc)(Native_chol(alloc, t))
      postprocess_chol(alloc, tri)
    }
    else super.linalg_chol(a, tri)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Native_linsolve(a,b) => reflectPure(new { override val original = Some(f,e) } with Native_linsolve(f(a),f(b))(pos))(mtype(manifest[A]),pos)
    case e@Native_lu(a,p) => reflectPure(new { override val original = Some(f,e) } with Native_lu(f(a),f(p))(pos))(mtype(manifest[A]),pos)
    case e@Native_chol(a,t) => reflectPure(new { override val original = Some(f,e) } with Native_chol(f(a),f(t))(pos))(mtype(manifest[A]),pos)

    case Reflect(e@Native_linsolve(a,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Native_linsolve(f(a),f(b))(pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@Native_lu(a,p), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Native_lu(f(a),f(p))(pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@Native_chol(a,t), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Native_chol(f(a),f(t))(pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait CudaGenLAPACKOps
trait OpenCLGenLAPACKOps
trait CGenLAPACKOps

trait ScalaGenLAPACKOps extends ScalaGenExternalBase {
  val IR: LAPACKOpsExp with OptiLAExp
  import IR._

  /**
   * JNI implementation
   */
  override def emitExternalNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Native_linsolve(a,b) =>
      val args = scala.List("%1$s._data", "%2$s._data", "%1$s._numRows", "%1$s._numCols")
                 .map { _.format(quote(a), quote(b)) }
      emitMethodCall(sym, e, LAPACK, args)

    case e@Native_lu(a,ipiv) =>
      val args = scala.List("%1$s._data", "%2$s._data", "%1$s._numRows", "%1$s._numCols")
                 .map { _.format(quote(a), quote(ipiv)) }
      emitMethodCall(sym, e, LAPACK, args)

    case e@Native_chol(a,t) =>
      val args = scala.List("%1$s._data", "%1$s._numRows", "%1$s._numCols", "%2$s")
                 .map { _.format(quote(a), quote(t)) }
      emitMethodCall(sym, e, LAPACK, args)

    case _ => super.emitExternalNode(sym,rhs)
  }

  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
    case e@Native_linsolve(a,b) =>
      val tp = "Double"
      emitInterfaceAndMethod(LAPACK, e.funcName,
        scala.List("A:Array[%1$s]", "b:Array[%1$s]", "M:Int", "N:Int") map { _.format(tp) },
        scala.List("j%1$sArray A", "j%1$sArray b", "jint M", "jint N") map { _.format(tp.toLowerCase) },
        """
        {
          jboolean copy;
          j%1$s *A_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)A, &copy));
          j%1$s *b_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)b, &copy));

          lapack_int m = M, n = N, nrhs = 1, lda = N, ldb = 1, info;

          info = LAPACKE_dgels(LAPACK_ROW_MAJOR, 'N', m, n, nrhs, A_ptr, lda, b_ptr, ldb);

          (*env)->ReleasePrimitiveArrayCritical(env, A, A_ptr, 0);
          (*env)->ReleasePrimitiveArrayCritical(env, b, b_ptr, 0);
        }""".format(tp.toLowerCase))

    case e@Native_lu(a,p) =>
      val tp = "Double"
      emitInterfaceAndMethod(LAPACK, e.funcName,
        scala.List("A:Array[%1$s]", "ipiv:Array[Int]", "M:Int", "N:Int") map { _.format(tp) },
        scala.List("j%1$sArray A", "jintArray ipiv", "jint M", "jint N") map { _.format(tp.toLowerCase) },
        """
        {
          jboolean copy;
          j%1$s *A_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)A, &copy));
          jint *ipiv_ptr = (jint*)((*env)->GetPrimitiveArrayCritical(env, (jarray)ipiv, &copy));

          lapack_int m = M, n = N, lda = N, info;

          info = LAPACKE_dgetrf(LAPACK_ROW_MAJOR, m, n, A_ptr, lda, ipiv_ptr);

          (*env)->ReleasePrimitiveArrayCritical(env, A, A_ptr, 0);
          (*env)->ReleasePrimitiveArrayCritical(env, ipiv, ipiv_ptr, 0);
        }""".format(tp.toLowerCase))


    case e@Native_chol(a,t) =>
      val tp = "Double"
      emitInterfaceAndMethod(LAPACK, e.funcName,
        scala.List("A:Array[%1$s]", "M:Int", "N:Int", "tri:Char") map { _.format(tp) },
        scala.List("j%1$sArray A", "jint M", "jint N", "jchar tri") map { _.format(tp.toLowerCase) },
        """
        {
          jboolean copy;
          j%1$s *A_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)A, &copy));

          lapack_int n = N, lda = N, info;

          info = LAPACKE_dpotrf(LAPACK_ROW_MAJOR, tri, n, A_ptr, lda);

          (*env)->ReleasePrimitiveArrayCritical(env, A, A_ptr, 0);
        }""".format(tp.toLowerCase))

    case _ => super.emitExternalLib(rhs)
  }
}

