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


trait LAPACKOpsExp extends LAPACKOps with LinAlgOpsExp {
  this: OptiLAExp =>

  /**
   * Intercept LAPACK-able operations
   */
  case class Native_linsolve(a: Rep[DenseMatrix[Double]],b: Rep[DenseVector[Double]])(implicit val __pos: SourceContext) extends DeliteOpExternal[DenseVector[Double]] {
    override def inputs = scala.List(a,b)
    def alloc = b
    val funcName = "linsolve"
  }

  // TODO: we also want an AX = B version where X and B are matrices
  override def linsolve(a: Rep[DenseMatrix[Double]], b: Rep[DenseVector[Double]])(implicit __pos: SourceContext): Rep[DenseVector[Double]] = {
    if (useLAPACK) {
      // a, b will be overwritten with answers
      val out = reflectPure(Native_linsolve(a.mutable,b.mutable))
      out.take(a.numCols) // answer is written in the first n entries of b
    }
    else super.linsolve(a,b)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Native_linsolve(a,b) => reflectPure(new { override val original = Some(f,e) } with Native_linsolve(f(a),f(b))(pos))(mtype(manifest[A]),pos)

    case Reflect(e@Native_linsolve(a,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Native_linsolve(f(a),f(b))(pos), mapOver(f,u), f(es)))(mtype(manifest[A]))
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
      emitMethodCall(sym, e, MKL, args)
    case _ => super.emitExternalNode(sym,rhs)
  }

  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {

    case e@Native_linsolve(a,b) =>
      val tp = "Double"
      emitInterfaceAndMethod(MKL, e.funcName,
        scala.List("A:Array[%1$s]", "b:Array[%1$s]", "M:Int", "N:Int") map { _.format(tp) },
        scala.List("j%1$sArray A", "j%1$sArray b", "jint M", "jint N") map { _.format(tp.toLowerCase) },
        """
        {
          jboolean copy;

          j%1$s *A_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)A, &copy));
          j%1$s *b_ptr = (j%1$s*)((*env)->GetPrimitiveArrayCritical(env, (jarray)b, &copy));

          MKL_INT m = M, n = N, nrhs = 1, lda = N, ldb = 1, info;

          info = LAPACKE_dgels(LAPACK_ROW_MAJOR, 'N', m, n, nrhs, A_ptr, lda, b_ptr, ldb);

          (*env)->ReleasePrimitiveArrayCritical(env, A, A_ptr, 0);
          (*env)->ReleasePrimitiveArrayCritical(env, b, b_ptr, 0);
        }""".format(tp.toLowerCase))

    case _ => super.emitExternalLib(rhs)
  }
}

