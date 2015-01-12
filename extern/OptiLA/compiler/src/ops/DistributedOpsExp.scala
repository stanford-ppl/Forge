package optila.compiler.ops

import scala.reflect.{Manifest,RefinedManifest,SourceContext}
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp,StructOps}
import ppl.delite.framework.ops.{DeliteCollection, DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastructures.{DeliteStructsExp,ScalaGenDeliteStruct}

import optila.shared._
import optila.shared.ops._
import optila.shared.typeclass._
import optila.compiler._
import optila.compiler.ops._


trait DistributedOpsExp extends DistributedOps with DenseMatrixOpsExp {
  this: OptiLAExp =>

  // This is used when we to distribute a parallel op over a matrix; the size passed in is the total matrix size
  // Each matrix chunk should allocate a smaller number of rows, depending on the total chunk size, while numCols is constant.
  // Is there any reason this shouldn't be the default implementation for densematrix_dc_alloc?
  // override def densematrix_dc_alloc[T:Manifest,R:Manifest](self: Rep[DenseMatrix[T]],__arg1: Rep[Int])(implicit __pos: SourceContext) = {
  //   if (ppl.delite.framework.Config.generateSerializable) {
  //     val numRows = __arg1 / self.numCols
  //     DenseMatrix[R](numRows, self.numCols)
  //   }
  //   else super.densematrix_dc_alloc[T,R](self,__arg1)
  // }
  override def densematrix_dc_alloc[R:Manifest,CR:Manifest](self: Rep[CR],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseMatrix[R]] = {
    if (ppl.delite.framework.Config.generateSerializable) {
      val simpleName = manifest[CR].erasure.getSimpleName
      val numCols = simpleName match {
        case s if s.startsWith("DenseMatrixView") =>
          densematrixview_numcols(self.asInstanceOf[Rep[DenseMatrixView[Any]]])
        case s if s.startsWith("DenseMatrix") =>
          densematrix_numcols(self.asInstanceOf[Rep[DenseMatrix[Any]]])
      }
      val numRows = __arg1 / numCols
      DenseMatrix[R](numRows, numCols)
    }
    else super.densematrix_dc_alloc[R,CR](self,__arg1)
  }
}

trait ScalaGenDistributedOps extends ScalaGenDeliteStruct {
  val IR: DenseMatrixOpsExp with DeliteOpsExp
  import IR._

  // We only support partitioning DenseMatrices over rows, so when we recombine them, we only add rows and the underlying data field together.
  override def dc_combine[A](structType: Manifest[A], elems: Seq[(String,Manifest[_])], prefixL: String, prefixR: String): Seq[(String,Manifest[_],String)] = {
    if (isDenseMatrixTpe(structType)) {
      elems map { case (field,tp) =>
        val lhs = prefixL + "." + field
        val rhs = prefixR + "." + field
        val newVal = field match {
          case "_numRows" => lhs + " + " + rhs
          case "_numCols" => lhs
          case "_data" => delite_array_combine(tp, lhs, rhs)
        }
        (field,tp,newVal)
      }
    }
    else super.dc_combine(structType, elems, prefixL, prefixR)
  }
}
