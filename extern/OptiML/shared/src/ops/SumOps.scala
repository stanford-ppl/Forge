package optiml.shared.ops

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import optiml.shared._
import optiml.shared.ops._
import optiml.shared.typeclass._

/**
 * Sums are externs now because they're complicated. Can we simplify them?
 */

trait CanSumOps extends Base {
  this: OptiML =>

  // CanSum allows the sum to be implemented efficiently by mutating the accumulator instead of creating a new one each iteration
  trait CanSum[R,A] {
    def accA(acc: Rep[R], y: Rep[A])(implicit ctx: SourceContext): Rep[R]
    def accR(acc: Rep[R], y: Rep[R])(implicit ctx: SourceContext): Rep[R]
    def mutableA(lhs: Rep[A])(implicit ctx: SourceContext): Rep[R]
    def mutableR(lhs: Rep[R])(implicit ctx: SourceContext): Rep[R]
  }

  def cstype[R,A,RB,B](x: CanSum[R,A]) = x.asInstanceOf[CanSum[RB,B]]

  implicit def canSumView[A:Manifest:Arith]: CanSum[DenseVector[A],DenseVectorView[A]] = new CanSum[DenseVector[A],DenseVectorView[A]] {
    def accA(acc: Rep[DenseVector[A]], y: Rep[DenseVectorView[A]])(implicit ctx: SourceContext) = { densevector_pleq(acc,y); acc }
    def accR(acc: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = { densevector_pleq(acc,y); acc }
    def mutableA(lhs: Rep[DenseVectorView[A]])(implicit ctx: SourceContext) = lhs.toDense.mutable
    def mutableR(lhs: Rep[DenseVector[A]])(implicit ctx: SourceContext) = densevector_mutable(lhs)
  }

  implicit def canSumDenseVector[A:Manifest:Arith]: CanSum[DenseVector[A],DenseVector[A]] = new CanSum[DenseVector[A],DenseVector[A]] {
    def accA(acc: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = { densevector_pleq(acc,y); acc }
    def accR(acc: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = { densevector_pleq(acc,y); acc }
    def mutableA(lhs: Rep[DenseVector[A]])(implicit ctx: SourceContext) = densevector_mutable(lhs)
    def mutableR(lhs: Rep[DenseVector[A]])(implicit ctx: SourceContext) = densevector_mutable(lhs)
  }

  implicit def canSumDenseMatrix[A:Manifest:Arith]: CanSum[DenseMatrix[A],DenseMatrix[A]] = new CanSum[DenseMatrix[A],DenseMatrix[A]] {
    def accA(acc: Rep[DenseMatrix[A]], y: Rep[DenseMatrix[A]])(implicit ctx: SourceContext) = { densematrix_pleq(acc,y); acc }
    def accR(acc: Rep[DenseMatrix[A]], y: Rep[DenseMatrix[A]])(implicit ctx: SourceContext) = { densematrix_pleq(acc,y); acc }
    def mutableA(lhs: Rep[DenseMatrix[A]])(implicit ctx: SourceContext) = densematrix_mutable(lhs)
    def mutableR(lhs: Rep[DenseMatrix[A]])(implicit ctx: SourceContext) = densematrix_mutable(lhs)
  }

  implicit def canSumNumeric[A:Manifest:Numeric:Arith]: CanSum[A,A] = new CanSum[A,A] {
    def accA(acc: Rep[A], y: Rep[A])(implicit ctx: SourceContext) = acc + y
    def accR(acc: Rep[A], y: Rep[A])(implicit ctx: SourceContext) = acc + y
    def mutableA(lhs: Rep[A])(implicit ctx: SourceContext) = lhs
    def mutableR(lhs: Rep[A])(implicit ctx: SourceContext) = lhs
  }
}

trait SumOps extends CanSumOps {
  this: OptiML =>

  def sum[A:Manifest:Arith](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[A])(implicit cs: CanSum[A,A], ctx: SourceContext) = optiml_sum(start, end, block)
  // sumRows currently just re-uses sumIf implementation; check if the condition always being true is actually slower than a specialized implementation with no conditional at all
  def sumRows[A:Manifest:Arith](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[DenseVectorView[A]])(implicit cs: CanSum[A,A], ctx: SourceContext) = optiml_sumif[DenseVector[A],DenseVectorView[A]](start,end,i => unit(true),block)
  def sumIf[A:Manifest:Arith](start: Rep[Int], end: Rep[Int])(cond: Rep[Int] => Rep[Boolean])(block: Rep[Int] => Rep[A])(implicit cs: CanSum[A,A], ctx: SourceContext) = optiml_sumif[A,A](start,end,cond,block)
  def sumRowsIf[A:Manifest:Arith](start: Rep[Int], end: Rep[Int])(cond: Rep[Int] => Rep[Boolean])(block: Rep[Int] => Rep[DenseVectorView[A]])(implicit cs: CanSum[A,A], ctx: SourceContext) = optiml_sumif[DenseVector[A],DenseVectorView[A]](start,end,cond,block)

  def optiml_sum[A:Manifest:Arith](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A])(implicit cs: CanSum[A,A], ctx: SourceContext): Rep[A]
  def optiml_sumif[R:Manifest:Arith,A:Manifest](start: Rep[Int], end: Rep[Int], cond: Rep[Int] => Rep[Boolean], block: Rep[Int] => Rep[A])(implicit cs: CanSum[R,A], ctx: SourceContext): Rep[R]
}

trait SumCompilerOps extends SumOps { this: OptiML => }
