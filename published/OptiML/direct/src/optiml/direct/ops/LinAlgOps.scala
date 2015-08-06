package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait LinAlgOps extends Base {
  this: OptiML => 

  def lu(__arg0: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext) = linalg_lu(__arg0)(__pos)
  def chol(A: Rep[DenseMatrix[Double]],tri: Rep[String] = unit("upper"))(implicit __pos: SourceContext) = linalg_chol(A,tri)(__pos)
  def det[T:Arith:Numeric:Typ](x: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = linalg_det[T](x)(implicitly[Arith[T]],implicitly[Numeric[T]],implicitly[Typ[T]],__pos)

  implicit def repToLinAlgDenseMatrixDoubleOpsCls(x: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext) = new LinAlgDenseMatrixDoubleOpsCls(x)(__pos)
  implicit def varToLinAlgDenseMatrixDoubleOpsCls(x: Var[DenseMatrix[Double]])(implicit __pos: SourceContext) = new LinAlgDenseMatrixDoubleOpsCls(readVar(x))(__pos)

  class LinAlgDenseMatrixDoubleOpsCls(val self: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext) {
    def \(__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext) = linsolve(self,__arg1)(__pos)
  }



  def linsolve(__arg0: Rep[DenseMatrix[Double]],__arg1: Rep[DenseVector[Double]])(implicit __pos: SourceContext): Rep[DenseVector[Double]]
  def linalg_lu(__arg0: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext): Tuple3[Rep[DenseMatrix[Double]],Rep[DenseMatrix[Double]],Rep[DenseMatrix[Int]]]
  def linalg_chol(A: Rep[DenseMatrix[Double]],tri: Rep[String] = unit("upper"))(implicit __pos: SourceContext): Rep[DenseMatrix[Double]]
  def linalg_det[T:Arith:Numeric:Typ](x: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[T]
}
trait LinAlgCompilerOps extends LinAlgOps {
  this: OptiML => 

  def densematrix_determinant_22[T:Arith:Numeric:Typ](x: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[T]
  def densematrix_determinant_33[T:Arith:Numeric:Typ](x: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[T]
  def densematrix_determinant_44[T:Arith:Numeric:Typ](x: Rep[DenseMatrix[T]])(implicit __pos: SourceContext): Rep[T]
}

