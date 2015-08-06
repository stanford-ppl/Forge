package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait BasicMathOps extends Base {
  this: OptiML => 

  def abs(__arg0: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload1): Rep[Double] = { Math.abs(__arg0) }
  def exp(__arg0: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload1): Rep[Double] = { Math.exp(__arg0) }
  def log(__arg0: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload1): Rep[Double] = { Math.log(__arg0) }
  def log10(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.log10(__arg0) }
  def square(__arg0: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload1): Rep[Double] = { __arg0*__arg0 }
  def sqrt(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.sqrt(__arg0) }
  def ceil(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Int] = { Math.ceil(__arg0).toInt }
  def floor(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Int] = { Math.floor(__arg0).toInt }
  def round(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Int] = { Math.round(__arg0).toInt }
  def sin(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.sin(__arg0) }
  def sinh(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.sinh(__arg0) }
  def asin(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.asin(__arg0) }
  def cos(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.cos(__arg0) }
  def cosh(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.cosh(__arg0) }
  def acos(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.acos(__arg0) }
  def tan(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.tan(__arg0) }
  def tanh(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.tanh(__arg0) }
  def atan(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.atan(__arg0) }
  def atan2(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.atan2(__arg0,__arg1) }
  def pow(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext): Rep[Double] = { Math.pow(__arg0,__arg1) }
  def max[T:Numeric:Typ](__arg0: Rep[T],__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload1) = basicmath_max[T](__arg0,__arg1)(implicitly[Numeric[T]],implicitly[Typ[T]],__pos,overload1)
  def min[T:Numeric:Typ](__arg0: Rep[T],__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload1) = basicmath_min[T](__arg0,__arg1)(implicitly[Numeric[T]],implicitly[Typ[T]],__pos,overload1)
  def normalize(__arg0: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload1): Rep[DenseVector[Double]] = { normalize(__arg0, Std) }
  def normalize(__arg0: Rep[DenseVector[Double]],__arg1: NormalizeMethod)(implicit __pos: SourceContext,__imp1: Overload2) = basicmath_normalize(__arg0,__arg1)(__pos,overload2)
  def normalize(__arg0: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload3): Rep[DenseMatrix[Double]] = { normalize(__arg0, Std) }
  def normalize(__arg0: Rep[DenseMatrix[Double]],__arg1: NormalizeMethod)(implicit __pos: SourceContext,__imp1: Overload4) = basicmath_normalize(__arg0,__arg1)(__pos,overload4)
  def sigmoid(__arg0: Rep[Double])(implicit __pos: SourceContext) = basicmath_sigmoid(__arg0)(__pos)
  def normpdf(x: Rep[Double],mu: Rep[Double],sigma: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload1) = basicmath_normpdf(x,mu,sigma)(__pos,overload1)
  def normpdf(x: Rep[DenseVector[Double]],mu: Rep[DenseVector[Double]],sigma: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: Overload2) = basicmath_normpdf(x,mu,sigma)(__pos,overload2)
  def normpdf(x: Rep[DenseMatrix[Double]],mu: Rep[DenseMatrix[Double]],sigma: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: Overload3) = basicmath_normpdf(x,mu,sigma)(__pos,overload3)
  def normpdf(x: Rep[DenseVector[Double]],mu: Rep[Double],sigma: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: ROverload4): Rep[DenseVector[Double]] = { x.zip(sigma) { (a,b) => normpdf(a, mu, b) } }
  def normpdf(x: Rep[DenseVector[Double]],mu: Rep[DenseVector[Double]],sigma: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload5): Rep[DenseVector[Double]] = { x.zip(mu) { (a,b) => normpdf(a, b, sigma) } }
  def normpdf(x: Rep[DenseVector[Double]],mu: Rep[Double],sigma: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload6): Rep[DenseVector[Double]] = { x.map { e => normpdf(e, mu, sigma) } }
  def normpdf(x: Rep[DenseMatrix[Double]],mu: Rep[Double],sigma: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: ROverload7): Rep[DenseMatrix[Double]] = { x.zip(sigma) { (a,b) => normpdf(a, mu, b) } }
  def normpdf(x: Rep[DenseMatrix[Double]],mu: Rep[DenseMatrix[Double]],sigma: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload8): Rep[DenseMatrix[Double]] = { x.zip(mu) { (a,b) => normpdf(a, b, sigma) } }
  def normpdf(x: Rep[DenseMatrix[Double]],mu: Rep[Double],sigma: Rep[Double])(implicit __pos: SourceContext,__imp1: ROverload9): Rep[DenseMatrix[Double]] = { x.map { e => normpdf(e, mu, sigma) } }
  def abs[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload2): Rep[DenseVector[T]] = { __arg0.abs }
  def exp[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload2): Rep[DenseVector[T]] = { __arg0.exp }
  def log[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload2): Rep[DenseVector[T]] = { __arg0.log }
  def square[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2) = basicmath_square[T](__arg0)(implicitly[Typ[T]],__pos,__imp0,overload2)
  def sum[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload1): Rep[T] = { __arg0.sum }
  def prod[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload1): Rep[T] = { __arg0.prod }
  def mean[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: ROverload1): Rep[Double] = { __arg0.mean }
  def variance[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload1) = basicmath_variance[T](__arg0)(implicitly[Typ[T]],__pos,conv,overload1)
  def stddev[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload1) = basicmath_stddev[T](__arg0)(implicitly[Typ[T]],__pos,conv,overload1)
  def min[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: ROverload2): Rep[T] = { __arg0.min }
  def max[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: ROverload2): Rep[T] = { __arg0.max }
  def median[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Numeric[T],__imp1: Ordering[T],__imp3: ROverload1): Rep[T] = { __arg0.median }
  def abs[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload3): Rep[DenseVector[T]] = { __arg0.abs }
  def exp[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload3): Rep[DenseVector[T]] = { __arg0.exp }
  def log[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload3): Rep[DenseVector[T]] = { __arg0.log }
  def square[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3) = basicmath_square[T](__arg0)(implicitly[Typ[T]],__pos,__imp0,overload3)
  def sum[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload2): Rep[T] = { __arg0.sum }
  def prod[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload2): Rep[T] = { __arg0.prod }
  def mean[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: ROverload2): Rep[Double] = { __arg0.mean }
  def variance[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload2) = basicmath_variance[T](__arg0)(implicitly[Typ[T]],__pos,conv,overload2)
  def stddev[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload2) = basicmath_stddev[T](__arg0)(implicitly[Typ[T]],__pos,conv,overload2)
  def min[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: ROverload3): Rep[T] = { __arg0.min }
  def max[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: ROverload3): Rep[T] = { __arg0.max }
  def abs(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload4): Rep[DenseVector[Int]] = { __arg0.abs }
  def exp(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload4): Rep[DenseVector[Int]] = { __arg0.exp }
  def log(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload4): Rep[DenseVector[Int]] = { __arg0.log }
  def square(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload4) = basicmath_square(__arg0)(__pos,overload4)
  def sum(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload3): Rep[Int] = { __arg0.sum }
  def prod(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload3): Rep[Int] = { __arg0.prod }
  def mean(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload3): Rep[Double] = { __arg0.mean }
  def variance(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload3) = basicmath_variance(__arg0)(__pos,overload3)
  def stddev(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload3) = basicmath_stddev(__arg0)(__pos,overload3)
  def min(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload4): Rep[Int] = { __arg0.min }
  def max(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: ROverload4): Rep[Int] = { __arg0.max }
  def abs[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload5): Rep[DenseMatrix[T]] = { __arg0.abs }
  def exp[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload5): Rep[DenseMatrix[T]] = { __arg0.exp }
  def log[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload5): Rep[DenseMatrix[T]] = { __arg0.log }
  def square[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5) = basicmath_square[T](__arg0)(implicitly[Typ[T]],__pos,__imp0,overload5)
  def sum[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload4): Rep[T] = { __arg0.sum }
  def prod[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload4): Rep[T] = { __arg0.prod }
  def mean[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: ROverload4): Rep[Double] = { __arg0.mean }
  def variance[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload4) = basicmath_variance[T](__arg0)(implicitly[Typ[T]],__pos,conv,overload4)
  def stddev[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload4) = basicmath_stddev[T](__arg0)(implicitly[Typ[T]],__pos,conv,overload4)
  def min[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: ROverload5): Rep[T] = { __arg0.min }
  def max[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: ROverload5): Rep[T] = { __arg0.max }
  def abs[T:Typ](__arg0: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload6): Rep[SparseVector[T]] = { __arg0.abs }
  def square[T:Typ](__arg0: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload6) = basicmath_square[T](__arg0)(implicitly[Typ[T]],__pos,__imp0,overload6)
  def sum[T:Typ](__arg0: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload5): Rep[T] = { __arg0.sum }
  def mean[T:Typ](__arg0: Rep[SparseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: ROverload5): Rep[Double] = { __arg0.mean }
  def min[T:Typ](__arg0: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: ROverload6): Rep[T] = { __arg0.min }
  def max[T:Typ](__arg0: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: ROverload6): Rep[T] = { __arg0.max }
  def abs[T:Typ](__arg0: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload7): Rep[SparseMatrix[T]] = { __arg0.abs }
  def square[T:Typ](__arg0: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload7) = basicmath_square[T](__arg0)(implicitly[Typ[T]],__pos,__imp0,overload7)
  def sum[T:Typ](__arg0: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: ROverload6): Rep[T] = { __arg0.sum }
  def mean[T:Typ](__arg0: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: ROverload6): Rep[Double] = { __arg0.mean }
  def min[T:Typ](__arg0: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: ROverload7): Rep[T] = { __arg0.min }
  def max[T:Typ](__arg0: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: ROverload7): Rep[T] = { __arg0.max }
  def mean[T:Typ](__arg0: Rep[T]*)(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload7) = basicmath_mean[T](__arg0)(implicitly[Typ[T]],__pos,conv,overload7)
  def min[T:Typ](__arg0: Rep[T]*)(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload8) = basicmath_min[T](__arg0)(implicitly[Typ[T]],__pos,__imp0,__imp1,overload8)
  def max[T:Typ](__arg0: Rep[T]*)(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload8) = basicmath_max[T](__arg0)(implicitly[Typ[T]],__pos,__imp0,__imp1,overload8)
  def median[T:Typ](__arg0: Rep[T]*)(implicit __pos: SourceContext,__imp0: Numeric[T],__imp1: Ordering[T],__imp3: Overload2) = basicmath_median[T](__arg0)(implicitly[Typ[T]],__pos,__imp0,__imp1,overload2)

  implicit def repToBasicMathDoubleOpsCls(x: Rep[Double])(implicit __pos: SourceContext) = new BasicMathDoubleOpsCls(x)(__pos)
  implicit def varToBasicMathDoubleOpsCls(x: Var[Double])(implicit __pos: SourceContext) = new BasicMathDoubleOpsCls(readVar(x))(__pos)

  class BasicMathDoubleOpsCls(val self: Rep[Double])(implicit __pos: SourceContext) {
    def ~^(__arg1: Rep[Int])(implicit __pos: SourceContext) = { Math.pow(self,__arg1) }
  }



  def basicmath_max[T:Numeric:Typ](__arg0: Rep[T],__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload1): Rep[T]
  def basicmath_min[T:Numeric:Typ](__arg0: Rep[T],__arg1: Rep[T])(implicit __pos: SourceContext,__imp1: Overload1): Rep[T]
  def basicmath_normalize(__arg0: Rep[DenseVector[Double]],__arg1: NormalizeMethod)(implicit __pos: SourceContext,__imp1: Overload2): Rep[DenseVector[Double]]
  def basicmath_normalize(__arg0: Rep[DenseMatrix[Double]],__arg1: NormalizeMethod)(implicit __pos: SourceContext,__imp1: Overload4): Rep[DenseMatrix[Double]]
  def basicmath_sigmoid(__arg0: Rep[Double])(implicit __pos: SourceContext): Rep[Double]
  def basicmath_normpdf(x: Rep[Double],mu: Rep[Double],sigma: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload1): Rep[Double]
  def basicmath_normpdf(x: Rep[DenseVector[Double]],mu: Rep[DenseVector[Double]],sigma: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: Overload2): Rep[DenseVector[Double]]
  def basicmath_normpdf(x: Rep[DenseMatrix[Double]],mu: Rep[DenseMatrix[Double]],sigma: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: Overload3): Rep[DenseMatrix[Double]]
  def basicmath_square[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload2): Rep[DenseVector[T]]
  def basicmath_variance[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload1): Rep[Double]
  def basicmath_stddev[T:Typ](__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload1): Rep[Double]
  def basicmath_square[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload3): Rep[DenseVector[T]]
  def basicmath_variance[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload2): Rep[Double]
  def basicmath_stddev[T:Typ](__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload2): Rep[Double]
  def basicmath_square(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload4): Rep[DenseVector[Int]]
  def basicmath_variance(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload3): Rep[Double]
  def basicmath_stddev(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload3): Rep[Double]
  def basicmath_square[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload5): Rep[DenseMatrix[T]]
  def basicmath_variance[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload4): Rep[Double]
  def basicmath_stddev[T:Typ](__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload4): Rep[Double]
  def basicmath_square[T:Typ](__arg0: Rep[SparseVector[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload6): Rep[SparseVector[T]]
  def basicmath_square[T:Typ](__arg0: Rep[SparseMatrix[T]])(implicit __pos: SourceContext,__imp0: Arith[T],__imp2: Overload7): Rep[SparseMatrix[T]]
  def basicmath_mean[T:Typ](__arg0: Seq[Rep[T]])(implicit __pos: SourceContext,conv: (Rep[T]) => Rep[Double],__imp2: Overload7): Rep[Double]
  def basicmath_min[T:Typ](__arg0: Seq[Rep[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload8): Rep[T]
  def basicmath_max[T:Typ](__arg0: Seq[Rep[T]])(implicit __pos: SourceContext,__imp0: Ordering[T],__imp1: HasMinMax[T],__imp3: Overload8): Rep[T]
  def basicmath_median[T:Typ](__arg0: Seq[Rep[T]])(implicit __pos: SourceContext,__imp0: Numeric[T],__imp1: Ordering[T],__imp3: Overload2): Rep[T]
}
