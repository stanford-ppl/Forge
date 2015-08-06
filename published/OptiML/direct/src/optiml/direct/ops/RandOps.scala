package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait RandOps extends Base {
  this: OptiML => 

  def random[A:Typ](implicit __pos: SourceContext) = rand_random[A]()(implicitly[Typ[A]],__pos)
  def randomElem[A:Typ](__arg0: Rep[DenseVector[A]])(implicit __pos: SourceContext) = rand_randomelem[A](__arg0)(implicitly[Typ[A]],__pos)
  def randomInt(__arg0: Rep[Int])(implicit __pos: SourceContext) = rand_randomint(__arg0)(__pos)
  def randomGaussian()(implicit __pos: SourceContext) = rand_randomgaussian()(__pos)
  def reseed()(implicit __pos: SourceContext) = rand_reseed()(__pos)
  def shuffle(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload1) = rand_shuffle(__arg0)(__pos,overload1)
  def shuffle[A:Typ](__arg0: Rep[DenseVector[A]])(implicit __pos: SourceContext,__imp1: Overload2) = rand_shuffle[A](__arg0)(implicitly[Typ[A]],__pos,overload2)
  def shuffle[A:Typ](__arg0: Rep[DenseMatrix[A]])(implicit __pos: SourceContext,__imp1: Overload3) = rand_shuffle[A](__arg0)(implicitly[Typ[A]],__pos,overload3)
  def sample(v: Rep[IndexVector],pct: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload1) = rand_sample(v,pct)(__pos,overload1)
  def sample[A:Typ](v: Rep[DenseVector[A]],pct: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload2) = rand_sample[A](v,pct)(implicitly[Typ[A]],__pos,overload2)
  def sample[A:Typ](m: Rep[DenseMatrix[A]],pct: Rep[Double],sampleRows: Rep[Boolean] = unit(true))(implicit __pos: SourceContext,__imp1: Overload3) = rand_sample[A](m,pct,sampleRows)(implicitly[Typ[A]],__pos,overload3)


  def rand_random[A:Typ]()(implicit __pos: SourceContext): Rep[A]
  def rand_randomelem[A:Typ](__arg0: Rep[DenseVector[A]])(implicit __pos: SourceContext): Rep[A]
  def rand_randomint(__arg0: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def rand_randomgaussian()(implicit __pos: SourceContext): Rep[Double]
  def rand_reseed()(implicit __pos: SourceContext): Rep[Unit]
  def rand_shuffle(__arg0: Rep[IndexVector])(implicit __pos: SourceContext,__imp1: Overload1): Rep[IndexVector]
  def rand_shuffle[A:Typ](__arg0: Rep[DenseVector[A]])(implicit __pos: SourceContext,__imp1: Overload2): Rep[DenseVector[A]]
  def rand_shuffle[A:Typ](__arg0: Rep[DenseMatrix[A]])(implicit __pos: SourceContext,__imp1: Overload3): Rep[DenseMatrix[A]]
  def rand_sample(v: Rep[IndexVector],pct: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload1): Rep[IndexVector]
  def rand_sample[A:Typ](v: Rep[DenseVector[A]],pct: Rep[Double])(implicit __pos: SourceContext,__imp1: Overload2): Rep[DenseVector[A]]
  def rand_sample[A:Typ](m: Rep[DenseMatrix[A]],pct: Rep[Double],sampleRows: Rep[Boolean] = unit(true))(implicit __pos: SourceContext,__imp1: Overload3): Rep[DenseMatrix[A]]
}
trait RandCompilerOps extends RandOps {
  this: OptiML => 

  def optila_rand_double()(implicit __pos: SourceContext): Rep[Double]
  def optila_rand_float()(implicit __pos: SourceContext): Rep[Float]
  def optila_rand_int()(implicit __pos: SourceContext): Rep[Int]
  def optila_rand_boolean()(implicit __pos: SourceContext): Rep[Boolean]
  def optila_shuffle_array[A:Typ](__arg0: Rep[ForgeArray[A]])(implicit __pos: SourceContext): Rep[ForgeArray[A]]
}

