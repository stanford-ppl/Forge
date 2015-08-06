package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait ControlOps extends Base {
  this: OptiML => 

  def untilconverged[T:Typ](x: Rep[T],tol: Rep[Double] = unit(.001),minIter: Rep[Int] = unit(1),maxIter: Rep[Int] = unit(1000),verbose: Rep[Boolean] = unit(false))(block: (Rep[T],Rep[Int]) => Rep[T])(implicit __pos: SourceContext,diff: (Rep[T],Rep[T]) => Rep[Double]) = control_untilconverged[T](x,tol,minIter,maxIter,verbose,block)(implicitly[Typ[T]],__pos,diff)
  def untilconverged_withdiff[T:Typ](x: Rep[T],tol: Rep[Double] = unit(.001),minIter: Rep[Int] = unit(1),maxIter: Rep[Int] = unit(1000))(block: (Rep[T],Rep[Int]) => Rep[T])(diff: (Rep[T],Rep[T]) => Rep[Double])(implicit __pos: SourceContext): Rep[T] = { untilconverged(x, tol, minIter, maxIter)(block)(implicitly[Typ[T]], implicitly[SourceContext], diff) }
  def untilconverged_buffered[T:Bufferable:Typ](x: Rep[T],tol: Rep[Double] = unit(.001),minIter: Rep[Int] = unit(1),maxIter: Rep[Int] = unit(1000))(block: (Rep[T]) => Rep[T])(implicit __pos: SourceContext,diff: (Rep[T],Rep[T]) => Rep[Double]) = control_untilconverged_buffered[T](x,tol,minIter,maxIter,block)(implicitly[Bufferable[T]],implicitly[Typ[T]],__pos,diff)
  def getSocket(implicit __pos: SourceContext) = control_getsocket()(__pos)
  def getNumSockets(implicit __pos: SourceContext) = control_getnumsockets()(__pos)
  def replicate(__arg0: Rep[FactorGraph])(implicit __pos: SourceContext) = control_replicate(__arg0)(__pos)


  def control_untilconverged[T:Typ](x: Rep[T],tol: Rep[Double] = unit(.001),minIter: Rep[Int] = unit(1),maxIter: Rep[Int] = unit(1000),verbose: Rep[Boolean] = unit(false),block: (Rep[T],Rep[Int]) => Rep[T])(implicit __pos: SourceContext,diff: (Rep[T],Rep[T]) => Rep[Double]): Rep[T]
  def control_untilconverged_buffered[T:Bufferable:Typ](x: Rep[T],tol: Rep[Double] = unit(.001),minIter: Rep[Int] = unit(1),maxIter: Rep[Int] = unit(1000),block: (Rep[T]) => Rep[T])(implicit __pos: SourceContext,diff: (Rep[T],Rep[T]) => Rep[Double]): Rep[T]
  def control_getsocket()(implicit __pos: SourceContext): Rep[Int]
  def control_getnumsockets()(implicit __pos: SourceContext): Rep[Int]
  def control_replicate(__arg0: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[Replicated[FactorGraph]]
}
