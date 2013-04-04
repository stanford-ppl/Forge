package optiwrangle.library.extern

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import optiwrangle.library.OptiWrangleBase

trait VarWrapper extends OptiWrangleBase {
  type Var[+T] = Variable[T @uncheckedVariance]  
  
  class Variable[T:Manifest](elem: T) {
    private var _elem = elem
    
    def get() = _elem
    def set(x: T) = _elem = x
  }  
  
  implicit def readVar[T:Manifest](v: Var[T])(implicit pos: SourceContext): Rep[T] = v.get()
  def implicit_convert[X,Y](x: Rep[X])(implicit c: X => Y, mX: Manifest[X], mY: Manifest[Y], pos: SourceContext): Rep[Y] = x.asInstanceOf[Rep[Y]]
  
  def var_new[T:Manifest](init: Rep[T])(implicit pos: SourceContext): Var[T] = new Variable(init)
  def var_assign[T:Manifest](lhs: Var[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Unit] = lhs.set(rhs)
  
  // the Variables interface is kind of broken..
  // TODO: need this to work on other numerics
  def var_plusequals[T:Manifest](lhs: Var[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Unit] = lhs.set((lhs.get.asInstanceOf[Int] + rhs.asInstanceOf[Int]).asInstanceOf[T])
  def var_minusequals[T:Manifest](lhs: Var[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Unit] = lhs.set((lhs.get.asInstanceOf[Int] - rhs.asInstanceOf[Int]).asInstanceOf[T])
  def var_timesequals[T:Manifest](lhs: Var[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Unit] = lhs.set((lhs.get.asInstanceOf[Int] * rhs.asInstanceOf[Int]).asInstanceOf[T])
  def var_divideequals[T:Manifest](lhs: Var[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Unit] = lhs.set((lhs.get.asInstanceOf[Int] / rhs.asInstanceOf[Int]).asInstanceOf[T])  
}




