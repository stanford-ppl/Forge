package dhdl.graph

import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.math.max
import dhdl.Design
import dhdl.graph._

class Range (s:Port, e:Port) {
  val start:Port = s
  val end:Port = e
  def by(step:Port) = (start, end, step)
}

/**
 * A type representing a group of wires in pir
 */
trait Port extends Node {
  def width(implicit design:Design) = design.arch.wordWidth
  def by(step:Port) (implicit design:Design) = (Const(0l, step.width), this, step)
  def until(max:Port) = new Range(this, max)
  def isConst = this.isInstanceOf[Const] 
  def copy(implicit design: Design) = this 
}

trait Const extends Port {
  val value:Long
  override def toString = s"Const(${value})"
  override def copy(implicit design: Design) = Const(name, value, Some(width))
}
object Const {
  def apply(nameStr:Option[String], v:Long, w:Option[Int])(implicit design: Design) =
   new {
     override val value = v
   } with Node(nameStr, "Const") with Const {
     override def width(implicit design:Design) = w.getOrElse(design.arch.wordWidth) 
   }
  def apply(v:Long, w:Int) (implicit design:Design):Const = Const(None, v, Some(w))
  def apply(v:Long) (implicit design:Design):Const = Const(None, v, None)
  def apply(name:String, v:Long) (implicit design:Design):Const = Const(Some(name), v, None)
}

