package dhdl.graph

import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.math.max
import dhdl.Design
import dhdl.graph._

class Range (s:Wire, e:Wire) {
  val start:Wire = s
  val end:Wire = e
  def by(step:Wire) = (start, end, step)
}

/**
 * An unassigned wire used to represent scalar outputs prior to assignment
 * @param tp: The type of value represented by this wire
 */
trait Wire extends Node {
  val width = Node.DefaultPrecision //TODO 
  def by(step:Wire) (implicit design:Design) = (Const(0l, step.width), this, step)
  def until(max:Wire) = new Range(this, max)
  def isConst = this.isInstanceOf[Const] 
  def copy(implicit design: Design) = this 
}

trait Const extends Wire {
  val value:Long
  override def toString = s"Const(${value})"
  override def copy(implicit design: Design) = Const(name, value, Some(width))
}
object Const {
  def apply(nameStr:Option[String], v:Long, w:Option[Int])(implicit design: Design) =
   new {
     override val value = v
     override val width = w.getOrElse(Node.DefaultPrecision) 
   } with Node(nameStr, "Const") with Const
  def apply(v:Long, w:Int) (implicit design:Design):Const = Const(None, v, Some(w))
  def apply(v:Long) (implicit design:Design):Const = Const(None, v, None)
  def apply(name:String, v:Long) (implicit design:Design):Const = Const(Some(name), v, None)
}

