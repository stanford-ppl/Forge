package dhdl.graph

import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.math.max
import dhdl.Design
import dhdl.graph._

/** Base class for all PIR nodes. 
  * @param name: optional user name for a node 
  * @param typeStr: Consice name for a type of node for printing purpose 
  */
class Node(val name:Option[String], val typeStr:String) (implicit design: Design) { 
	design.addNode(this)
  val id : Int = design.nextId // Unique id for each node
  var toUpdate:Boolean = false // Whether fields of the node is not yet defined, 
                               // which would be updated later. Debug purpose only 

  override def equals(that: Any) = that match {
    case n: Node => super.equals(that) && id == n.id
    case _ => super.equals(that)
  }
  override def toString = s"${typeStr}${id}${if(name.isDefined) "_" else ""}${name.getOrElse("")}" 
}
object Node {
}
