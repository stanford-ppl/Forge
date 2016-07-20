package dhdl.graph.traversal
import dhdl.graph._
import dhdl._
import dhdl.PIRMisc._

import scala.collection.mutable.Set
import scala.collection.mutable.HashMap

class ForwardRef(implicit val design: Design) extends Traversal{

  private val nameMap = HashMap[String, Node]()

  override def reset = nameMap.clear()
  override def traverse = {
    design.allNodes.foreach(n => addName(n))
    design.toUpdate.foreach { case (k,f) =>
      val n:Node = getByName(k)
      f(n)
    }
    design.toUpdate.clear()
    design.allNodes.foreach(n => assert(!n.toUpdate, s"Node ${n} contains unupdated field/fields!"))
  } 

  override def finPass = {
    info("Finishing updating forward referenced nodes")
  }

  def addName(n:Node):Unit = if (n.name.isDefined) {
    n match {
      case c:Controller => 
        val s = n.name.get  
        assert(!nameMap.contains(s), s"Already create controller with name ${s}: ${n}")
        nameMap += (s -> c)
      case p:Primitive =>
        assert(p.ctrler!=null, s"Primitive ${p} doesn't have ctriler!")
        val s = s"${p.ctrler}_${n.name.get}"
        assert(!nameMap.contains(s),
          s"Already create primitive with name ${s} for controller ${p.ctrler}")
        nameMap += (s -> p)
      case w:Port =>
        //assert(false, "No support for adding name for wire yet!")
    }
  }

  def getByName(s:String):Node = {
    assert(nameMap.contains(s), s"No node defined with name:${s}. nameMap:${nameMap}")
    nameMap(s)
  }

}
