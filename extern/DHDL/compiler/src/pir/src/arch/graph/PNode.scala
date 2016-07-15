package dhdl.plasticine.graph

import dhdl.graph._

import scala.collection.mutable.ListBuffer

class PNode { 
  val id : Int = PNode.nextId
}
object PNode {
  var nextSym = 0
  def nextId = {val temp = nextSym; nextSym +=1; temp}
}

trait RegMapping {
  var preg:PReg = _
  def mapTo(r:PReg) = {
    preg = r
    r.map(this)
  }
}

case class PReg() extends PNode{
  val mapping = ListBuffer[RegMapping]()
  def map(n:RegMapping) = {
    if (!mapping.contains(n))
      mapping += n
  }
}

case class PSRAM(numPort:Int) extends PNode with RegMapping{
}
case class PCounter() extends PNode with RegMapping{
}

case class Port() extends PNode with RegMapping {
}
sealed trait PortType
trait InPort extends PortType
object InPort {
  def apply() = {
    new Port() with InPort
  }
}
trait OutPort extends PortType
object OutPort {
  def apply() = {
    new Port() with OutPort
  }
}

case class PComputeUnit(regs:List[PReg], srams:List[PSRAM], ctrs:List[PCounter], 
  inports:List[InPort], outports:List[InPort]) extends PNode{
}

trait PMemoryController {
}
object PMemoryController {
  def apply(regs:List[PReg], srams:List[PSRAM], ctrs:List[PCounter], 
    inports:List[InPort], outports:List[InPort]) = {
    new PComputeUnit(regs, srams, ctrs, inports, outports) with PMemoryController 
  }
}

