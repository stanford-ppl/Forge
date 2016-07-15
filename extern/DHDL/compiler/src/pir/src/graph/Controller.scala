package dhdl.graph

import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.math.max
import dhdl.Design
import dhdl.graph._

class Controller(name: Option[String], block: => Any,typeStr:String)(implicit design: Design) extends Node(name, typeStr) {
  val nodes = design.addBlock(block, (n:Node) => true) 
}

abstract class ComputeUnit(name: Option[String], block: => Any,typeStr:String)(implicit design: Design) extends Controller(name, block, typeStr) {
  val tpe:CtrlType
  var parent:Controller = _
  toUpdate = true
  val cchains = nodes.filter(n => n.isInstanceOf[CounterChain]).asInstanceOf[List[CounterChain]] 
  val srams:List[SRAM] = nodes.filter(n => n.isInstanceOf[SRAM]).asInstanceOf[List[SRAM]]
  val pipeline = {
    val temp = nodes.filter(n => n.isInstanceOf[Pipeline]) 
    assert(temp.size <= 1, "ComputeUnit has less than or equals to 1 Pipeline")
    if (temp.size == 1)
      temp(0).asInstanceOf[Pipeline]
    else
      { val p = Pipeline{}; p.ctrler = this; p }
  }
  val mapping:PipeRegMapping
  nodes.foreach {n => n match {
      case n:Primitive => n.ctrler = this
      case _ =>
    }
  }
  def update = (p:Controller) => {this.parent = p; toUpdate = false}

}
object ComputeUnit {
  val typeStr = "CU"
  def apply (parent:String, tpe:CtrlType) (block: PipeRegMapping => Any) (implicit design: Design):ComputeUnit =
    ComputeUnit(None, parent, tpe) (block)
  def apply (name:String, parent: String, tpe:CtrlType) (block:PipeRegMapping => Any) (implicit design: Design):ComputeUnit =
    ComputeUnit(Some(name), parent, tpe) (block)
  def apply (parent:Controller, tpe:CtrlType) (block:PipeRegMapping => Any) (implicit design: Design):ComputeUnit =
    ComputeUnit(None, parent, tpe) (block)
  def apply (name:String, parent: Controller, tpe:CtrlType) (block:PipeRegMapping => Any) (implicit design: Design):ComputeUnit =
    ComputeUnit(Some(name), parent, tpe) (block)
  def apply (name:Option[String], parent: String, tpe:CtrlType) (block:PipeRegMapping => Any) (implicit design: Design):ComputeUnit = {
    val c = ComputeUnit(name, block, tpe); 
    design.updateLater(parent, (n:Node) => c.update(n.asInstanceOf[Controller]))
    c
  }
  def apply (name:Option[String], parent: Controller, tpe:CtrlType) (block:PipeRegMapping => Any) (implicit design: Design):ComputeUnit = {
    val c = ComputeUnit(name, block, tpe); c.update(parent); c
  }
  def apply (name:Option[String], block:PipeRegMapping => Any, t:CtrlType) (implicit design: Design):ComputeUnit = {
    val m = PipeRegMapping(if(name.isDefined) Some(name.get + "_mapping") else None)
    val c = new { override val mapping = m; override val tpe = t }
            with ComputeUnit(name, block(m), typeStr)
    m.ctrler = c
    c
  }
}

trait MemoryController extends ComputeUnit {
  val dram:String
} 
object MemoryController extends {
  val typeStr = "MemCtrl"
  def apply (parent:String, dram:String) (block: PipeRegMapping => Any) (implicit design: Design):MemoryController =
    MemoryController(None, parent, dram) (block)
  def apply (name:String, parent: String, dram:String) (block:PipeRegMapping => Any) (implicit design: Design):MemoryController =
    MemoryController(Some(name), parent, dram) (block)
  def apply (parent:Controller, dram:String) (block:PipeRegMapping => Any) (implicit design: Design):MemoryController =
    MemoryController(None, parent, dram) (block)
  def apply (name:String, parent: Controller, dram:String) (block:PipeRegMapping => Any) (implicit design: Design):MemoryController =
    MemoryController(Some(name), parent, dram) (block)
  def apply (name:Option[String], parent: String, dram:String) (block:PipeRegMapping => Any) (implicit design: Design):MemoryController = {
    val c = MemoryController(name, block, dram); 
    design.updateLater(parent, (n:Node) => c.update(n.asInstanceOf[Controller]))
    c
  }
  def apply (name:Option[String], parent: Controller, dram:String) (block:PipeRegMapping => Any) (implicit design: Design):MemoryController = {
    val c = MemoryController(name, block, dram); c.update(parent); c
  }
  def apply (name:Option[String], block:PipeRegMapping => Any, d:String) (implicit design: Design):MemoryController = {
    val m = PipeRegMapping(if(name.isDefined) Some(name.get + "_mapping") else None)
    val c = new { override val mapping = m; override val tpe = Pipe; override val dram = d}
            with ComputeUnit(name, block(m), typeStr) with MemoryController
    m.ctrler = c
    c
  }
}

trait Top extends Controller {
  val ctrlList:List[Controller] =
    nodes.filter(_.isInstanceOf[Controller]).asInstanceOf[List[Controller]] 
}
object Top {
  val typeStr = "Top"
  def apply (block: => Any) (implicit design: Design):Top =
    new Controller(Some("Top"), block, typeStr) with Top
}
