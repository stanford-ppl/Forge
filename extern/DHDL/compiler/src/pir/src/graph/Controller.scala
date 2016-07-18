package dhdl.graph

import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.math.max
import dhdl.Design
import dhdl.graph._

abstract class Controller(name: Option[String],typeStr:String)(implicit design: Design) extends Node(name, typeStr) {
  val nodes:List[Node] 
}

case class ComputeUnit(n: Option[String], tpe:CtrlType, nds:List[Node], 
  cchains:List[CounterChain], srams:List[SRAM], pipeline:Pipeline)
(implicit design: Design) extends Controller(n, "CU") {
  var parent:Controller = _
  toUpdate = true
  override val nodes = nds
  nodes.foreach {n => n match {
      case n:Primitive => n.ctrler = this
      case _ =>
    }
  }
  def update = (p:Controller) => {this.parent = p; toUpdate = false}
}
object ComputeUnit {
  def apply (parent:String, tpe:CtrlType) (block: Pipeline => Any) (implicit design: Design):ComputeUnit =
    ComputeUnit(None, parent, tpe) (block)
  def apply (name:String, parent: String, tpe:CtrlType) (block:Pipeline => Any) (implicit design: Design):ComputeUnit =
    ComputeUnit(Some(name), parent, tpe) (block)
  def apply (parent:Controller, tpe:CtrlType) (block:Pipeline => Any) (implicit design: Design):ComputeUnit =
    ComputeUnit(None, parent, tpe) (block)
  def apply (name:String, parent: Controller, tpe:CtrlType) (block:Pipeline => Any) (implicit design: Design):ComputeUnit =
    ComputeUnit(Some(name), parent, tpe) (block)
  def apply (name:Option[String], parent: String, tpe:CtrlType) (block:Pipeline => Any) (implicit design: Design):ComputeUnit = {
    val c = ComputeUnit(name, block, tpe); 
    design.updateLater(parent, (n:Node) => c.update(n.asInstanceOf[Controller]))
    c
  }
  def apply(name:Option[String], parent: Controller, tpe:CtrlType) (block:Pipeline => Any) (implicit design: Design):ComputeUnit = {
    val c = ComputeUnit(name, block, tpe); c.update(parent); c
  }
  def apply(name:Option[String], block:Pipeline => Any, tpe:CtrlType) (implicit design: Design):ComputeUnit = {
    val (nodes, cchains, srams, pipeline) = unwrapBlock(block)
    new ComputeUnit(name, tpe, nodes, cchains, srams, pipeline)
  }
  def unwrapBlock(block: Pipeline => Any)(implicit design: Design):
    (List[Node], List[CounterChain], List[SRAM], Pipeline) = {
    val pipeline = Pipeline(None)
    val (nds, cchains, srams) = 
      design.addBlock[Node, CounterChain, SRAM](block(pipeline), 
                            (n:Node) => true, 
                            (n:Node) => n.isInstanceOf[CounterChain], 
                            (n:Node) => n.isInstanceOf[SRAM]) 
    (nds :+ pipeline, cchains, srams, pipeline)
  }
}

trait MemoryController extends ComputeUnit {
  val dram:String
  override val typeStr = "MemCtrl"
} 
object MemoryController extends {
  def apply (parent:String, dram:String) (block: Pipeline => Any) (implicit design: Design):MemoryController =
    MemoryController(None, parent, dram) (block)
  def apply (name:String, parent: String, dram:String) (block:Pipeline => Any) (implicit design: Design):MemoryController =
    MemoryController(Some(name), parent, dram) (block)
  def apply (parent:Controller, dram:String) (block:Pipeline => Any) (implicit design: Design):MemoryController =
    MemoryController(None, parent, dram) (block)
  def apply (name:String, parent: Controller, dram:String) (block:Pipeline => Any) (implicit design: Design):MemoryController =
    MemoryController(Some(name), parent, dram) (block)
  def apply (name:Option[String], parent: String, dram:String) (block:Pipeline => Any) (implicit design: Design):MemoryController = {
    val c = MemoryController(name, block, dram); 
    design.updateLater(parent, (n:Node) => c.update(n.asInstanceOf[Controller]))
    c
  }
  def apply (name:Option[String], parent: Controller, dram:String)(block:Pipeline => Any) (implicit design: Design):MemoryController = {
    val c = MemoryController(name, block, dram); c.update(parent); c }
  def apply(name:Option[String], block:Pipeline => Any, d:String) (implicit design: Design):MemoryController = {
    val (nodes, cchains, srams, pipeline) = ComputeUnit.unwrapBlock(block)
    new {override val dram = d} with ComputeUnit(name, Pipe, nodes, cchains, srams, pipeline) with MemoryController
  }
}

abstract case class Top(ctrlList:List[Controller])(implicit design: Design) 
  extends Controller(Some("Top"), "Top")
object Top {
  def apply (block: => Any) (implicit design: Design):Top = {
    val (nds,cl) = design.addBlock[Node, Controller](block, 
      (n:Node) => true,
      (n:Node) => n.isInstanceOf[Controller])
    new { override val nodes = nds } with Top(cl)
  }
}
