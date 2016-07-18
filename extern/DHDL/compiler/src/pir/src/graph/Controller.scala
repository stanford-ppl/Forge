package dhdl.graph

import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.math.max
import dhdl.Design
import dhdl.graph._

trait Controller extends Node

case class ComputeUnit(n: Option[String], tpe:CtrlType, cchains:List[CounterChain], srams:List[SRAM], 
  pipeline:Pipeline) (implicit design: Design) extends Node(n, "CU") with Controller {
  var parent:Controller = _
  toUpdate = true
  cchains.foreach(_.updateCtrler(this))
  srams.foreach(_.updateCtrler(this))
  pipeline.updateCtrler(this)

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
    val (cchains, srams, pipeline) = unwrapBlock(block)
    new ComputeUnit(name, tpe, cchains, srams, pipeline)
  }
  def unwrapBlock(block: Pipeline => Any)(implicit design: Design):
    (List[CounterChain], List[SRAM], Pipeline) = {
    val pipeline = Pipeline(None)
    val (cchains, srams) = 
      design.addBlock[CounterChain, SRAM](block(pipeline), 
                            (n:Node) => n.isInstanceOf[CounterChain], 
                            (n:Node) => n.isInstanceOf[SRAM]) 
    (cchains, srams, pipeline)
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
    val (cchains, srams, pipeline) = ComputeUnit.unwrapBlock(block)
    new {override val dram = d} with ComputeUnit(name, Pipe, cchains, srams, pipeline) with MemoryController
  }
}

case class Top(ctrlList:List[Controller])(implicit design: Design) 
  extends Node(Some("Top"), "Top") with Controller
