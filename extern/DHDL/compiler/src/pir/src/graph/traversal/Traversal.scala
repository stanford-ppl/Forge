package dhdl.graph.traversal
import dhdl.graph._
import dhdl.Design
import dhdl.Config

import scala.collection.mutable.Set

abstract class Traversal(implicit val design: Design) {
  var isInit = false
  val visited = Set[Node]()
  val debug = false

  def msg(x: String) = if (Config.quick) () else System.out.println(x)
  def dbg(x: String) = if (debug && !Config.dse) System.out.println(x)

  def reset() {
    visited.clear()
    isInit = false
  }

  def run(node: Node) = {
    initPass()
    visitNode(node)
    finPass()
  }

  def initPass() = {
    isInit = true
  }

  /* Depth first search traversal on node and their fields */
  def visitNode(node: Node) : Unit = {
    assert(!visited.contains(node), s"Revisiting visited node ${node}!")
    node match {
      case n:Controller => n match {
        case c:Top => 
          c.ctrlList.foreach(n => visitNode(n))
        case c:ComputeUnit => {
          c.cchains.foreach { cc => visitNode(cc) }
          c.srams.foreach { s => visitNode(s) }
          visitNode(c.pipeline)
          c match {
            case cu:MemoryController =>
            case _ =>
          }
        }
      } 
      case n:Primitive => n match {
        case p:CounterChain => p.counters.foreach(c => visitNode(c))
        case p:SRAM => 
        case p:Stage =>
          p.operands.foreach(op => visitNode(op))
          visitNode(p.result)
        case p:Pipeline => p.stages.foreach(s => visitNode(s))
      }
      case n:Port =>
      case _ =>
        throw new Exception(s"Don't know how to visit $node")
    }
    visited += node
  }

  def finPass() = {}
}
