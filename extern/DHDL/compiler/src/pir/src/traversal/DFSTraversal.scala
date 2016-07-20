package dhdl.graph.traversal
import dhdl.graph._
import dhdl.Design
import dhdl.Config

import scala.collection.mutable.Set

abstract class DFSTraversal(implicit val design: Design) extends Traversal{
  def traverse(node: Node) = {
    visitNode(node)
  }
  
  override def traverse = traverse(design.top)

  /* Depth first search traversal on node and their fields */
  def visitNode(node: Node) : Unit = {
    assert(!visited.contains(node), s"Revisiting visited node ${node}! visitedNodes:${visited}")
    node match {
      case n:Controller => n match {
        case c:Top => 
          c.ctrlNodes.foreach(n => visitNode(n))
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
          p.operands.foreach(op => visitNode(op.src))
          visitNode(p.result.src)
        case p:Pipeline => p.stages.foreach(s => visitNode(s))
      }
      case n:Const =>
      case _ =>
        throw new Exception(s"Don't know how to visit $node")
    }
    visited += node
  }
}
