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

  def visitNode(node: Node) : Unit = {
    node match {
      case n:Controller => n match {
        case c:Top => 
          c.ctrlList.foreach(n => visitNode(n))
        case c:ComputeUnit => {
          c.cchains.foreach { cc => visitNode(cc) }
          c.srams.foreach { s => visitNode(s) }
          visitNode(c.pipeline)
        }
      } 
      case n:Primitive => n match {
        case p:CounterChain => p.counters.foreach(c => visitNode(c))
        case p:Pipeline => p.stages.foreach(s => visitNode(s))
        case p:SRAM =>
        case p:Stage =>
      }
      case n:Wire =>
      case _ =>
        throw new Exception(s"Don't know how to visit $node")
    }
  }

  def finPass() = {}
}
