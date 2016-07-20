package dhdl.graph.traversal
import dhdl.graph._
import dhdl.Design
import dhdl.Config

import scala.collection.mutable.Set

trait Traversal {
  var isInit = false
  val visited = Set[Node]()
  val debug = false

  def msg(x: String) = if (Config.quick) () else System.out.println(x)
  def dbg(x: String) = if (debug && !Config.dse) System.out.println(x)

  def reset {
    visited.clear()
    isInit = false
  }
  
  def run:Unit

  def initPass() = {
    isInit = true
  }

  def finPass() = {}
}
