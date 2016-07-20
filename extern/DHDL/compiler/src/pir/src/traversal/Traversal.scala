package dhdl.graph.traversal
import dhdl.graph._
import dhdl.Design
import dhdl.Config
import dhdl.PIRMisc._

import scala.collection.mutable.Set

trait Traversal {
  var isInit = false
  val visited = Set[Node]()

  def reset {
    visited.clear()
    isInit = false
  }
  
  def run = {
    initPass
    traverse
    finPass
  }

  def traverse:Unit

  def initPass = {
    isInit = true
  }

  def finPass:Unit
}
