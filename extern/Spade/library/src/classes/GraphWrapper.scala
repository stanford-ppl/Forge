package spade.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}

import spade.shared._
import spade.shared.ops._
import spade.library._
import spade.library.classes._

trait GraphWrapper {
  this: SpadeBase with SpadeClasses =>

  type HwNode = AbstractHwNode
  type ComputeNode = AbstractComputeNode
  type MemoryNode = AbstractMemoryNode
  type SwitchNode = AbstractSwitchNode
  type EdgeNode = AbstractEdgeNode

  abstract class AbstractHwNode
  abstract class AbstractComputeNode extends AbstractHwNode
  abstract class AbstractMemoryNode extends AbstractHwNode
  abstract class AbstractSwitchNode extends AbstractHwNode
  abstract class AbstractEdgeNode

  // Node representing compute
  case class Compute() extends AbstractComputeNode

  // Node representing memory
  case class Memory() extends AbstractMemoryNode

  // Node representing edge
  case class Edge(src: HwNode, dst: HwNode) extends AbstractEdgeNode

  // Node representing switch
  case class Switch() extends AbstractSwitchNode

  def compute_new(implicit ctx: SourceContext): ComputeNode = Compute()
  def memory_new(implicit ctx: SourceContext): MemoryNode = new Memory()
  def edge_new(src: HwNode, dst: HwNode)(implicit ctx: SourceContext): EdgeNode = new Edge(src, dst)
  def switch_new(implicit ctx: SourceContext): SwitchNode = new Switch()
}
