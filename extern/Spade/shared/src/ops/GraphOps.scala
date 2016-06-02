package spade.shared.ops

import scala.virtualization.lms.common.{Base, Record}
import scala.reflect.{Manifest,SourceContext}

import spade.shared._
import spade.shared.ops._

trait GraphOps extends Base {
  this: Spade =>

  type HwNode
  type ComputeNode
  type MemoryNode
  type SwitchNode
  type EdgeNode

  def compute(implicit ctx: SourceContext): ComputeNode = compute_new
  def memory(implicit ctx: SourceContext): MemoryNode = memory_new
  def edge(src: HwNode, dst: HwNode)(implicit ctx: SourceContext): EdgeNode = edge_new(src, dst)
  def switch(implicit ctx: SourceContext): SwitchNode = switch_new

  def compute_new(implicit ctx: SourceContext): ComputeNode
  def memory_new(implicit ctx: SourceContext): MemoryNode
  def edge_new(src: HwNode, dst: HwNode)(implicit ctx: SourceContext): EdgeNode
  def switch_new(implicit ctx: SourceContext): SwitchNode
}

trait GraphCompilerOps extends GraphOps { this: Spade => }
