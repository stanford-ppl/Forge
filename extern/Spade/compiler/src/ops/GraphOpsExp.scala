package spade.compiler.ops

import scala.virtualization.lms.common.BaseExp
import scala.virtualization.lms.common.DotGenBase
import scala.virtualization.lms.common.ScalaGenBase
import scala.reflect.{Manifest,SourceContext}

import spade.shared._
import spade.shared.ops._
import spade.compiler._
import spade.compiler.ops._

trait GraphOpsExp extends GraphOps with BaseExp {
  this: SpadeExp =>

  type HwNode = AbstractHwNode
  type ComputeNode = AbstractComputeNode
  type MemoryNode = AbstractMemoryNode
  type SwitchNode = AbstractSwitchNode
  type EdgeNode = AbstractEdgeNode

  abstract class AbstractHwNode extends Def
  abstract class AbstractComputeNode extends AbstractHwNode
  abstract class AbstractMemoryNode extends AbstractHwNode
  abstract class AbstractSwitchNode extends AbstractHwNode
  abstract class AbstractEdgeNode extends Def

  // Node representing compute
  case class Compute() extends AbstractComputeNode

  // Node representing memory
  case class Memory() extends AbstractMemoryNode

  // Node representing edge
  case class Edge(src: HwNode, dst: HwNode) extends AbstractEdgeNode

  // Node representing switch
  case class Switch() extends AbstractSwitchNode

  def compute_new(implicit ctx: SourceContext): AbstractComputeNode = {
    Compute()
  }
  def memory_new(implicit ctx: SourceContext): AbstractMemoryNode = {
    Memory()
  }
  def edge_new(src: HwNode, dst: HwNode)(implicit ctx: SourceContext): AbstractEdgeNode = {
    Edge(src, dst)
  }
  def switch_new(implicit ctx: SourceContext): AbstractSwitchNode = {
    Switch()
  }
}

trait ScalaGenGraphOps extends ScalaGenBase {
  val IR: GraphOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case mn: Compute =>
      stream.println(" // " + quote(sym) + " " + rhs)

    case mn: Memory =>
      stream.println(" // " + quote(sym) + " " + rhs)

    case mn: Switch =>
      stream.println(" // " + quote(sym) + " " + rhs)

    case mn@Edge(lhs, rhs) =>
      stream.println(" // " + quote(sym) + " " + rhs)

    case _ => super.emitNode(sym, rhs)
  }
}

trait DotGenGraphOps extends DotGenBase {
  val IR: GraphOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case mn: Compute =>
      stream.println(" // " + quote(sym) + " " + rhs)

    case mn: Memory =>
      stream.println(" // " + quote(sym) + " " + rhs)

    case mn: Switch =>
      stream.println(" // " + quote(sym) + " " + rhs)

    case mn@Edge(lhs, rhs) =>
      stream.println(" // " + quote(sym) + " " + rhs)

    case _ => super.emitNode(sym, rhs)
  }
}
