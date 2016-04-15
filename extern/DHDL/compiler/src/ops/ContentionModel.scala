package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._
import dhdl.compiler.transform._

import scala.collection.mutable.HashMap

trait ContentionModel {
  val IR: DHDLExp
  import IR._

  var top: Exp[Any] = null

  val isolatedContention = HashMap[Exp[Any],List[Int]]()

  def contentionOfOuterPipe(x: Exp[Any], P: Int): Int = {
    if (styleOf(x) != Fine) {
      val ics = childrenOf(x).map(calcContention).map(c => c * P)
      isolatedContention(x) -> ics
      if (styleOf(x) == Coarse) ics.sum else ics.max
    }
    else 0
  }

  def calcContention(x: Exp[Any]): Int = x match {
    case EatReflect(_:Pipe_parallel) =>
      childrenOf(x).map(calcContention).sum

    case EatReflect(_:Unit_pipe) if styleOf(x) != Fine =>
      val ics = childrenOf(x).map(calcContention)
      contentionOfOuterPipe(x, 1)

    case EatReflect(e:Pipe_foreach) if styleOf(x) != Fine =>
      val P = parOf(e.cchain).reduce{_*_}
      contentionOfOuterPipe(e, P)

    case EatReflect(e:Pipe_reduce[_,_]) if styleOf(x) != Fine =>
      val P = parOf(e.cchain).reduce{_*_}
      contentionOfOuterPipe(e, P)

    case EatReflect(e:Block_reduce[_]) =>
      val P = parOf(e.ccOuter).reduce{_*_}
      contentionOfOuterPipe(e, P)

    case EatReflect(_:TileTransfer[_]) => 1
    case _ => 0
  }

  def markOuterPipe(x: Exp[Any], parent: Int) {
    if (styleOf(x) == Coarse) childrenOf(x).foreach{n => markContention(x,parent) }
    else if (styleOf(x) == Disabled) {
      val ics = isolatedContention(x)
      val mx = ics.max
      childrenOf(x).zip(ics).foreach{case (child,c) => markContention(child, (parent/mx)*c) }
    }
  }

  def markContention(x: Exp[Any], parent: Int): Unit = x match {
    case EatReflect(_:Pipe_parallel) => childrenOf(x).foreach{n => markContention(x,parent) }
    case EatReflect(_:Unit_pipe) => markOuterPipe(x, parent)
    case EatReflect(_:Pipe_foreach) => markOuterPipe(x, parent)
    case EatReflect(_:Pipe_reduce[_,_]) => markOuterPipe(x, parent)
    case EatReflect(_:Block_reduce[_]) => markOuterPipe(x, parent)
    case EatReflect(_:TileTransfer[_]) => contentionOf(x) = parent
    case _ => // do nothing
  }

  def run() = {
    if (top eq null) stageError("Contention model was uninitialized when run")
    val c = calcContention(top)
    markContention(top, c)
  }

  // TODO: This assumes no transformers were run before contention model is being used
  def init() = {
    for ((s,m) <- metadata) {
      if (meta[MPipeType](s).isDefined && !meta[MParent](s).isDefined) {
        if (top eq null) top = s
        else stageError(s"Found multiple candidates for top control node: $s and $top")
      }
    }
  }
}
