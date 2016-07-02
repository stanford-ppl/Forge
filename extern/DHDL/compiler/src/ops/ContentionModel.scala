package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._
import dhdl.compiler.transform._

import scala.collection.mutable.HashMap

trait ContentionModel {
  val IR: DHDLExp with PipeStageToolsExp
  import IR._

  val isolatedContention = HashMap[Exp[Any],List[Int]]()

  def outerContention(x: Exp[Any], P: => Int): Int = {
    if (!isInnerPipe(x) && childrenOf(x).nonEmpty) {
      val ics = childrenOf(x).map{c => calcContention(c) * P}
      isolatedContention(x) = ics
      if (isMetaPipe(x) || isStreamPipe(x)) ics.sum else ics.max
    }
    else 0
  }

  def calcContention(x: Exp[Any]): Int = x match {
    case Deff(_:Hwblock)         => outerContention(x, 1)
    case Deff(_:Pipe_parallel)   => childrenOf(x).map(calcContention).sum
    case Deff(_:Unit_pipe)       => outerContention(x, 1)
    case Deff(e:Pipe_foreach)    => outerContention(x, parsOf(e.cchain).reduce{_*_})
    case Deff(e:Pipe_fold[_,_])  => outerContention(x, parsOf(e.cchain).reduce{_*_})
    case Deff(e:Accum_fold[_,_]) => outerContention(x, parsOf(e.ccOuter).reduce{_*_})
    case Deff(_:Offchip_load_cmd[_])  => 1
    case Deff(_:Offchip_store_cmd[_]) => 1
    case Deff(_:Scatter[_]) => 1
    case Deff(_:Gather[_]) => 1
    case _ => 0
  }

  def markPipe(x: Exp[Any], parent: Int) {
    if (isMetaPipe(x) || isStreamPipe(x)) {
      childrenOf(x).foreach{child => markContention(child,parent) }
    }
    else if (isSequential(x) && childrenOf(x).nonEmpty) {
      val ics = isolatedContention(x)
      val mx = ics.max
      // Can just skip case where mx = 0 - no offchip memory accesses in this sequential anyway
      if (mx > 0) childrenOf(x).zip(ics).foreach{case (child,c) => markContention(child, (parent/mx)*c) }
    }
  }

  def markContention(x: Exp[Any], parent: Int): Unit = x match {
    case Deff(_:Hwblock)         => markPipe(x, parent)
    case Deff(_:Pipe_parallel)   => childrenOf(x).foreach{child => markContention(child,parent)}
    case Deff(_:Unit_pipe)       => markPipe(x, parent)
    case Deff(_:Pipe_foreach)    => markPipe(x, parent)
    case Deff(_:Pipe_fold[_,_])  => markPipe(x, parent)
    case Deff(_:Accum_fold[_,_]) => markPipe(x, parent)
    case Deff(_:Offchip_load_cmd[_])  => contentionOf(x) = parent
    case Deff(_:Offchip_store_cmd[_]) => contentionOf(x) = parent
    case Deff(_:Scatter[_]) => contentionOf(x) = parent
    case Deff(_:Gather[_]) => contentionOf(x) = parent
    case _ => // do nothing
  }

  def run(top: Exp[Any]) = {
    val c = calcContention(top)
    markContention(top, c)
  }
}
