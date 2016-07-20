package dhdl.graph.mapping
import dhdl.Design
import dhdl.Config
import dhdl.graph.{ComputeUnit => CU, MemoryController => MC}
import dhdl.graph.{Counter => Ctr, _}
import dhdl.plasticine.graph.{ComputeUnit => PCU, MemoryController => PMC}
import dhdl.plasticine.graph.{Counter => PCtr, SRAM => PSRAM}
import dhdl.graph.mapping.CUMapping.PrimMapping
import dhdl.graph.traversal.DFMapping

import scala.collection.immutable.Set
import scala.collection.immutable.HashMap
import scala.collection.immutable.Map

case class CtrMapping(cu:CU, pcu:PCU, cuMap:Map[CU, PrimMapping])(implicit val design: Design) extends Mapping[Ctr, PCtr, PCtr]{

  override var mapping:Map[Ctr, PCtr] = _

  lazy private val arch = design.arch
  lazy private val top = design.top
  lazy private val allNodes = design.allNodes

  def mapCtr(c:Ctr, p:PCtr, map:Map[Ctr, PCtr]) = {
    (true, Nil, map + (c -> p))
  }

  override def map:(Boolean, List[Hint]) = {
    val ctrs = cu.cchains.flatMap{cc => cc.counters}
    if (ctrs.size > pcu.ctrs.size) {
      (false, List(OutOfCtr(pcu)))
    } else {
      val (csuc, chints, cmap) = simAneal(pcu.ctrs, ctrs, HashMap[Ctr, PCtr](), List(mapCtr _))
      mapping = cmap
      (csuc, chints)
    }
  }

  import DFMapping._
  override def printMap = {
    emitBS("ctrMap")
    mapping.foreach{ case (k,v) =>
      emitln(s"($k -> $v)")
    }
    emitBE
  }

}
