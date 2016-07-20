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

case class SRAMMapping(cu:CU, pcu:PCU, cuMap:Map[CU, PrimMapping])(implicit val design: Design) extends Mapping[SRAM, PSRAM, PSRAM]{

  override var mapping:Map[SRAM, PSRAM] = _

  lazy private val arch = design.arch
  lazy private val top = design.top
  lazy private val allNodes = design.allNodes

  def mapSRAM(s:SRAM, p:PSRAM, map:Map[SRAM, PSRAM]) = {
    (true, Nil, map + (s -> p))
  }

  override def map:(Boolean, List[Hint]) = {
    if (cu.srams.size > pcu.srams.size) {
      (false, List(OutOfSram(pcu)))
    } else {
      val (ssuc, shints, smap) = simAneal(pcu.srams, cu.srams, HashMap[SRAM, PSRAM](), List(mapSRAM _))
      mapping = smap
      (ssuc, shints)
    }
  }

  import DFMapping._
  override def printMap = {
    emitBS("sramMap")
    mapping.foreach{ case (k,v) =>
      emitln(s"($k -> $v)")
    }
    emitBE
  }

}
