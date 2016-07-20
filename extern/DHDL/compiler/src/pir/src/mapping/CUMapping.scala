package dhdl.graph.mapping
import dhdl.graph._
import dhdl.graph.{ComputeUnit => CU, MemoryController => MC}
import dhdl.Design
import dhdl.Config
import dhdl.plasticine.graph.{ComputeUnit => PCU, MemoryController => PMC}
import dhdl.graph.mapping
import dhdl.graph.mapping.CUMapping.PrimMapping

import scala.collection.immutable.Set
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

object CUMapping {
  type PrimMapping = (PCU, SRAMMapping, CtrMapping)
}
class CUMapping(implicit val design: Design) extends Mapping[CU, PCU, PrimMapping]{

  override var mapping:Map[CU, PrimMapping] = null

  private val pcus = ListBuffer[PCU]()
  private val pmcs = ListBuffer[PMC]()
  private val cus = ListBuffer[CU]()
  private val mcs = ListBuffer[MC]()

  lazy private val arch = design.arch
  lazy private val top = design.top
  lazy private val allNodes = design.allNodes

  override def reset = {
    super.reset
    pcus.clear
    pmcs.clear
    cus.clear
    mcs.clear
  }

  /* Saperate Compute Unit and Memory controller to map saperately later on
   * Check whether design would fit the architecture using a rough count */
  private def setResource: (Boolean, List[Hint]) = {
    arch.computeUnits.foreach { c => c match {
        case n:PMC => pmcs += n
        case n:PCU => pcus += n
        case n => new Exception(s"TODO: unknown Spade ComputeUnit type: ${n}") 
      }
    }
    allNodes.foreach { c => c match {
        case n:MC => mcs += n
        case n:CU => cus += n
        case n => new Exception(s"TODO: unknown PIR controller type: ${n}") 
      }
    }
    val (s1, h1) = if(cus.size > pcus.size) (false, List(OutOfPCU(arch)) )
                   else (true, Nil)
    val (s2, h2) = if(mcs.size > pmcs.size) (false, List(OutOfPMC(arch)) )
                   else (true, Nil)
    (s1 && s2, h1 ++ h2)
  }

  def checkIntConnct(cu:CU, pcu:PCU, cuMap:Map[CU, PrimMapping]):
    (Boolean, List[Hint], Map[CU, PrimMapping]) = {
    val suc = true
    (suc, if (suc) Nil else List(IntConnct(cu, pcu)), cuMap)
  }

  def primMapping(cu:CU, pcu:PCU, cuMap:Map[CU, PrimMapping]):
    (Boolean, List[Hint], Map[CU, PrimMapping]) = {
  //TODO
    val sm = SRAMMapping(cu, pcu, cuMap)
    val (smSuc, smHints) = sm.map
    val cm = CtrMapping(cu, pcu, cuMap)
    val (cmSuc, cmHints) = cm.map
    (smSuc && cmSuc, smHints ++ cmHints, cuMap + (cu -> (pcu, sm, cm)))
  }

  override def map:(Boolean, List[Hint]) = {
    val (suc, hts) = setResource
    var hints = hts 
    if (!suc)
      return (false, hints)

    val cons = List(checkIntConnct _, primMapping _)

    val (cuSuc, cuHints, cm) = 
      simAneal(pcus.toList, cus.toList, Map[CU, PrimMapping](), cons)
    hints ++= cuHints
    if (!cuSuc) {
      this.mapping = cm
      return (false, hints)
    }
    val (mcSuc, mcHints, mm) = 
      simAneal(pmcs.toList, mcs.toList, cm, cons)
    hints ++= mcHints
    this.mapping = mm 
    //println("-------- Finish CU Mapping ----------")
    (mcSuc, hints)
  }

  override def printMap = {
    pBS("cuMap")
    mapping.foreach{ case (cu, (pcu, sm, cm)) =>
      pln(s"[$cu -> $pcu]")
      sm.printMap
      cm.printMap
    }
    pBE
  }

}
