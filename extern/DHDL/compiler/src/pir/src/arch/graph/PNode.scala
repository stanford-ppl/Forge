package dhdl.plasticine.graph

import dhdl.graph._

import scala.collection.mutable.ListBuffer

class Node { 
  val id : Int = Node.nextId
}
object Node {
  var nextSym = 0
  def nextId = {val temp = nextSym; nextSym +=1; temp}
}

/** Type of nodes that can connect to pipeline registers */
trait RegMapping {
  var preg:Reg = _
  /** create a mapping to a pipeline register
   *  @param r: mapped register */
  def mapTo(r:Reg) = {
    preg = r
    r.map(this)
  }
}

/** 1 mapping of pipeline register (1 row of reg for all stages) */
case class Reg() extends Node{
  val mapping = ListBuffer[RegMapping]()
  def map(n:RegMapping) = {
    if (!mapping.contains(n))
      mapping += n
  }
}

/** Physical SRAM 
 *  @param numPort: number of banks. Usually equals to number of lanes in CU */
case class SRAM(numBanks:Int) extends Node with RegMapping{
}
object SRAM {
  def apply(numBanks:Int, reg:Reg):SRAM = {
    val s = SRAM(numBanks)
    s.mapTo(reg)
    s
  }
}

/** Physical Counter  */
case class Counter() extends Node with RegMapping{
}
object Counter {
  def apply(reg:Reg):Counter = {
    val c = Counter()
    c.mapTo(reg)
    c
  }
}

/** Physical Port of CU  */
case class Port() extends Node with RegMapping {
}
trait InPort
object InPort {
  def apply(reg:Reg) = {
    val p = new Port() with InPort
    p.mapTo(reg)
    p
  }
}
trait OutPort
object OutPort {
  def apply(reg:Reg) = {
    val p = new Port() with OutPort
    p.mapTo(reg)
    p
  }
}

/** Physical ComputeUnit 
 *  @param regs: one set of pipeline registers in a pipeline register block 
 *  @param srams: one set of SRAMs availabe in the CU
 *  @param ctrs: one set of Counters availabe in the CU
 *  @param inports: one set of input ports availabe in the CU
 *  @param outports: one set of output ports availabe in the CU
 *  */
case class ComputeUnit(regs:List[Reg], srams:List[SRAM], ctrs:List[Counter], 
  inports:List[InPort], outports:List[OutPort], reduce:Reg) extends Node{
  def numPRs = regs.size 
  def numCtrs = ctrs.size
  def numSRAMs = srams.size
  def numInPorts = inports.size 
  def numOutPorts = outports.size 
}

trait MemoryController {
}
object MemoryController {
  def apply(regs:List[Reg], srams:List[SRAM], ctrs:List[Counter], 
    inports:List[InPort], outports:List[OutPort], reduce:Reg) = {
    new ComputeUnit(regs, srams, ctrs, inports, outports, reduce) with MemoryController 
  }
}

