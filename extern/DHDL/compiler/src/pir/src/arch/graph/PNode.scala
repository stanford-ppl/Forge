package dhdl.plasticine.graph

import dhdl.graph._

import scala.collection.mutable.ListBuffer

class Node { 
  val id : Int = Node.nextId
  override def equals(that: Any) = that match {
    case n: Node => super.equals(that) && id == n.id
    case _ => super.equals(that)
  }

  val typeStr = this.getClass().getSimpleName()
  override def toString = s"${typeStr}_${id}" 
}
object Node {
  var nextSym = 0
  def nextId = {val temp = nextSym; nextSym +=1; temp}
}

/** 1 mapping of pipeline register (1 row of reg for all stages) */
case class Reg() extends Node{
  // List of connections one register can mapp to
  val mapping = ListBuffer[RMPort]()
  def map(n:RMPort) = {
    if (!mapping.contains(n))
      mapping += n
  }
}

/** Physical SRAM 
 *  @param numPort: number of banks. Usually equals to number of lanes in CU */
case class SRAM(numBanks:Int) extends Node{
  val readPort = RMPort(this)
  val writePort = RMPort(this) 
  def load = readPort
  def write = writePort
}
object SRAM {
  def apply(numBanks:Int, rreg:Reg, wreg:Reg):SRAM = {
    val s = SRAM(numBanks)
    s.load.mapTo(rreg)
    s.write.mapTo(wreg)
    s
  }
}

/** Physical Counter  */
case class Counter() extends Node{
  val out = RMPort(this)
}
object Counter {
  def apply(reg:Reg):Counter = {
    val c = Counter()
    c.out.mapTo(reg)
    c
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
  inRegs:List[Reg], outRegs:List[Reg], reduceReg:Reg) extends Node{
  override val typeStr = "CU"
  val reducePort = RMPort(this, reduceReg)
  val inPorts = inRegs.map{r => RMPort(this, r)} 
  val outPorts = outRegs.map{r => RMPort(this, r)} 
  def numPRs = regs.size 
  def numCtrs = ctrs.size
  def numSRAMs = srams.size
  def numInPorts = inPorts.size
  def numPorts = outPorts.size 
}

trait MemoryController extends ComputeUnit{
}
object MemoryController {
  def apply(regs:List[Reg], srams:List[SRAM], ctrs:List[Counter], 
    inRegs:List[Reg], outRegs:List[Reg], reduceReg:Reg) = {
    new {override val typeStr = "MemCtrl"} 
    with ComputeUnit(regs, srams, ctrs, inRegs, outRegs, reduceReg) with MemoryController 
  }
}

trait RMPort { self:Port =>
  var preg:Reg = _
  /** create a mapping to a pipeline register
   *  @param r: mapped register */
  def mapTo(r:Reg) = {
    preg = r
    r.map(this)
  }
}
object RMPort {
  def apply(s: Node, reg:Reg) = {
    val p = new {override val src = s} with Port with RMPort 
    p.mapTo(reg)
    p
  }
  def apply(s:Node) = new {override val src = s} with Port with RMPort 
}
/** Physical Port of a module. Can map to a pipeline register */
trait Port {
  val src:Node
}
object Port{
  def apply(s: Node) = new {override val src = s} with Port
}

//trait CUPort
//trait CUInPort extends CUPort
//object CUInPort {
//  def apply(reg:Reg) = {
//    val p = new Port() with CUInPort
//    p.mapTo(reg)
//    p
//  }
//}
//trait CUOutPort extends CUPort
//object CUOutPort {
//  def apply(reg:Reg) = {
//    val p = new Port() with CUOutPort
//    p.mapTo(reg)
//    p
//  }
//}

