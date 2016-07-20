package dhdl.plasticine.config

import dhdl.plasticine.graph._

object Config0 extends Spade {

  // Assume all CUs are homogenous for now

   // Inner CU Specs
  override val wordWidth = 32
  override val numLanes = 4
  
  private val cus = List.tabulate(5) { i =>
    val numPRs = 40
    val numCtrs = 10
    val numSRAMs = 2
    val numInPorts = numLanes * numSRAMs
    val numOutPorts = numLanes 
    // Create Pipeline Regs (entire row of physicall register for all stages)
    // No overlapping between mappings
    val regs = List.tabulate(numPRs) { ir => Reg() }
    val ctrs = List.tabulate(numCtrs) { ic => Counter(regs(ic)) }
    val srams = List.tabulate(numSRAMs) { is => SRAM(numLanes, regs(is + numCtrs), regs(is + numCtrs)) } 
    val inRegs = List.tabulate(numInPorts) { ip => regs(ip + numCtrs + numSRAMs) }
    val outRegs = List.tabulate(numOutPorts) { ip => regs(ip + numCtrs + numSRAMs + numInPorts) }
    ComputeUnit(regs, srams, ctrs, inRegs, outRegs, regs(numCtrs + numSRAMs + numInPorts + numOutPorts))
  } 

  private val memCtrls = List.tabulate(4) { i =>
    val numPRs = 40
    val numCtrs = 10
    val numSRAMs = 2
    val numInPorts = numLanes * numSRAMs
    val numOutPorts = numLanes 
    // Create Pipeline Regs (entire row of physicall register for all stages)
    // No overlapping between mappings
    val regs = List.tabulate(numPRs) { ir => Reg() }
    val ctrs = List.tabulate(numCtrs) { ic => Counter(regs(ic)) }
    val srams = List.tabulate(numSRAMs) { is => SRAM(numLanes, regs(is + numCtrs), regs(is + numCtrs)) } 
    val inRegs = List.tabulate(numInPorts) { ip => regs(ip + numCtrs + numSRAMs) }
    val outRegs = List.tabulate(numOutPorts) { ip => regs(ip + numCtrs + numSRAMs + numInPorts) }
    MemoryController(regs, srams, ctrs, inRegs, outRegs, regs(numCtrs + numSRAMs + numInPorts + numOutPorts))
  }

  override val computeUnits = cus ++ memCtrls 

}
