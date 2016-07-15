package dhdl.plasticine.config

import dhdl.plasticine.graph._

object Config0 extends Spade {

  // Assume all CUs are homogenous for now
  
  // Intra CU specs
  override val numCU = 2
  override val numMemCtrls = 2

   // Inner CU Specs
  override val numLane = 4
  override val numReg = 10
  override val numStage = 5
  override val numCtr = 6
  override val numSRAM = 2
  override val numInPort = numLane * 2
  override val numOutPort = numLane
  override val wordWidth = 32
  
  val cus = List.tabulate(numCU) { i =>
    val regs = List.tabulate(numReg) { ir =>
      PReg()
    }
    val ctrs = List.tabulate(numCtr) { ic =>
      val c = PCounter()
      c.mapTo(regs(ic))
      c
    }
    val srams = List.tabulate(numSRAM) {is =>
      val s = PSRAM(numLane)
      s.mapTo(regs(is + numCtr))
      s
    } 
    val inports = List.tabulate(numInPort) {ip =>
      InPort()
    }
    val outports = List.tabulate(numOutPort) {ip =>
      InPort()
    }
    PComputeUnit(regs, srams, ctrs, inports, outports)
  } 

  val memCtrls = List.tabulate(numMemCtrls) { i =>
    val regs = List.tabulate(numReg) { ir =>
      PReg()
    }
    val ctrs = List.tabulate(numCtr) { ic =>
      val c = PCounter()
      c.mapTo(regs(ic))
      c
    }
    val srams = List.tabulate(numSRAM) {is =>
      val s = PSRAM(numLane)
      s.mapTo(regs(is + numCtr))
      s
    } 
    val inports = List.tabulate(numInPort) {ip =>
      InPort()
    }
    val outports = List.tabulate(numOutPort) {ip =>
      InPort()
    }
    PMemoryController(regs, srams, ctrs, inports, outports)
  }
}
