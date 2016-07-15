package dhdl.plasticine.config

import dhdl.plasticine.graph._

trait Spade {
  val numCU:Int
  val numMemCtrls:Int
  val numLane:Int
  val numReg:Int
  val numStage:Int
  val numCtr:Int
  val numSRAM:Int
  val numInPort:Int
  val numOutPort:Int
  val wordWidth:Int
}
