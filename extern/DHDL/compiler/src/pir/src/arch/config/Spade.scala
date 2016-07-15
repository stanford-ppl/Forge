package dhdl.plasticine.config

import dhdl.plasticine.graph._

abstract class Spade {
  val wordWidth:Int
  val numLanes:Int

  val computeUnits:List[ComputeUnit]
  def numCUs = computeUnits.size
}
