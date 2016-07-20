package dhdl.graph.mapping
import dhdl.graph.{ComputeUnit => CU, MemoryController => MC, _}
import dhdl.Design
import dhdl.Config
import dhdl.plasticine.config._
import dhdl.plasticine.graph.{ComputeUnit => PCU, MemoryController => PMC}

import scala.collection.immutable.Set
import scala.collection.immutable.HashMap

trait Hint {
  val msg:String
  override def toString = s"Hint(${msg})"
}
case class IntConnct(cu:CU, pcu:PCU) extends Hint {
  override val msg = s"Fail to map ${cu} on ${pcu} due to interconnection constrain"
}
trait OutOfResource extends Hint
case class OutOfPMC(arch:Spade) extends OutOfResource {
  override val msg = s"Not enough MemoryControllers in ${arch} to map application"
} 
case class OutOfPCU(arch:Spade) extends OutOfResource {
  override val msg = s"Not enough ComputeUnits in ${arch} to map application"
} 
case class OutOfCtr(pcu:PCU) extends OutOfResource {
  override val msg = s"Not enough Counters in ${pcu} to map application"
}
case class OutOfSram(pcu:PCU) extends OutOfResource {
  override val msg = s"Not enough SRAMs in ${pcu} to map application"
}

