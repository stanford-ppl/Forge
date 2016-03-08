package dhdl.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect}
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait BlockRAM[T]
trait Register[T]
trait DRAM[T]

trait DHDLCounter
trait DHDLCounterChain
trait DHDLPipeline

trait DHDLBit
trait DHDLFix
trait DHDLFlt

trait MemoryTemplateOpsExp extends MemoryTemplateOps with EffectExp {
  this: DHDLExp =>

  type OffChipMem[T] = DRAM[T]
  type BRAM[T] = BlockRAM[T]
  type Reg[T] = Register[T]

  type Counter = DHDLCounter
  type CounterChain = DHDLCounterChain
  type Pipeline = DHDLPipeline

  type Bit = DHDLBit
  type Fix = DHDLFix
  type Flt = DHDLFlt

  def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]] = manifest[DRAM[T]]
  def bramManifest[T:Manifest]: Manifest[BRAM[T]] = manifest[BlockRAM[T]]
  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[Register[T]]
  def counterManifest: Manifest[Counter] = manifest[DHDLCounter]
  def counterChainManifest: Manifest[CounterChain] = manifest[DHDLCounterChain]
  def pipelineManifest: Manifest[Pipeline] = manifest[DHDLPipeline]

  def fixManifest: Manifest[Fix] = manifest[DHDLFix]
  def fltManifest: Manifest[Flt] = manifest[DHDLFlt]
  def bitManifest: Manifest[Bit] = manifest[DHDLBit]
}

// Defines type remappings required in Scala gen (should be same as in library)
trait ScalaGenMemoryTemplateOps extends ScalaGenEffect {
  val IR: MemoryTemplateOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "BlockRAM" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "Register" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DRAM"     => "Array[" + remap(m.typeArguments(0)) + "]"

    case "DHDLCounter" => "scala.collection.immutable.NumericRange[Long]"
    case "DHDLCounterChain" => "Array[scala.collection.immutable.NumericRange[Long]]"
    case "DHDLPipeline" => "Unit"

    case "DHDLBit" => "Boolean"
    case "DHDLFix" => "Long"
    case "DHDLFlt" => "Double"
    case _ => super.remap(m)
  }
}