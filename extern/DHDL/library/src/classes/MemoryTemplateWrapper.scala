package dhdl.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.library._
import dhdl.library.classes._

trait MemoryTemplateWrapper {
  this: DHDLBase with DHDLClasses =>

  // Memories are all equivalent to Scala Arrays in library
  type OffChipMem[T] = Array[T]
  type BRAM[T] = Array[T]
  type Reg[T] = Array[T]
  type Counter = scala.collection.immutable.NumericRange[Long]
  type CounterChain = Array[Counter]
  type Pipeline = Unit

  type Bit = Boolean
  type Fix = Long
  type Flt = Double

  def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]] = manifest[Array[T]]
  def bramManifest[T:Manifest]: Manifest[BRAM[T]] = manifest[Array[T]]
  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[Array[T]]
  def counterManifest: Manifest[Counter] = manifest[scala.collection.immutable.NumericRange[Long]]
  def counterChainManifest: Manifest[CounterChain] = manifest[Array[scala.collection.immutable.NumericRange[Long]]]
  def pipelineManifest: Manifest[Pipeline] = manifest[Unit]

  def fixManifest: Manifest[Fix] = manifest[Long]
  def fltManifest: Manifest[Flt] = manifest[Double]
  def bitManifest: Manifest[Bit] = manifest[Boolean]
}
