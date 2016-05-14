package dhdl.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._

trait MemoryTemplateTypes {
  type OffChipMem[T]
  type BRAM[T]
  type Vector[T]
  type Reg[T]

  type Pipeline
  type Indices

  def isBRAM[T:Manifest]: Boolean
  def isRegister[T:Manifest]: Boolean

  implicit def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]]
  implicit def bramManifest[T:Manifest]: Manifest[BRAM[T]]
  implicit def vectorManifest[T:Manifest]: Manifest[Vector[T]]
  implicit def regManifest[T:Manifest]: Manifest[Reg[T]]
  implicit def pipelineManifest: Manifest[Pipeline]
  implicit def indicesManifest: Manifest[Indices]
}

trait MemoryTemplateOps extends MemoryTemplateTypes with Base {
  this: DHDL =>
}
trait MemoryTemplateCompilerOps extends MemoryTemplateOps {
  this: DHDL =>

  def vector_from_list[T:Manifest](elems: List[Rep[T]])(implicit ctx: SourceContext): Rep[Vector[T]]

  def bram_load_vector[T:Manifest](bram: Rep[BRAM[T]], offsets: List[Rep[FixPt[Signed,B32,B0]]], len: Rep[FixPt[Signed,B32,B0]], cchain: Rep[CounterChain])(implicit ctx: SourceContext): Rep[Vector[T]]
  def bram_store_vector[T:Manifest](bram: Rep[BRAM[T]], offsets: List[Rep[FixPt[Signed,B32,B0]]], vec: Rep[Vector[T]], cchain: Rep[CounterChain])(implicit ctx: SourceContext): Rep[Unit]
}

