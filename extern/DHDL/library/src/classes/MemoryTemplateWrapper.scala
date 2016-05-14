package dhdl.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,RefinedManifest,SourceContext}
import scala.virtualization.lms.common.Record

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.library._
import dhdl.library.classes._

trait MemoryTemplateWrapper extends ControllerTemplateWrapper with ExternPrimitiveWrapper {
  this: DHDLBase with DHDLClasses =>

  // Memories are all equivalent to Scala Arrays in library
  type OffChipMem[T] = Array[T]
  type BRAM[T] = Array[T]
  type Vector[T] = Array[T]
  type Reg[T] = Array[T]
  type Pipeline = Unit
  type Indices = RecordImpl

  // Library can't definitively tell the difference between BRAM and Reg
  def isBRAM[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[Array[_]])
  def isRegister[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[Array[_]])

  def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]] = manifest[Array[T]]
  def bramManifest[T:Manifest]: Manifest[BRAM[T]] = manifest[Array[T]]
  def vectorManifest[T:Manifest]: Manifest[Vector[T]] = manifest[Array[T]]
  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[Array[T]]
  def pipelineManifest: Manifest[Pipeline] = manifest[Unit]
  def indicesManifest: Manifest[Indices] = manifest[RecordImpl]

  def vector_from_list[T:Manifest](elems: List[Rep[T]])(implicit ctx: SourceContext): Rep[Vector[T]] = elems.toArray

  def bram_load_vector[T:Manifest](bram: Rep[BRAM[T]], offsets: List[Rep[FixPt[Signed,B32,B0]]], len: Rep[FixPt[Signed,B32,B0]], cchain: Rep[CounterChain])(implicit ctx: SourceContext): Rep[Vector[T]] = {
    val dims = cchain.map(ctr => ctr.len).toList  // NOTE: This wouldn't work in compiler
    val vec = array_empty[T](dims.reduce{_*_}.toInt)
    loopList(cchain, 0, Nil, {i: List[Rep[FixPt[Signed,B32,B0]]] =>
      val bramAddr = calcAddress(offsets.zip(i).map{case (a,b) => a+b}, dimsOf(bram))
      val vecAddr = calcAddress(i, dims)
      vec(vecAddr.toInt) = bram(bramAddr.toInt)
    })
    vec
  }

  def bram_store_vector[T:Manifest](bram: Rep[BRAM[T]], offsets: List[Rep[FixPt[Signed,B32,B0]]], vec: Rep[Vector[T]], cchain: Rep[CounterChain])(implicit ctx: SourceContext): Rep[Unit] = {
    val dims = cchain.map{ctr => ctr.len}.toList // NOTE: This wouldn't work in compiler
    loopList(cchain, 0, Nil, {i: List[Rep[FixPt[Signed,B32,B0]]] =>
      val bramAddr = calcAddress(offsets.zip(i).map{case (a,b) => a + b}, dimsOf(bram))
      val vecAddr = calcAddress(i, dims)
      bram(bramAddr.toInt) = vec(vecAddr.toInt)
    })
  }
}
