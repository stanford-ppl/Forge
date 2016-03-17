package dhdl.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._

trait TypeInspectionOps // Stub
trait TypeInspectionCompilerOps extends MemoryTemplateCompilerOps with DHDLMiscInternalOps {
  this: DHDL =>

  lazy val bx = "B([0-9]+)".r
  object BXX {
    // HACK: Given Manifest, match using regex bx, return * (where * must be numeric)
    def unapply[T](x: Manifest[T]): Option[Int] = x.runtimeClass.getSimpleName match {
      case bx(bits) => Some(bits.toInt)
      case _ => None
    }
  }

  def sign[T:Manifest]: Boolean = manifest[T] match {
    case mA if mA == manifest[Signed] => true
    case mA if mA == manifest[Unsign] => false
    case mA => stageError("Unknown type in signed test: " + mA.runtimeClass.getSimpleName)
  }
  def nbits[T:Manifest]: Int = manifest[T] match {
    case mA if isFixPtType(mA) => nbits(mA.typeArguments(1)) + nbits(mA.typeArguments(2))
    case mA if isFltPtType(mA) => nbits(mA.typeArguments(0)) + nbits(mA.typeArguments(1))
    case mA if isBitType(mA) => 1
    case BXX(bits) => bits
    case mA => stageError("Unknown type in nbits: " + mA.runtimeClass.getSimpleName)
  }

  def isFixPtType[T:Manifest]: Boolean
  def isFltPtType[T:Manifest]: Boolean
  def isBitType[T:Manifest]: Boolean
}


trait MemoryTemplateOps extends Base {
  this: DHDL =>

  type OffChipMem[T]
  type BRAM[T]
  type Reg[T]

  type Counter
  type CounterChain
  type Pipeline

  // MUST be invariant (otherwise can get something like Fix[Any,Any,Any] which is not great)
  type FixPt[SIGN,INT,FRAC]
  type FltPt[SIG,EXP] // Sign bit is included in bits for significand
  type Bit // Left undefined here so we can use Boolean for emulation in library if needed

  type Index = FixPt[Signed,B32,B0]

  implicit def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]]
  implicit def bramManifest[T:Manifest]: Manifest[BRAM[T]]
  implicit def regManifest[T:Manifest]: Manifest[Reg[T]]
  implicit def counterManifest: Manifest[Counter]
  implicit def counterChainManifest: Manifest[CounterChain]
  implicit def pipelineManifest: Manifest[Pipeline]
  implicit def fixManifest[S:Manifest,I:Manifest,F:Manifest]: Manifest[FixPt[S,I,F]]
  implicit def fltManifest[G:Manifest,E:Manifest]: Manifest[FltPt[G,E]]
  implicit def bitManifest: Manifest[Bit]

  def tile_transfer[T:Manifest](mem: Rep[OffChipMem[T]], local: Rep[BRAM[T]], strides: List[Rep[Index]], memOfs: Rep[Index], tileDims: List[Int], cchain: Rep[CounterChain], store: Boolean)(implicit ctx: SourceContext): Rep[Unit]

}
trait MemoryTemplateCompilerOps extends MemoryTemplateOps { this: DHDL => }

