package dhdl.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._

trait TypeInspectionOps // Stub
trait TypeInspectionCompilerOps extends TypeInspectionOps with MemoryTemplateCompilerOps {
  this: DHDLIdentifiers =>

  lazy val bx = "B([0-9]+)".r
  object BXX {
    // HACK: Given Manifest, match using regex bx, return * (where * must be numeric)
    def unapply[T](x: Manifest[T]): Option[Int] = x.runtimeClass.getSimpleName match {
      case bx(bits) => Some(bits.toInt)
      case _ => None
    }
  }

  def sign[T:Manifest]: Boolean = manifest[T] match {
    case mA if isFixPtType(mA) => sign(mA.typeArguments(0))
    case mA if isFltPtType(mA) => true
    case mA if mA == manifest[Signed] => true
    case mA if mA == manifest[Unsign] => false
    case mA => throw new Exception("Unknown type in signed test: " + mA.runtimeClass.getSimpleName)
  }
  def nbits[T:Manifest]: Int = manifest[T] match {
    case mA if isFixPtType(mA) => nbits(mA.typeArguments(1)) + nbits(mA.typeArguments(2))
    case mA if isFltPtType(mA) => nbits(mA.typeArguments(0)) + nbits(mA.typeArguments(1))
    case mA if isBitType(mA) => 1
    case mA if isRegister(mA) => nbits(mA.typeArguments(0))
    case BXX(bits) => bits
    case mA => throw new Exception("Unknown type in nbits: " + mA.runtimeClass.getSimpleName)
  }

  def isFixPtType[T:Manifest]: Boolean
  def isFltPtType[T:Manifest]: Boolean
  def isBitType[T:Manifest]: Boolean
  def isRegister[T:Manifest]: Boolean
}


trait MemoryTemplateTypes {
  type OffChipMem[T]
  type BRAM[T]
  type Vector[T]
  type Reg[T]

  type Counter
  type CounterChain
  type Pipeline
  type Indices

  // MUST be invariant (otherwise can get something like Fix[Any,Any,Any] which is not great)
  type FixPt[SIGN,INT,FRAC]
  type FltPt[SIG,EXP] // Sign bit is included in bits for significand
  type Bit // Left undefined here so we can use Boolean for emulation in library if needed

  implicit def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]]
  implicit def bramManifest[T:Manifest]: Manifest[BRAM[T]]
  implicit def vectorManifest[T:Manifest]: Manifest[Vector[T]]
  implicit def regManifest[T:Manifest]: Manifest[Reg[T]]
  implicit def counterManifest: Manifest[Counter]
  implicit def counterChainManifest: Manifest[CounterChain]
  implicit def pipelineManifest: Manifest[Pipeline]
  implicit def indicesManifest: Manifest[Indices]
  implicit def fixManifest[S:Manifest,I:Manifest,F:Manifest]: Manifest[FixPt[S,I,F]]
  implicit def fltManifest[G:Manifest,E:Manifest]: Manifest[FltPt[G,E]]
  implicit def bitManifest: Manifest[Bit]
}

trait MemoryTemplateOps extends MemoryTemplateTypes with Base {
  this: DHDLIdentifiers =>
}
trait MemoryTemplateCompilerOps extends MemoryTemplateOps {
  this: DHDLIdentifiers =>

  def vector_from_list[T:Manifest](elems: List[Rep[T]])(implicit ctx: SourceContext): Rep[Vector[T]]
}

