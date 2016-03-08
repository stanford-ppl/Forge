package dhdl.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._

trait MemoryTemplateOps extends Base {
  this: DHDL =>

  type OffChipMem[T]
  type BRAM[T]
  type Reg[T]

  type Counter
  type CounterChain
  type Pipeline

  type Fix
  type Flt
  type Bit

  /*abstract class Bits {
    def precision: Precision
  }

  abstract class NumericPt[T:Numeric] extends Bits {
    val value: T = implicitly[Numeric[T]].zero
  }

  abstract class Fix(val s: Boolean, val m: Int, val f: Int) extends NumericPt[Long] {
    def precision = FixedPoint(s, m, f)
  }
  abstract class SFix(override val m: Int, override val f: Int) extends FixPt(true, m, f)
  abstract class UFix(override val m: Int, override val f: Int) extends FixPt(false, m, f)

  abstract class Flt(val s: Boolean, val m: Int, val e: Int) extends NumericPt[Double]*/

  implicit def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]]
  implicit def bramManifest[T:Manifest]: Manifest[BRAM[T]]
  implicit def regManifest[T:Manifest]: Manifest[Reg[T]]
  implicit def counterManifest: Manifest[Counter]
  implicit def counterChainManifest: Manifest[CounterChain]
  implicit def pipelineManifest: Manifest[Pipeline]
  implicit def fixManifest: Manifest[Fix]
  implicit def fltManifest: Manifest[Flt]
  implicit def bitManifest: Manifest[Bit]
}
trait MemoryTemplateCompilerOps extends MemoryTemplateOps { this: DHDL => }
