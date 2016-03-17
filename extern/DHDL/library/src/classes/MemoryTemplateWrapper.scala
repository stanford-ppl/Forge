package dhdl.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}

import scala.math.BigDecimal.RoundingMode

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.library._
import dhdl.library.classes._

trait TypeInspectionWrapper extends TypeInspectionCompilerOps {
  this: DHDL =>
}

trait MemoryTemplateWrapper extends PipeTemplateWrapper with NumEmulation {
  this: DHDLBase with DHDLClasses =>

  // Memories are all equivalent to Scala Arrays in library
  type OffChipMem[T] = Array[T]
  type BRAM[T] = Array[T]
  type Reg[T] = Array[T]
  type Counter = FixedPointRange
  type CounterChain = Array[FixedPointRange]
  type Pipeline = Unit

  type Bit = Boolean
  type FixPt[SIGN,INT,FRAC] = FixedPoint
  type FltPt[SIG,EXP] = FloatPoint

  def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]] = manifest[Array[T]]
  def bramManifest[T:Manifest]: Manifest[BRAM[T]] = manifest[Array[T]]
  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[Array[T]]
  def counterManifest: Manifest[Counter] = manifest[FixedPointRange]
  def counterChainManifest: Manifest[CounterChain] = manifest[Array[FixedPointRange]]
  def pipelineManifest: Manifest[Pipeline] = manifest[Unit]

  def fixManifest[S:Manifest,I:Manifest,F:Manifest] = manifest[FixedPoint]
  def fltManifest[G:Manifest,E:Manifest] = manifest[FloatPoint]
  def bitManifest: Manifest[Bit] = manifest[Boolean]

  def isFixPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FixedPoint])
  def isFltPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FloatPoint])
  def isBitType[T:Manifest]   = isSubtype(manifest[T].runtimeClass, classOf[Boolean])

  // Note that tileDims are not fixed point values yet - they're just integers
  /*private def localDimsToStrides(dims: List[Int]) = List.tabulate(dims.length){d =>
    if (d == dims.length - 1) 1
    else dims.drop(d + 1).reduce(_*_)
  }*/

  def tile_transfer[T:Manifest](mem: Rep[OffChipMem[T]], local: Rep[BRAM[T]], strides: List[Rep[Index]], memOfs: Rep[Index], tileDims: List[Int], cchain: Rep[CounterChain], store: Boolean)(implicit ctx: SourceContext): Rep[Unit] = {
    val localStrides = sdimsToStrides(tileDims).map(k => FixedPoint(k)(FixFormat(true,32,0)))

    loop(cchain, 0, Nil, {iters: Rep[Indices] =>
      val offaddr   = List.tabulate(cchain.length){i => indices_getindex(iters, i) * strides(i) }.fold(memOfs)(_+_)
      val localaddr = List.tabulate(cchain.length){i => indices_getindex(iters, i) * localStrides(i) }.reduce(_+_)
      if (store)
        mem(offaddr.toInt) = local(localaddr.toInt)
      else
        local(localaddr.toInt) = mem(offaddr.toInt)
    })
  }
}


trait NumEmulation extends FixedPointEmulation with FloatingPointEmulation

// Defines class for emulating arbitrary fixed point
// Note that all computation is boxed here and done with BigInt for generality. Probably not the best performance
trait FixedPointEmulation { this: NumEmulation =>

  case class FixFormat(signed: Boolean, m: Int, f: Int) {
    def bits = m + f
    lazy val maxValue = if (signed) (BigInt(1) << (bits-1)) - 1 else (BigInt(1) << bits) - 1
    lazy val minValue = if (signed) -(BigInt(1) << (bits-1))    else BigInt(0)
  }
  private def check(a: FixFormat, b: FixFormat) {
    if (a != b) throw new Exception("Operations on mismatched fixed point representations are unsupported")
  }

  // Could use NumericRange, but there's an absolutely excessive amount of stuff that needs to be defined in a type class to
  // get that off the ground. Going the quicky and dirty route for now.
  case class FixedPointRange(start: FixedPoint, end: FixedPoint, step: FixedPoint) {
    def foreach(func: FixedPoint => Unit) = {
      var i = start
      while (i < end) {
        func(i)
        i += step
      }
    }
    def by(s: FixedPoint) = FixedPointRange(start, end, s)
  }


  class FixedPoint(private val v: BigInt)(implicit format: FixFormat) {
    private def rep = this.format

    def unary_-() = { FixedPoint( -this.v ) }
    def +(that: FixedPoint) = { check(this.rep, that.rep); FixedPoint(this.v + that.v) }
    def -(that: FixedPoint) = { check(this.rep, that.rep); FixedPoint(this.v - that.v) }
    def *(that: FixedPoint) = { check(this.rep, that.rep); FixedPoint( (this.v * that.v) >> rep.f) }
    def /(that: FixedPoint) = { check(this.rep, that.rep); FixedPoint( (this.v << rep.f) / that.v ) }
    def %(that: FixedPoint) = {
      check(this.rep, that.rep)
      if (rep.f != 0) throw new Exception("Modulus on non-integer fixed point values currently unsupported")
      FixedPoint( this.v % that.v )
    }
    def <(that: FixedPoint) = { check(this.rep, that.rep); this.v < that.v }
    def >(that: FixedPoint) = { check(this.rep, that.rep); this.v > that.v }
    def <=(that: FixedPoint) = { check(this.rep, that.rep); this.v <= that.v }
    def >=(that: FixedPoint) = { check(this.rep, that.rep); this.v >= that.v }
    override def equals(that: Any) = that match {
      case that: FixedPoint =>
        check(this.rep, that.rep)
        this.v == that.v
      case _ => false
    }
    def &(that: FixedPoint) = { check(this.rep, that.rep); FixedPoint(this.v & that.v) }
    def |(that: FixedPoint) = { check(this.rep, that.rep); FixedPoint(this.v | that.v) }

    def <<(that: FixedPoint) = {
      if (that.rep.f != 0) throw new Exception("Cannot shift left by a fractional amount")
      FixedPoint(this.v << that.v.intValue)
    }
    def >>(that: FixedPoint) = {
      if (that.rep.f != 0) throw new Exception("Cannot shift right by a fractional amount")
      FixedPoint(this.v >> that.v.intValue)
    }

    def toInt = {
      if (rep.f != 0) throw new Exception("Cannot convert fractional fixed point value to Int")
      v.intValue
    }

    def toFloatPoint(implicit format: FloatFormat) = {
      val value = BigDecimal(v >> rep.f) + (BigDecimal(v & ((BigInt(1) << rep.f) - 1)) / BigDecimal(BigInt(1) << rep.f))
      FloatPoint(value)
    }
    def changeFormat(rep2: FixFormat) = {
      if (rep2.f > rep.f)
        FixedPoint(v << (rep2.f - rep.f))(rep2)
      else
        FixedPoint(v >> (rep.f - rep2.f))(rep2)
    }

    override def toString() = {
      if (format.f > 0) {
        (v >> format.f).toString + "." + (BigDecimal(v & ((BigInt(1) << format.f) - 1)) / BigDecimal(BigInt(1) << format.f)).toString.split('.').last
      }
      else v.toString()
    }

    def until(that: FixedPoint) = {
      check(this.rep, that.rep)
      FixedPointRange(this, that, FixedPoint(1)(rep))
    }
  }

  object FixedPoint {
    def apply(v: Int)(implicit format: FixFormat): FixedPoint = FixedPoint(BigInt(v) << format.f)
    def apply(v: Long)(implicit format: FixFormat): FixedPoint = FixedPoint(BigInt(v) << format.f)
    def apply(v: Float)(implicit format: FixFormat): FixedPoint = FixedPoint(BigDecimal(v))
    def apply(v: Double)(implicit format: FixFormat): FixedPoint = FixedPoint(BigDecimal(v))

    // TODO: Should support arbitrary rounding here, currently always use default (half even)
    def apply(v: BigDecimal)(implicit format: FixFormat): FixedPoint = {
      FixedPoint((v * (1 << format.f)).setScale(0,RoundingMode.HALF_EVEN).toString)
    }
    def apply(v: String)(implicit format: FixFormat): FixedPoint = FixedPoint(BigDecimal(v))
    def apply(v: BigInt)(implicit format: FixFormat): FixedPoint = {
      var value = v
      // Emulate overflow and underflow
      // TODO: Write this using modulus instead
      while (value < format.minValue) value = format.maxValue + (value - format.minValue) + 1
      while (value > format.maxValue) value = format.minValue + (value - format.maxValue) - 1
      new FixedPoint(value)
    }

    def abs(f: FixedPoint) = FixedPoint(f.v.abs)(f.rep)

    def rand(f: FixedPoint) = {
      FixedPoint(BigInt(java.util.concurrent.ThreadLocalRandom.current().nextLong(f.v.longValue)))(f.rep)
    }
    def rand(implicit format: FixFormat) = {
      FixedPoint(BigInt(java.util.concurrent.ThreadLocalRandom.current().nextLong()))
    }
  }
}


// Defines class for emulating arbitrary floating point
trait FloatingPointEmulation { this: NumEmulation =>

  case class FloatFormat(s: Int, e: Int) {
    def bits = s + e
    lazy val maxValue = 0
    lazy val minValue = 1
  }
  private def check(a: FloatFormat, b: FloatFormat) = {
    if (a != b) throw new Exception("Operations on mismatched floating point representations are unsupported")
  }

  class FloatPoint(private val v: BigDecimal)(implicit format: FloatFormat) {
    private def rep = format

    def unary_-() = { FloatPoint( -this.v ) }
    def +(that: FloatPoint) = { check(this.rep, that.rep); FloatPoint(this.v + that.v) }
    def -(that: FloatPoint) = { check(this.rep, that.rep); FloatPoint(this.v - that.v) }
    def *(that: FloatPoint) = { check(this.rep, that.rep); FloatPoint(this.v * that.v) }
    def /(that: FloatPoint) = { check(this.rep, that.rep); FloatPoint(this.v / that.v) }

    def <(that: FloatPoint) = { check(this.rep, that.rep); this.v < that.v }
    def >(that: FloatPoint) = { check(this.rep, that.rep); this.v > that.v }
    def <=(that: FloatPoint) = { check(this.rep, that.rep); this.v <= that.v }
    def >=(that: FloatPoint) = { check(this.rep, that.rep); this.v >= that.v }
    override def equals(that: Any) = that match {
      case that: FloatPoint =>
        check(this.rep, that.rep)
        this.v == that.v
      case _ => false
    }

    def toFixedPoint(implicit format: FixFormat): FixedPoint = FixedPoint(v)
    def changeFormat(rep2: FloatFormat) = FloatPoint(v)(rep2)

    override def toString() = v.toString()
  }

  object FloatPoint {
    def apply(v: Int)(implicit format: FloatFormat): FloatPoint = FloatPoint(BigDecimal(v))
    def apply(v: Long)(implicit format: FloatFormat): FloatPoint = FloatPoint(BigDecimal(v))
    def apply(v: Float)(implicit format: FloatFormat): FloatPoint = FloatPoint(BigDecimal(v))
    def apply(v: Double)(implicit format: FloatFormat): FloatPoint = FloatPoint(BigDecimal(v))
    def apply(v: String)(implicit format: FloatFormat): FloatPoint = FloatPoint(BigDecimal(v))

    // TODO: Support overflow/underflow and precision
    def apply(v: BigDecimal)(implicit format: FloatFormat): FloatPoint = {
      new FloatPoint(v)
    }

    def abs(f: FloatPoint) = FloatPoint(f.v.abs)(f.rep)

    // TODO: Just using double precision right now - no default library implementation of these :(
    def log(f: FloatPoint) = FloatPoint(Math.log(f.v.doubleValue))(f.rep)
    def exp(f: FloatPoint) = FloatPoint(Math.exp(f.v.doubleValue))(f.rep)
    def sqrt(f: FloatPoint) = FloatPoint(Math.sqrt(f.v.doubleValue))(f.rep)

    def rand(implicit format: FloatFormat) = {
      FloatPoint(java.util.concurrent.ThreadLocalRandom.current().nextDouble())
    }
  }
}
