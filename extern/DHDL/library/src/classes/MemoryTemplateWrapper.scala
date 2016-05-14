package dhdl.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,RefinedManifest,SourceContext}
import scala.virtualization.lms.common.Record

import scala.math.BigDecimal.RoundingMode

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.library._
import dhdl.library.classes._

trait TypeInspectionWrapper extends TypeInspectionCompilerOps {
  this: DHDLIdentifiers =>
}

trait MemoryTemplateWrapper extends ControllerTemplateWrapper with TypeInspectionWrapper with NumEmulation {
  this: DHDLBase with DHDLClasses =>

  // Memories are all equivalent to Scala Arrays in library
  type OffChipMem[T] = Array[T]
  type BRAM[T] = Array[T]
  type Vector[T] = Array[T]
  type Reg[T] = Array[T]
  type Counter = FixedPointRange[Signed,B32,B0]
  type CounterChain = Array[FixedPointRange[Signed,B32,B0]]
  type Pipeline = Unit
  type Indices = RecordImpl

  type Bit = Boolean
  type FixPt[SIGN,INT,FRAC] = FixedPoint[SIGN,INT,FRAC]
  type FltPt[SIG,EXP] = FloatPoint[SIG,EXP]

  def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]] = manifest[Array[T]]
  def bramManifest[T:Manifest]: Manifest[BRAM[T]] = manifest[Array[T]]
  def vectorManifest[T:Manifest]: Manifest[Vector[T]] = manifest[Array[T]]
  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[Array[T]]
  def counterManifest: Manifest[Counter] = manifest[FixedPointRange[Signed,B32,B0]]
  def counterChainManifest: Manifest[CounterChain] = manifest[Array[FixedPointRange[Signed,B32,B0]]]
  def pipelineManifest: Manifest[Pipeline] = manifest[Unit]
  def indicesManifest: Manifest[Indices] = manifest[RecordImpl]

  // Library implementation needs to also have type parameters
  def fixManifest[S:Manifest,I:Manifest,F:Manifest]: Manifest[FixPt[S,I,F]] = manifest[FixedPoint[S,I,F]]
  def fltManifest[G:Manifest,E:Manifest]: Manifest[FltPt[G,E]] = manifest[FloatPoint[G,E]]
  def bitManifest: Manifest[Bit] = manifest[Boolean]

  def isFixPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FixedPoint[_,_,_]])
  def isFltPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FloatPoint[_,_]])
  def isBitType[T:Manifest]   = isSubtype(manifest[T].runtimeClass, classOf[Boolean])
  def isRegister[T:Manifest]  = isSubtype(manifest[T].runtimeClass, classOf[Array[_]])  // eh...

  def vector_from_list[T:Manifest](elems: List[Rep[T]])(implicit ctx: SourceContext): Rep[Vector[T]] = elems.toArray

  def bram_load_vector[T:Manifest](bram: Rep[BRAM[T]], offsets: List[Rep[FixPt[Signed,B32,B0]]], len: Rep[FixPt[Signed,B32,B0]], cchain: Rep[CounterChain])(implicit ctx: SourceContext): Rep[Vector[T]] = {
    val dims = cchain.map(ctr => ctr.len)
    val vec = array_empty[T](dims.reduce{_*_}.toInt)
    loop(cchain, 0, Nil, {i: Rep[Indices] =>
      val bramAddr = calcAddress(offsets.zip(i.toList), dimsOf(bram))
      val vecAddr = calcAddress(i.toList, dims)
      vec(vecAddr) = bram(bramAddr)
    })
    vec
  }

  def bram_store_vector[T:Manifest](bram: Rep[BRAM[T]], offsets: List[Rep[FixPt[Signed,B32,B0]]], vec: Rep[Vector[T]], cchain: Rep[CounterChain])(implicit ctx: SourceContext): Rep[Unit] = {
    val dims = cchain.map{ctr => ctr.len}
    loop(cchain, 0, Nil, {i: Rep[Indices] =>
      val bramAddr = calcAddress(offsets.zip(i.toList), dimsOf(bram))
      val vecAddr = calcAddress(i.toList, dims)
      bram(bramAddr) = vec(vecAddr)
    })
  }
}


trait NumEmulation extends FixedPointEmulation with FloatingPointEmulation {
  this: TypeInspectionCompilerOps =>
}

// Defines class for emulating arbitrary fixed point
// Note that all computation is boxed here and done with BigInt for generality. Probably not the best performance
trait FixedPointEmulation {
  this: NumEmulation with TypeInspectionCompilerOps =>

  case class FixFormat(signed: Boolean, m: Int, f: Int) {
    def bits = m + f
    lazy val maxValue = if (signed) (BigInt(1) << (bits-1)) - 1 else (BigInt(1) << bits) - 1
    lazy val minValue = if (signed) -(BigInt(1) << (bits-1))    else BigInt(0)
  }
  private def check(a: FixFormat, b: FixFormat) {
    if (a != b) {
      throw new Exception("Operations on mismatched fixed point representations (" + a + " versus " + b + ") are unsupported")
    }
  }

  // Could use NumericRange, but there's an absolutely excessive amount of stuff that needs to be defined in a type class to
  // get that off the ground. Going the quicky and dirty route for now.
  case class FixedPointRange[S:Manifest,I:Manifest,F:Manifest](start: FixedPoint[S,I,F], end: FixedPoint[S,I,F], step: FixedPoint[S,I,F]) {
    def foreach(func: FixedPoint[S,I,F] => Unit) = {
      var i = start
      while (i < end) {
        func(i)
        i += step
      }
    }
    def by(s: FixedPoint[S,I,F]) = FixedPointRange[S,I,F](start, end, s)

    def len = (end - start)/step
  }


  class FixedPoint[S:Manifest,I:Manifest,F:Manifest](private val v: BigInt) {
    private def rep = FixFormat(sign[S],nbits[I],nbits[F])

    def unary_-() = { FixedPoint[S,I,F]( -this.v ) }
    def +(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F](this.v + that.v) }
    def -(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F](this.v - that.v) }
    def *(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F]( (this.v * that.v) >> rep.f) }
    def /(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F]( (this.v << rep.f) / that.v ) }
    def %(that: FixedPoint[S,I,F]) = {
      if (nbits[F] > 0) throw new Exception("Modulus on non-integer fixed point values currently unsupported")
      FixedPoint[S,I,F]( this.v % that.v )
    }
    def <(that: FixedPoint[S,I,F]) = { this.v < that.v }
    def >(that: FixedPoint[S,I,F]) = { this.v > that.v }
    def <=(that: FixedPoint[S,I,F]) = { this.v <= that.v }
    def >=(that: FixedPoint[S,I,F]) = { this.v >= that.v }
    override def equals(that: Any) = that match {
      case that: FixedPoint[_,_,_] =>
        check(this.rep, that.rep)
        this.v == that.v
      case _ => false
    }
    def &(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F](this.v & that.v) }
    def |(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F](this.v | that.v) }

    def <<[F2:Manifest](that: FixedPoint[S,I,F2]) = {
      if (nbits[F2] > 0) throw new Exception("Cannot shift left by a fractional amount")
      FixedPoint[S,I,F](this.v << that.v.intValue)
    }
    def >>[F2:Manifest](that: FixedPoint[S,I,F2]) = {
      if (nbits[F2] > 0) throw new Exception("Cannot shift right by a fractional amount")
      FixedPoint[S,I,F](this.v >> that.v.intValue)
    }

    def toInt = {
      if (nbits[F] > 0) {
        throw new Exception("Cannot convert fractional fixed point value (FixedPoint[" +
          manifest[S].runtimeClass.getSimpleName + "," + manifest[I].runtimeClass.getSimpleName + manifest[F].runtimeClass.getSimpleName + "]) to Int")
      }
      v.intValue
    }

    // Really stupid implementation
    def toFloatPoint[G:Manifest,E:Manifest] = {
      val vv = v.abs
      val value = BigDecimal(vv >> rep.f) + (BigDecimal(vv & ((BigInt(1) << rep.f) - 1)) / BigDecimal(BigInt(1) << rep.f))
      FloatPoint[G,E]((if (v < 0) -value else value))
    }
    def changeFormat[S2:Manifest,I2:Manifest,F2:Manifest] = {
      val rep2 = FixFormat(sign[S2],nbits[I2],nbits[F2])
      if (rep2.f > rep.f)
        FixedPoint[S2,I2,F2](v << (rep2.f - rep.f))
      else
        FixedPoint[S2,I2,F2](v >> (rep.f - rep2.f))
    }

    override def toString() = {
      if (rep.f > 0) {
        val vv = v.abs
        val str = (vv >> rep.f).toString + "." + (BigDecimal(vv & ((BigInt(1) << rep.f) - 1)) / BigDecimal(BigInt(1) << rep.f)).toString.split('.').last
        if (v < 0) "-"+str else str
      }
      else v.toString()
    }

    def until(that: FixedPoint[S,I,F]) = {
      check(this.rep, that.rep)
      FixedPointRange(this, that, FixedPoint[S,I,F](1))
    }
  }

  object FixedPoint {
    def apply[S:Manifest,I:Manifest,F:Manifest](v: Int): FixedPoint[S,I,F] = FixedPoint[S,I,F](BigInt(v) << nbits[F])
    def apply[S:Manifest,I:Manifest,F:Manifest](v: Long): FixedPoint[S,I,F] = FixedPoint[S,I,F](BigInt(v) << nbits[F])
    def apply[S:Manifest,I:Manifest,F:Manifest](v: Float): FixedPoint[S,I,F] = FixedPoint[S,I,F](BigDecimal(v))
    def apply[S:Manifest,I:Manifest,F:Manifest](v: Double): FixedPoint[S,I,F] = FixedPoint[S,I,F](BigDecimal(v))

    // TODO: Should support arbitrary rounding here, currently always use default (half even)
    def apply[S:Manifest,I:Manifest,F:Manifest](v: BigDecimal): FixedPoint[S,I,F] = {
      FixedPoint[S,I,F](BigInt((v * (1 << nbits[F])).setScale(0,RoundingMode.HALF_EVEN).toString))
    }
    def apply[S:Manifest,I:Manifest,F:Manifest](v: String): FixedPoint[S,I,F] = FixedPoint[S,I,F](BigDecimal(v))
    def apply[S:Manifest,I:Manifest,F:Manifest](v: BigInt): FixedPoint[S,I,F] = {
      var value = v
      val format = FixFormat(sign[S],nbits[I],nbits[F])
      // Emulate overflow and underflow
      // TODO: Write this using modulus instead
      while (value < format.minValue) value = format.maxValue + (value - format.minValue) + 1
      while (value > format.maxValue) value = format.minValue + (value - format.maxValue) - 1
      new FixedPoint[S,I,F](value)
    }

    def abs[S:Manifest,I:Manifest,F:Manifest](f: FixedPoint[S,I,F]) = FixedPoint[S,I,F](f.v.abs)

    def randbnd[S:Manifest,I:Manifest,F:Manifest](f: FixedPoint[S,I,F]) = {
      FixedPoint[S,I,F](BigInt(java.util.concurrent.ThreadLocalRandom.current().nextLong(f.v.longValue)))
    }
    def rand[S:Manifest,I:Manifest,F:Manifest] = {
      FixedPoint[S,I,F](BigInt(java.util.concurrent.ThreadLocalRandom.current().nextLong()))
    }
  }
}


// Defines class for emulating arbitrary floating point
trait FloatingPointEmulation {
  this: NumEmulation with TypeInspectionCompilerOps =>

  case class FloatFormat(s: Int, e: Int) {
    def bits = s + e
    lazy val maxValue = 0
    lazy val minValue = 1
  }
  private def check(a: FloatFormat, b: FloatFormat) = {
    if (a != b) throw new Exception("Operations on mismatched floating point representations are unsupported")
  }

  class FloatPoint[G:Manifest,E:Manifest](private val v: BigDecimal) {
    private def rep = FloatFormat(nbits[G],nbits[E])

    def unary_-() = { FloatPoint[G,E]( -this.v ) }
    def +(that: FloatPoint[G,E]) = { FloatPoint[G,E](this.v + that.v) }
    def -(that: FloatPoint[G,E]) = { FloatPoint[G,E](this.v - that.v) }
    def *(that: FloatPoint[G,E]) = { FloatPoint[G,E](this.v * that.v) }
    def /(that: FloatPoint[G,E]) = { FloatPoint[G,E](this.v / that.v) }

    def <(that: FloatPoint[G,E]) = { this.v < that.v }
    def >(that: FloatPoint[G,E]) = { this.v > that.v }
    def <=(that: FloatPoint[G,E]) = { this.v <= that.v }
    def >=(that: FloatPoint[G,E]) = { this.v >= that.v }
    override def equals(that: Any) = that match {
      case that: FloatPoint[_,_] =>
        check(this.rep, that.rep)
        this.v == that.v
      case _ => false
    }

    def toFixedPoint[S:Manifest,I:Manifest,F:Manifest]: FixedPoint[S,I,F] = FixedPoint[S,I,F](v)
    def changeFormat[G2:Manifest,E2:Manifest] = FloatPoint[G2,E2](v)

    override def toString() = v.toString()
  }

  object FloatPoint {
    def apply[G:Manifest,E:Manifest](v: Int): FloatPoint[G,E] = FloatPoint[G,E](BigDecimal(v))
    def apply[G:Manifest,E:Manifest](v: Long): FloatPoint[G,E] = FloatPoint[G,E](BigDecimal(v))
    def apply[G:Manifest,E:Manifest](v: Float): FloatPoint[G,E] = FloatPoint[G,E](BigDecimal(v))
    def apply[G:Manifest,E:Manifest](v: Double): FloatPoint[G,E] = FloatPoint[G,E](BigDecimal(v))
    def apply[G:Manifest,E:Manifest](v: String): FloatPoint[G,E] = FloatPoint[G,E](BigDecimal(v))

    // TODO: Support overflow/underflow and precision
    def apply[G:Manifest,E:Manifest](v: BigDecimal): FloatPoint[G,E] = {
      new FloatPoint[G,E](v)
    }

    def abs[G:Manifest,E:Manifest](f: FloatPoint[G,E]) = FloatPoint[G,E](f.v.abs)

    // TODO: Just using double precision right now - no default library implementation of these :(
    def log[G:Manifest,E:Manifest](f: FloatPoint[G,E]) = FloatPoint[G,E](Math.log(f.v.doubleValue))
    def exp[G:Manifest,E:Manifest](f: FloatPoint[G,E]) = FloatPoint[G,E](Math.exp(f.v.doubleValue))
    def sqrt[G:Manifest,E:Manifest](f: FloatPoint[G,E]) = FloatPoint[G,E](Math.sqrt(f.v.doubleValue))

    def rand[G:Manifest,E:Manifest] = FloatPoint[G,E](java.util.concurrent.ThreadLocalRandom.current().nextDouble())
  }
}
