package dhdl.compiler.ops

import java.io.{File,FileWriter,PrintWriter}
import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect, Record}
import scala.virtualization.lms.internal.{Traversal}
import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.{DeliteTransform}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait BlockRAM[T]
trait DHDLVector[T]
trait Register[T]
trait DRAM[T]

trait DHDLCounter
trait DHDLCounterChain
trait DHDLPipeline

trait DHDLBit
trait FixedPoint[SIGN,INT,FRAC]
trait FloatPoint[SIG,EXP]
trait DHDLIndices

trait TypeInspectionOpsExp extends TypeInspectionCompilerOps with TpesOpsExp with DHDLMetadataOpsExp {
  this: DHDLExp =>

  def isFixPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FixedPoint[_,_,_]])
  def isFltPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FloatPoint[_,_]])
  def isBitType[T:Manifest]   = isSubtype(manifest[T].runtimeClass, classOf[DHDLBit])
  def isPipeline[T:Manifest]  = isSubtype(manifest[T].runtimeClass, classOf[DHDLPipeline])
  def isRegister[T:Manifest]  = isSubtype(manifest[T].runtimeClass, classOf[Register[_]])
  def isBRAM[T:Manifest]      = isSubtype(manifest[T].runtimeClass, classOf[BlockRAM[_]])

  // Shorthand versions for matching on ConstFixPt and ConstFltPt without the manifests
  object ParamFix {
    def unapply(x: Exp[Any]): Option[Param[Int]] = x match {
      case Def(EatReflect(Tpes_Int_to_fix(e: Param[Int]))) => Some(e)
      case _ => None
    }
  }

  object ConstFix {
    def unapply(x: Any): Option[Any] = x match {
      case ConstFixPt(x,_,_,_) => Some(x)
      case Def(ConstFixPt(x,_,_,_)) => Some(x)
      case _ => None
    }
  }
  object ConstFlt {
    def unapply(x: Any): Option[Any] = x match {
      case ConstFltPt(x,_,_) => Some(x)
      case Def(ConstFltPt(x,_,_)) => Some(x)
      case _ => None
    }
  }

  override def nbits[T:Manifest]: Int = manifest[T] match {
    case StructType(_,fields) => fields.map(f => nbits(f._2)).fold(0){_+_}
    case _ => super.nbits[T]
  }

  def nbits(e: Exp[Any]): Int = nbits(e.tp)
  def sign(e: Exp[Any]): Boolean = sign(e.tp)

  def isBits[T:Manifest]: Boolean = manifest[T] match {
    case t if isFltPtType(t) || isFixPtType(t) || isBitType(t) => true
    case StructType(_,fields) => fields.map(f => isBits(f._2)).fold(true){_&&_}
    case _ => false
  }

  private def extractNumericConst[T:Manifest](x: T): Option[Double] = {
    val mD = manifest[Double]
    val mF = manifest[Float]
    val mI = manifest[Int]
    val mL = manifest[Long]

    manifest[T] match {
      case `mI` => Some(x.asInstanceOf[Int].toDouble)
      case `mL` => Some(x.asInstanceOf[Long].toDouble)
      case `mF` => Some(x.asInstanceOf[Float].toDouble)
      case `mD` => Some(x.asInstanceOf[Double])
      case _ => None
    }
  }
  override def boundOf(__arg0: Rep[Any])(implicit __pos: SourceContext): Option[MBound] = __arg0 match {
    case p: Param[_] if p.tp == manifest[Int] =>
      val c = extractNumericConst(p.asInstanceOf[Param[Int]].x)
      c.map{c => if (p.isFixed) fixed(c) else exact(c) }

    case Const(x) =>
      val c = extractNumericConst(x)
      c.map{c => fixed(c) }

    case _ => super.boundOf(__arg0)
  }

}

trait MaxJGenTypeInspectionOps extends MaxJGenEffect {
	val IR:DHDLExp
	import IR.{infix_until => _, looprange_until => _, println => _, _}

	lazy val preCodegen = new MaxJPreCodegen {
		val IR: MaxJGenTypeInspectionOps.this.IR.type = MaxJGenTypeInspectionOps.this.IR
	}

  override def initializeGenerator(bd:String): Unit = {
		preCodegen.buildDir = bd
		super.initializeGenerator(bd)
	}

  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
		preCodegen.run(body)
		super.emitSource(args, body, className, out)
	}

}

trait MemoryTemplateTypesExp extends MemoryTemplateTypes {
  type OffChipMem[T] = DRAM[T]
  type BRAM[T] = BlockRAM[T]
  type Vector[T] = DHDLVector[T]
  type Reg[T] = Register[T]

  type Counter = DHDLCounter
  type CounterChain = DHDLCounterChain
  type Pipeline = DHDLPipeline
  type Indices = DHDLIndices

  type Bit = DHDLBit
  type FixPt[SIGN,INT,FRAC] = FixedPoint[SIGN,INT,FRAC]
  type FltPt[SIG,EXP]       = FloatPoint[SIG,EXP]

  def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]] = manifest[DRAM[T]]
  def bramManifest[T:Manifest]: Manifest[BRAM[T]] = manifest[BlockRAM[T]]
  def vectorManifest[T:Manifest]: Manifest[Vector[T]] = manifest[DHDLVector[T]]
  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[Register[T]]
  def counterManifest: Manifest[Counter] = manifest[DHDLCounter]
  def counterChainManifest: Manifest[CounterChain] = manifest[DHDLCounterChain]
  def pipelineManifest: Manifest[Pipeline] = manifest[DHDLPipeline]

  // TODO: Should be refined manifest? But how to know how many fields to fill in?
  def indicesManifest: Manifest[Indices] = manifest[DHDLIndices]

  def fixManifest[S:Manifest,I:Manifest,F:Manifest]: Manifest[FixPt[S,I,F]] = manifest[FixedPoint[S,I,F]]
  def fltManifest[G:Manifest,E:Manifest]: Manifest[FltPt[G,E]] = manifest[FloatPoint[G,E]]
  def bitManifest: Manifest[Bit] = manifest[DHDLBit]
}

trait MemoryTemplateOpsExp extends TypeInspectionOpsExp with MemoryTemplateTypesExp with EffectExp with BRAMOpsExp {
  this: DHDLExp =>

  // --- Nodes
  case class Vector_from_list[T](elems: List[Exp[T]])(implicit val mT: Manifest[T], val ctx: SourceContext) extends Def[Vector[T]]

  // TODO: Can generalize to Mem[T] rather than BRAM?
  case class Bram_load_vector[T](
    bram:   Exp[BRAM[T]],
    ofs:    Exp[FixPt[Signed,B32,B0]],
    cchain: Exp[CounterChain],
    inds:   List[Sym[FixPt[Signed,B32,B0]]]
  )(implicit val mT: Manifest[T], val ctx: SourceContext) extends Def[Vector[T]]

  case class Bram_store_vector[T](
    bram:   Exp[BRAM[T]],
    ofs:    Exp[FixPt[Signed,B32,B0]],
    vec:    Exp[Vector[T]],
    cchain: Exp[CounterChain],
    inds:   List[Sym[FixPt[Signed,B32,B0]]]
  )(implicit val mT: Manifest[T], val ctx: SourceContext) extends Def[Unit]


  // --- Internal API
  def vector_from_list[T:Manifest](elems: List[Rep[T]])(implicit ctx: SourceContext): Rep[Vector[T]] = reflectPure(Vector_from_list(elems))

  def bram_load_vector[T:Manifest](bram: Rep[BRAM[T]], offsets: List[Rep[FixPt[Signed,B32,B0]]], len: Rep[FixPt[Signed,B32,B0]], cchain: Rep[CounterChain])(implicit ctx: SourceContext): Rep[Vector[T]] = {
    val inds = List.fill(lenOf(cchain)){ fresh[FixPt[Signed,B32,B0]] }
    val ofs = calcAddress(offsets, dimsOf(bram))
    val vec = reflectPure(Bram_load_vector(bram, ofs, cchain, inds))
    accessIndicesOf(vec) = offsets
    vec
  }

  def bram_store_vector[T:Manifest](bram: Rep[BRAM[T]], offsets: List[Rep[FixPt[Signed,B32,B0]]], vec: Rep[Vector[T]], cchain: Rep[CounterChain])(implicit ctx: SourceContext): Rep[Unit] = {
    val inds = List.fill(lenOf(cchain)){ fresh[FixPt[Signed,B32,B0]] }
    val ofs = calcAddress(offsets, dimsOf(bram))
    val st = reflectEffect(Bram_store_vector(bram, ofs, vec, cchain, inds), Write(List(bram.asInstanceOf[Sym[BRAM[T]]])))
    accessIndicesOf(st) = offsets
    st
  }


  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@Vector_from_list(elems) => reflectPure(Vector_from_list(f(elems))(e.mT, e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@Vector_from_list(elems), u, es) => reflectMirrored(Reflect(Vector_from_list(f(elems))(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Bram_load_vector(b,o,c,i) => reflectPure(Bram_load_vector(f(b),f(o),f(c),i)(e.mT,e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@Bram_load_vector(b,o,c,i), u, es) => reflectMirrored(Reflect(Bram_load_vector(f(b),f(o),f(c),i)(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Bram_store_vector(b,o,v,c,i) => reflectPure(Bram_store_vector(f(b),f(o),f(v),f(c),i)(e.mT,e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@Bram_store_vector(b,o,v,c,i), u, es) => reflectMirrored(Reflect(Bram_store_vector(f(b),f(o),f(v),f(c),i)(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e, f)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case e: Bram_load_vector[_] => syms(e.bram) ::: syms(e.ofs) ::: syms(e.cchain)
    case e: Bram_store_vector[_] => syms(e.bram) ::: syms(e.ofs) ::: syms(e.vec) ::: syms(e.cchain)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case e: Bram_load_vector[_] => readSyms(e.bram) ::: readSyms(e.ofs) ::: readSyms(e.cchain)
    case e: Bram_store_vector[_] => readSyms(e.bram) ::: readSyms(e.ofs) ::: readSyms(e.vec) ::: readSyms(e.cchain)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e: Bram_load_vector[_] => freqNormal(e.bram) ::: freqNormal(e.ofs) ::: freqNormal(e.cchain)
    case e: Bram_store_vector[_] => freqNormal(e.bram) ::: freqNormal(e.ofs) ::: freqNormal(e.vec) ::: freqNormal(e.cchain)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: Bram_load_vector[_] => e.inds
    case e: Bram_store_vector[_] => e.inds
    case _ => super.boundSyms(e)
  }

  // --- Aliasing
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e: Bram_store_vector[_] => Nil
    case _ => super.aliasSyms(e)
  }
}

// Defines type remappings required in Scala gen (should be same as in library)
trait ScalaGenMemoryTemplateOps extends ScalaGenEffect with ScalaGenControllerTemplateOps {
  val IR: ControllerTemplateOpsExp with DHDLIdentifiers
  import IR._

  override def emitDataStructures(path: String) {
    new File(path + deviceTarget).mkdirs()
    val stream = new PrintWriter(new FileWriter(path + deviceTarget + "NumericEmulation.scala"))
    withStream(stream){ emitFileHeader() }
    stream.println(emul)
    stream.close()

    super.emitDataStructures(path)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "BlockRAM" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DHDLVector" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "Register" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DRAM"     => "Array[" + remap(m.typeArguments(0)) + "]"

    case "DHDLCounter" => "FixedPointRange[Signed,B32,B0]"
    case "DHDLCounterChain" => "Array[FixedPointRange[Signed,B32,B0]]"
    case "DHDLPipeline" => "Unit"

    case "DHDLBit" => "Boolean"
    case "Signed" => "Signed"
    case "Unsign" => "Unsign"
    case "FixedPoint" => "FixedPoint[" + m.typeArguments.map(s=> remap(s)).mkString(",") + "]"
    case "FloatPoint" => "FloatPoint[" + m.typeArguments.map(s=> remap(s)).mkString(",") + "]"
    case bx(n) => "B"+n
    case _ => super.remap(m)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Vector_from_list(elems) =>
      emitValDef(sym, "Array" + elems.map(quote).mkString("(", ",", ")"))

    case _ => super.emitNode(sym, rhs)
  }

// HACK: Have this entire template as a string right now...
  val emul = s"""
import scala.math.BigDecimal.RoundingMode
import scala.reflect.Manifest

trait Signed
trait Unsign

trait B0;  trait B1;  trait B2;  trait B3;  trait B4;  trait B5;  trait B6;  trait B7
trait B8;  trait B9;  trait B10; trait B11; trait B12; trait B13; trait B14; trait B15
trait B16; trait B17; trait B18; trait B19; trait B20; trait B21; trait B22; trait B23
trait B24; trait B25; trait B26; trait B27; trait B28; trait B29; trait B30; trait B31
trait B32; trait B33; trait B34; trait B35; trait B36; trait B37; trait B38; trait B39
trait B40; trait B41; trait B42; trait B43; trait B44; trait B45; trait B46; trait B47
trait B48; trait B49; trait B50; trait B51; trait B52; trait B53; trait B54; trait B55
trait B56; trait B57; trait B58; trait B59; trait B60; trait B61; trait B62; trait B63
trait B64

object NumEmul {
  def sign[T:Manifest] = manifest[T] match {
    case mT if mT == manifest[Signed] => true
    case mT if mT == manifest[Unsign] => false
    case mT => throw new Exception("Unknown type in sign: " + mT.runtimeClass.getSimpleName)
  }

  private lazy val bx = "B([0-9]+)".r
  object BXX {
    def unapply[T](x: Manifest[T]): Option[Int] = x.runtimeClass.getSimpleName match {
      case bx(bits) => Some(bits.toInt)
      case _ => None
    }
  }

  def nbits[T:Manifest] = manifest[T] match {
    case BXX(bits) => bits
    case mT => throw new Exception("Unknown type in nbits: " + mT.runtimeClass.getSimpleName)
  }

  def check(a: FixFormat, b: FixFormat) {
    if (a != b) throw new Exception("Operations on mismatched fixed point representations (" + a + " versus " + b + ") are unsupported")
  }
  def check(a: FloatFormat, b: FloatFormat) {
    if (a != b) throw new Exception("Operations on mismatched floating point representations are unsupported")
  }
}

case class FixFormat(signed: Boolean, m: Int, f: Int) {
  def bits = m + f
  lazy val maxValue = if (signed) (BigInt(1) << (bits-1)) - 1 else (BigInt(1) << bits) - 1
  lazy val minValue = if (signed) -(BigInt(1) << (bits-1))    else BigInt(0)
}

case class FloatFormat(s: Int, e: Int) {
  def bits = s + e
  lazy val maxValue = 0 //TODO
  lazy val minValue = 1 //TODO
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
}

// Defines class for emulating arbitrary fixed point
// Note that all computation is boxed here and done with BigInt for generality. Probably not the best performance
class FixedPoint[S:Manifest,I:Manifest,F:Manifest](private val v: BigInt) {
  import NumEmul._

  private lazy val rep = FixFormat(sign[S],nbits[I],nbits[F])

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
  import NumEmul._

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


// Defines class for emulating arbitrary floating point
class FloatPoint[G:Manifest,E:Manifest](private val v: BigDecimal) {
  import NumEmul._

  private lazy val rep = FloatFormat(nbits[G],nbits[E])

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
"""
}

trait DotGenMemoryTemplateOps extends DotGenEffect with DotGenControllerTemplateOps{
	  val IR: ControllerTemplateOpsExp with OffChipMemOpsExp with DHDLCodegenOps with RegOpsExp with
		DHDLIdentifiers with DeliteTransform
		  import IR._

	var emittedSize = Set.empty[Exp[Any]]
  override def initializeGenerator(buildDir:String): Unit = {
		emittedSize = Set.empty[Exp[Any]]
		super.initializeGenerator(buildDir)
	}

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    /*case TileTransfer(mem,local,strides,memOfs,tileStrides,cchain,iters, store) => // Load
			val l = s"Tile${if (store) "Store" else "Load"}_" + quote(sym).split("_")(1)
      emit(s"""subgraph cluster_${quote(sym)} {""")
			emit(s"""label="$l"""")
			emit(s"""shape="rectangle"""")
			emit(s"""style="rounded, filled"""")
			emit(s"""fillcolor=$tileTransFillColor""")
			emit(s"""color="black"""")
			var nl = if (store) "TileSt" else "TileLd"
			nl += "|stride=\\{"
			strides.zipWithIndex.foreach{ case (s, i) =>
				if (quote(s).forall(_.isDigit)) {
					nl += s"$i:${quote(s)}"
					if (i!=strides.length-1) nl += ", "
				} else {
					emitEdge(s, sym, s"stride $i")
				}
			}
			nl += "\\}"
			/*if (quote(memOfs).forall(_.isDigit))
				nl += s"|memOfs=${quote(memOfs)}"
			else
				emitEdge(memOfs, sym, "memOfs")*/
			emit(s"""${quote(sym)} [label="$nl" shape="record" style="rounded, filled" color="black" fillcolor="gray"]""")
			emitCtrChain(cchain)
			emit(s"""} """)
			if (store) {
				emitEdge(sym, mem)
				emitEdge(local, sym)
			} else {
				emitEdge(mem, sym)
				emitEdge(sym, local)
			}*/

		case Offchip_new(size) =>
			/* Special case to hand nodes producing size of offchip outside hardware scope. Not actual
       * codegen to Offchip_new */
			def hackGen(x: Exp[Any]): Unit = x match {
				case Def(EatReflect(_:Reg_new[_])) => // Nothing
				case ConstFix(_) => // Nothing
				case ConstFlt(_) => // Nothing
				case Def(d) =>
					alwaysGen {
						emitNode(x.asInstanceOf[Sym[Any]], d)
					}
					syms(d).foreach{ s => s match {
							case _ => hackGen(s)
						}
					}
				case _ => // Nothing
			}
			if (!emittedSize.contains(size)) {
				hackGen(size)
				emittedSize = emittedSize + size
			}
			super.emitNode(sym, rhs)

    case _ => super.emitNode(sym, rhs)
  }
}

trait MaxJGenMemoryTemplateOps extends MaxJGenEffect with MaxJGenControllerTemplateOps{
  val IR: ControllerTemplateOpsExp with TpesOpsExp with ParallelOpsExp
          with OffChipMemOpsExp with RegOpsExp with CounterExternOpsExp
          with DHDLCodegenOps with DeliteTransform
  import IR._

  // Current TileLd/St templates expect that LMem addresses are
  // statically known during graph build time in MaxJ. That can be
  // changed, but until then we have to assign LMem addresses
  // statically. Assigning each OffChipArray a 384MB chunk now
  val burstSize = 384
  var nextLMemAddr = burstSize * 1024 * 1024
  def getNextLMemAddr() = {
    val addr = nextLMemAddr
    nextLMemAddr += burstSize * 1024 * 1024;
    addr/burstSize
  }

	var emittedSize = Set.empty[Exp[Any]]
  override def initializeGenerator(buildDir:String): Unit = {
		emittedSize = Set.empty[Exp[Any]]
    nextLMemAddr = burstSize * 1024 * 1024
		super.initializeGenerator(buildDir)
	}

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
		case Offchip_new(size) =>
      alwaysGen {
        emit(s"""int ${quote(sym)} = ${getNextLMemAddr()};""")
      }

		case Reg_new(init) =>
      emitComment("Reg_new {")
			val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
			regType(sym) match {
				case Regular =>
          val parent = if (parentOf(sym).isEmpty) "Top" else quote(parentOf(sym).get) //TODO
					if (isDblBuf(sym)) {
						emit(s"""DblRegFileLib ${quote(sym)}_lib = new DblRegFileLib(this, $ts, ${quote(sym)}, ${parOf(sym)});""")
            val readstr = if (parOf(sym)>1) "readv" else "read"
            emit(s"""${maxJPre(sym)} ${quote(sym)} = ${quote(sym)}_lib.${readstr}()""")
           	emit(quote(sym) + "_lib.connectWdone(" + quote(writerOf(sym).get._1) + "_done);")
            readersOf(sym).foreach { case (r, _, _) =>
           	  emit(quote(sym) +"_lib.connectRdone(" + quote(r) + "_done);")
           	}
          } else {
            emit(s"""${quote(maxJPre(sym))} ${quote(sym)} = ${quote(ts)}.newInstance(this);""")
					}
				case ArgumentIn =>  // alwaysGen
        	alwaysGen {
          	emit(s"""DFEVar ${quote(sym)} = io.scalarInput(${quote(sym)} , $ts );""")
				  }
				case ArgumentOut => //emitted in reg_write
			}
      emitComment("} Reg_new")

		case e@Reg_write(reg, value) =>
      emitComment("Reg_write {")
			val ts = tpstr(parOf(reg))(reg.tp.typeArguments.head, implicitly[SourceContext])
			if (isDblBuf(reg)) {
			 	emit(s"""${quote(reg)}_lib.write(${value}, ${quote(writerOf(reg).get._1)}_done);""")
      } else {
				regType(reg) match {
					case Regular =>
      		  val parent = if (parentOf(reg).isEmpty) "top" else quote(parentOf(reg).get) //TODO
      		  val rst = quote(parent) + "_rst_en"
					  if (writerOf(reg).isEmpty)
					  	throw new Exception(s"Reg ${quote(reg)} is not written by a controller, which is not supported at the moment")
					  val enSignalStr = writerOf(reg).get._1 match {
					  	case p@Def(EatReflect(e:Pipe_foreach)) => styleOf(p) match {
					  		case Fine => quote(e.cchain) + "_en_from_pipesm"
					  		case _ => quote(p) + "_en"
					  	}
					  	case p@Def(EatReflect(e:Pipe_fold[_,_])) => styleOf(p) match {
					  		case Fine => quote(e.cchain) + "_en_from_pipesm"
					  		case _ => quote(p) + "_en"
					  	}
					  	case p@_ => val Def(d) = p
                          throw new Exception(s"Reg ${quote(reg)} is written by non Pipe node ${p} def:${d}")
					  }
      		  emit(s"""DFEVar ${quote(value)}_real = $enSignalStr ? ${quote(value)}:${quote(reg)}; // enable""")
      		  emit(s"""DFEVar ${quote(reg)}_hold = Reductions.streamHold(${quote(value)}_real, ($rst | ${quote(writerOf(reg).get._1)}_redLoop_done));""")
      		  emit(s"""${quote(reg)} <== $rst ? constant.var($ts, ${quote(resetValue(reg))}):stream.offset(${quote(reg)}_hold, -${quote(writerOf(reg).get._1)}_offset); // reset""")
				  case ArgumentIn => new Exception("Cannot write to Argument Out! " + quote(reg))
				  case ArgumentOut =>
				 	  val controlStr = if (parentOf(reg).isEmpty) s"top_done" else quote(parentOf(reg).get) + "_done" //TODO
      	  	emit(s"""io.scalarOutput(${quote(reg)}, ${quote(value)}, $ts, $controlStr);""")
				}
			}
      emitComment("} Reg_write")

    case Bram_new (size, zero) =>
      emitComment("Bram_new {")
			val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
      //TODO: does templete assume bram has 2 dimension?
      val dims = dimsOf(sym)
      val size0 = dims(0)
      val size1 = if (dims.size==1) 1 else dims(1)
      if (isDblBuf(sym)) {
        //readers.foreach { r =>
        //  if (!readerToMemMap.contains(r)) readerToMemMap += r -> Set[MemNode]()
        //  readerToMemMap(r) += n
        //}
        //rdoneSet += n -> Set()

        //emit(s"""// ${quote(sym)} has ${readersOf(sym).size} readers - ${readersOf(sym)}""")
        emit(s"""SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new ${quote(sym)}_DblBufSM(this));""")
        val dims = dimsOf(sym)
        emit(s"""DblBufKernelLib ${quote(sym)} = new DblBufKernelLib(this, ${quote(sym)}_sm,
          $size0, $size1, $ts, ${banks(sym)}, stride_TODO, ${readersOf(sym).size});""")
        if (writerOf(sym).isEmpty)
          throw new Exception(s"Bram ${quote(sym)} has no writer!")
          //  If writer is a unit Pipe, wait until parent is finished
        //val doneSig = n.getWriter().getOrElse(throw new Exception(s"BRAM $n has no writer")) match {
        //  case p: Pipe if p.isUnitPipe =>
        //    var writer = p.getParent()
        //    while (!(writer.isInstanceOf[Sequential] || writer.isInstanceOf[MetaPipeline])) {
        //      writer = writer.getParent()
        //    }
        //    s"${quote(writer)}_done"
        //  case _ =>
        //    s"${quote(n.getWriter())}_done"
        //}

      val doneSig = ""
        emit(s"""${quote(sym)}.connectWdone($doneSig);""")
      } else {
        emit(s"""BramLib ${quote(sym)} = new BramLib(this, ${quote(size0)}, ${size1}, ${ts}, ${banks(sym)}, stride_TODO);""")
      }
      emitComment("} Bram_new")

    case Bram_load(bram, addr) =>
      emitComment("Bram_load {")
			val pre = maxJPre(bram)
      emit(s"""${pre} ${quote(sym)} = ${quote(bram)}.connectRport(${quote(addr)});""")
      if (isDblBuf(bram)) {
        if (parentOf(bram).isEmpty)
          throw new Exception("Bram (DblBuf)" + quote(bram) + " does not have a parent!")
        val p = parentOf(bram).get
        if (readersOf(bram).map(_._1).contains(p)) {
          //if (!rdoneSet(mem).contains(r)) { TODO
          emit(s"""${quote(bram)}.connectRdone(${p}_done);""")
          //rdoneSet(mem) += r
          //}
        }
      }
      // Handle if loading a composite type
      //n.compositeValues.zipWithIndex.map { t =>
      //  val v = t._1
      //  val idx = t._2
      //  visitNode(v)
      //  emit(s"""${quote(v)} <== ${quote(sym)}[$idx];""")
      //}
      emitComment("} Bram_load")

    case Bram_store(bram, addr, value) =>
      emitComment("Bram_store {")
			val dataStr = quote(value)
      if (isAccum(bram)) {
        val offsetStr = quote(writerOf(bram).get._1) + "_offset"
        val parentPipe = parentOf(bram).getOrElse(throw new Exception(s"Bram ${quote(bram)} does not have a parent!"))
        val parentCtr = parentPipe match {
          case Def(EatReflect(d)) => d match {
            case d:Pipe_fold[_,_] => d.cchain
            case d:Pipe_foreach => d.cchain
          }
          case p => throw new Exception(s"Unknown parent type ${p}!")
        }
        emit(s"""$bram.connectWport(stream.offset($addr, -$offsetStr),
          stream.offset($dataStr, -$offsetStr), ${quote(parentCtr)}_en_from_pipesm, start_TODO, stride_TODO);""")
      } else {
         emit(s"""${quote(bram)}.connectWport(${quote(addr)}, ${dataStr}, ${quote(parentOf(bram).get)}_en, start_TODO, stride_TODO ;""") //TODO
      }
      emitComment("} Bram_store")

    case _ => super.emitNode(sym, rhs)
  }
}
