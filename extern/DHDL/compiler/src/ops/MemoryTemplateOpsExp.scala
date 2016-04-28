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
trait Register[T]
trait DRAM[T]

trait DHDLCounter
trait DHDLCounterChain
trait DHDLPipeline

trait DHDLBit
trait FixedPoint[SIGN,INT,FRAC]
trait FloatPoint[SIG,EXP]
trait DHDLIndices

// Stub (nothing here for now)
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
  case class TileTransfer[T:Manifest](
    mem:      Rep[OffChipMem[T]],                // Offchip memory array
    local:    Rep[BRAM[T]],                      // Local memory (BRAM)
    strides:  List[Rep[FixPt[Signed,B32,B0]]],   // Dimensions converted to strides for offchip memory
    memOfs:   Rep[FixPt[Signed,B32,B0]],         // Offset into offchip memory
    tileStrides: List[Rep[FixPt[Signed,B32,B0]]],   // Tile strides
    cchain:   Rep[CounterChain],                 // Counter chain for copy
    iters:    List[Sym[FixPt[Signed,B32,B0]]],   // Bound iterator variables
    store:    Boolean                            // Is this transfer a store (true) or a load (false)
  )(implicit ctx: SourceContext) extends Def[Unit] {
    val mT = manifest[T]
  }

  // --- Internal API
  def tile_transfer[T:Manifest](mem: Rep[OffChipMem[T]], local: Rep[BRAM[T]], strides: List[Rep[FixPt[Signed,B32,B0]]], memOfs: Rep[FixPt[Signed,B32,B0]], tileStrides: List[Rep[FixPt[Signed,B32,B0]]], cchain: Rep[CounterChain], store: Boolean)(implicit ctx: SourceContext): Rep[Unit] = {
    val iters = List.fill(lenOf(cchain)){ fresh[FixPt[Signed,B32,B0]] }

    if (store) reflectWrite(mem)(TileTransfer(mem,local,strides,memOfs,tileStrides,cchain,iters,store))
    else       reflectWrite(local)(TileTransfer(mem,local,strides,memOfs,tileStrides,cchain,iters,store))
  }


  // HACK: Only want to allow DSE parameters or constants (ConstFix) here
  override def bram_create[T:Manifest](__arg0: Option[String],__arg1: List[Rep[FixPt[Signed,B32,B0]]])(implicit __pos: SourceContext,__imp0: Num[T]) = {
    __arg1.foreach{
      case ConstFix(_) =>
      case ParamFix(_) =>
      case _ => stageError("Only constants and DSE parameters are allowed as dimensions of BRAM")(__pos)
    }
    super.bram_create[T](__arg0,__arg1)(implicitly[Manifest[T]],__pos,__imp0)
  }


  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@TileTransfer(m,l,s,o,t,c,i,st) => reflectPure(TileTransfer(f(m),f(l),f(s),f(o),t,f(c),i,st)(e.mT,pos))(mtype(manifest[A]), pos)
    case Reflect(e@TileTransfer(m,l,s,o,t,c,i,st), u, es) => reflectMirrored(Reflect(TileTransfer(f(m),f(l),f(s),f(o),t,f(c),i,st)(e.mT,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }

  // --- Dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case e: TileTransfer[_] => syms(e.mem) ::: syms(e.local) ::: syms(e.strides) ::: syms(e.tileStrides) ::: syms(e.memOfs) ::: syms(e.cchain)
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case Pipe_foreach(chain, func, _) => readSyms(chain) ::: readSyms(func)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e: TileTransfer[_] => freqNormal(e.mem) ::: freqNormal(e.local) ::: freqNormal(e.strides) ::: freqNormal(e.tileStrides) ::: freqNormal(e.memOfs) ::: freqNormal(e.cchain)

    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: TileTransfer[_] => e.iters
    case _ => super.boundSyms(e)
  }

  // --- Aliasing
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e: TileTransfer[_] => Nil
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
    case TileTransfer(mem,local,strides,memOfs,tileStrides,cchain,iters, store) =>
      emitNestedLoop(iters, cchain) {
        val offaddr = (iters.zip(strides).map{case (i,s) => quote(i) + "*" + quote(s) } :+ quote(memOfs)).mkString(" + ")
        stream.println("val offaddr = " + offaddr)

        val localAddr = iters.zip(tileStrides).map{ case (i,s) => quote(i) + "*" + quote(s) }.mkString(" + ")
        stream.println("val localaddr = " + localAddr)

        if (store)
          stream.println(quote(mem) + "(offaddr.toInt) = " + quote(local) + "(localaddr.toInt)")
        else
          stream.println(quote(local) + "(localaddr.toInt) = " + quote(mem) + "(offaddr.toInt)")
      }

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
    case TileTransfer(mem,local,strides,memOfs,tileDims,cchain,iters, store) => // Load
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
			if (quote(memOfs).forall(_.isDigit))
				nl += s"|memOfs=${quote(memOfs)}"
			else
				emitEdge(memOfs, sym, "memOfs")
			emit(s"""${quote(sym)} [label="$nl" shape="record" style="rounded, filled" color="black" fillcolor="gray"]""")
			emitCtrChain(cchain)
			emit(s"""} """)
			if (store) {
				emitEdge(sym, mem)
				emitEdge(local, sym)
			} else {
				emitEdge(mem, sym)
				emitEdge(sym, local)
			}

		case Offchip_new(size) =>
			/* Special case to hand nodes producing size of offchip outside hardware scope */
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

trait MaxJGenMemoryTemplateOps extends MaxJGenEffect {
	  val IR: ControllerTemplateOpsExp with OffChipMemOpsExp with DHDLCodegenOps with RegOpsExp with DHDLIdentifiers
  import IR._

	var emittedSize = Set.empty[Exp[Any]]
  override def initializeGenerator(buildDir:String): Unit = {
		emittedSize = Set.empty[Exp[Any]]
		super.initializeGenerator(buildDir)
	}

  // Note that tileDims are not fixed point values yet - they're just integers
  private def localDimsToStrides(dims: List[Int]) = List.tabulate(dims.length){d =>
    if (d == dims.length - 1) 1
    else dims.drop(d + 1).reduce(_*_)
  }

  // TODO: match on store = true or store = false if want as different gen rules
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case TileTransfer(mem,local,strides,memOfs,tileDims,cchain,iters, store) =>

		case Offchip_new(size) =>
			/* Special case to hand nodes producing size of offchip outside hardware scope */
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

		case Reg_new(init) =>
			val ts = tpstr(par(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
			regType(sym) match {
				case Regular =>
					//TODO
          val parent = if (parentOf(sym).isEmpty) "Top" else quote(parentOf(sym).get)
					if (isDblBuf(sym)) {
						emit(s"""DblRegFileLib ${quote(sym)}_lib = new DblRegFileLib(this, $ts, ${quote(sym)}, ${par(sym)});""")
            val readstr = if (par(sym)>1) "readv" else "read" 
            emit(s"""${maxJPre(sym)} ${quote(sym)} = ${quote(sym)}_lib.${readstr}()""")
           	emit(quote(sym) + "_lib.connectWdone(" + quote(writerOf(sym).get._1) + "_done);")
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

		case e@Reg_write(reg, value) => 
			val ts = tpstr(par(reg))(reg.tp.typeArguments.head, implicitly[SourceContext])
			if (isDblBuf(reg)) {
			 	emit(quote(reg) + "_lib.write(" + value + ", " + quote(writerOf(reg).get._1) + "_done);")
      } else {
				regType(reg) match {
					case Regular => //TODO
      		  val parent = if (parentOf(reg).isEmpty) "top" else quote(parentOf(reg).get)
      		  val rst = quote(parent) + "_rst_en"
					  if (writerOf(reg).isEmpty)
					  	throw new Exception("Reg " + quote(reg) + " is not written by a controller, which is not supported at the moment")
					  val enSignalStr = writerOf(reg).get._1 match {
					  	case p@Def(Pipe_foreach(cchain,_,_)) => styleOf(reg) match {
					  		case Fine =>
					  			emit(quote(cchain) + "_en_from_pipesm")
					  		case _ =>
					  			emit(quote(writerOf(reg).get._1) + "_en")
					  	}
					  	case p@Def(Pipe_reduce(cchain, _, _, _, _, _, _, _, _, _, _, _)) => styleOf(reg) match {
					  		case Fine =>
					  			emit(quote(cchain) + "_en_from_pipesm")
					  		case _ =>
					  			emit(quote(writerOf(reg).get._1) + "_en")
					  	}
					  	case _ =>
					  }
      		  emit(s"""DFEVar ${quote(value)}_real = $enSignalStr ? ${quote(value)}:${quote(reg)}; // enable""")
      		  emit(s"""DFEVar ${quote(reg)}_hold = Reductions.streamHold(${quote(value)}_real, ($rst | ${quote(writerOf(reg).get._1)}_redLoop_done));""")
      		  emit(s"""${quote(reg)} <== $rst ? constant.var(${tpstr(par(reg))(reg.tp, implicitly[SourceContext])}, ${quote(resetValue(reg))}):stream.offset(${quote(reg)}_hold, -${quote(writerOf(reg).get._1)}_offset); // reset""")
				  case ArgumentIn => new Exception("Cannot write to Argument Out! " + quote(reg))
				  case ArgumentOut =>
				 	  val controlStr = if (parentOf(reg).isEmpty) s"top_done" else quote(parentOf(reg).get) + "_done"
      	  	  emit(s"""io.scalarOutput(${quote(reg)}, ${quote(value)}, $ts, $controlStr);""")
				}
			}

    case _ => super.emitNode(sym, rhs)
  }
}
