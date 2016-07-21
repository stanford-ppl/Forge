package dhdl.compiler.ops

import java.io.{File,FileWriter,PrintWriter}
import scala.virtualization.lms.internal.{Traversal}
import scala.virtualization.lms.common.{BaseExp, EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import ppl.delite.framework.transform.{DeliteTransform}
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait DHDLBit
trait FixedPoint[SIGN,INT,FRAC]
trait FloatPoint[SIG,EXP]

trait ExternPrimitiveTypesExp extends ExternPrimitiveTypes with BaseExp {
  type Bit                  = DHDLBit
  type FixPt[SIGN,INT,FRAC] = FixedPoint[SIGN,INT,FRAC]
  type FltPt[SIG,EXP]       = FloatPoint[SIG,EXP]

  def isFixPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FixedPoint[_,_,_]])
  def isFltPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FloatPoint[_,_]])
  def isBitType[T:Manifest]   = isSubtype(manifest[T].runtimeClass, classOf[DHDLBit])

  def fixManifest[S:Manifest,I:Manifest,F:Manifest]: Manifest[FixPt[S,I,F]] = manifest[FixedPoint[S,I,F]]
  def fltManifest[G:Manifest,E:Manifest]: Manifest[FltPt[G,E]] = manifest[FloatPoint[G,E]]
  def bitManifest: Manifest[Bit] = manifest[DHDLBit]
}

trait ExternPrimitiveOpsExp extends ExternPrimitiveCompilerOps with ExternPrimitiveTypesExp with TpesOpsExp
  with DHDLMetadataOpsExp with FixPtOpsExp {

  this: DHDLExp =>

  case class Min2[T](a: Rep[T],b:Rep[T])(implicit val mT: Manifest[T], val oT: Order[T], val nT: Num[T], val ctx: SourceContext) extends Def[T]
  case class Max2[T](a: Rep[T],b:Rep[T])(implicit val mT: Manifest[T], val oT: Order[T], val nT: Num[T], val ctx: SourceContext) extends Def[T]

  def min2[T:Manifest:Order:Num](a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) = reflectPure(Min2(a,b))
  def max2[T:Manifest:Order:Num](a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) = reflectPure(Max2(a,b))

  // --- Internal API
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

  def isStaticSize[T:Manifest](x: Rep[T]): Boolean = x match {
    case ConstFix(_) => true
    case ParamFix(_) => true
    case _ => false
  }

  // --- Rewrite Rules
  // TODO: Allow rewrite rules in forge on metadata helpers
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

  override def globalCheck(__arg0: Rep[Any])(implicit __pos: SourceContext): Boolean = __arg0 match {
    case p: Param[_] => true
    case Const(x) => true
    case _ => super.globalCheck(__arg0)
  }

  // TODO: Move to spec later?
  // Rewrite needed for length calculation of vectors, ranges
  override def sub[S:Manifest,I:Manifest,F:Manifest](__arg0: Rep[FixPt[S,I,F]],__arg1: Rep[FixPt[S,I,F]])(implicit __pos: SourceContext,__imp1: Overload2) = {
    (__arg0) match {
      case Def(FixPt_Add(`__arg1`, y: Rep[FixPt[S,I,F]])) => y   // (x + y) - x == y
      case Def(FixPt_Add(y: Rep[FixPt[S,I,F]], `__arg1`)) => y   // (y + x) - x == y
      case _ => super.sub(__arg0, __arg1)(manifest[S],manifest[I],manifest[F],__pos,__imp1)
    }
  }

  // Dual (just for completeness)
  override def add[S:Manifest,I:Manifest,F:Manifest](__arg0: Rep[FixPt[S,I,F]],__arg1: Rep[FixPt[S,I,F]])(implicit __pos: SourceContext,__imp1: Overload2) = {
    (__arg0,__arg1) match {
      case (Def(FixPt_Sub(x: Rep[FixPt[S,I,F]], `__arg1`)), _) => x  // (x - y) + y == x
      case (_, Def(FixPt_Sub(x: Rep[FixPt[S,I,F]], `__arg0`))) => x  // y + (x - y) == x
      case _ => super.add(__arg0, __arg1)(manifest[S],manifest[I],manifest[F],__pos,__imp1)
    }
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case EatReflect(e@Min2(a,b)) => reflectPure(Min2(f(a),f(b))(e.mT,e.oT,e.nT,e.ctx))(mtype(manifest[A]),pos)
    case EatReflect(e@Max2(a,b)) => reflectPure(Max2(f(a),f(b))(e.mT,e.oT,e.nT,e.ctx))(mtype(manifest[A]),pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}


trait MaxJGenExternPrimitiveOps extends MaxJGenEffect {
  val IR:DHDLExp with MemoryAnalysisExp with DeliteTransform


  import IR.{infix_until => _, looprange_until => _, println => _, _}

	var traversals: List[Traversal{val IR: MaxJGenExternPrimitiveOps.this.IR.type}] = Nil

  lazy val preCodegen = new MaxJPreCodegen {
    val IR: MaxJGenExternPrimitiveOps.this.IR.type = MaxJGenExternPrimitiveOps.this.IR
  }

  override def initializeGenerator(bd:String): Unit = {
    preCodegen.buildDir = bd
		traversals = IR.traversals
    super.initializeGenerator(bd)
  }


  def runTraversals[A:Manifest](b: Block[A]): Block[A] = {
    println("MaxJCodegen: applying transformations")
    var curBlock = b
    println("Traversals:\n\t" + traversals.map(_.name).mkString("\n\t"))

    for (t <- traversals) {
      printlog("  Block before transformation: " + curBlock)
      curBlock = t.run(curBlock)
      printlog("  Block after transformation: " + curBlock)
    }
    println("MaxJGodegen: done transforming")
    (curBlock)
  }

  override def preProcess[A: Manifest](body: Block[A]) = {
    preCodegen.run(body)
    super.preProcess(body)
  }
}


// Defines type remappings required in Scala gen (should be same as in library)
trait ScalaGenExternPrimitiveOps extends ScalaGenEffect {
  val IR: ExternPrimitiveOpsExp with DHDLIdentifiers
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
    case "DHDLBit" => "Boolean"
    case "Signed" => "Signed"
    case "Unsign" => "Unsign"
    case "FixedPoint" => "FixedPoint[" + m.typeArguments.map(s=> remap(s)).mkString(",") + "]"
    case "FloatPoint" => "FloatPoint[" + m.typeArguments.map(s=> remap(s)).mkString(",") + "]"
    case bx(n) => "B"+n
    case _ => super.remap(m)
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

case class FixedPointRange[S:Manifest,I:Manifest,F:Manifest](start: FixedPoint[S,I,F], end: FixedPoint[S,I,F], step: FixedPoint[S,I,F], par: Int) {
  private val parStep = FixedPoint[S,I,F](par)
  private val fullStep = parStep * step
  private val vecOffsets = Array.tabulate(par){p => FixedPoint[S,I,F](p) * step}

  def foreach(func: Array[FixedPoint[S,I,F]] => Unit) = {
    var i = start
    while (i < end) {
      val vec = vecOffsets.map{ofs => ofs + i}
      func(vec)
      i += fullStep
    }
  }
  def by(s: FixedPoint[S,I,F]) = FixedPointRange[S,I,F](start, end, s, 1)
  def par(p: Int) = FixedPointRange[S,I,F](start, end, step, p)
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
    FixedPointRange(this, that, FixedPoint[S,I,F](1), 1)
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



