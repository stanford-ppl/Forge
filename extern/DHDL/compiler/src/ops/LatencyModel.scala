package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait LatencyModel {
  this: DHDLExp with CounterToolsExp =>

  lazy val memModel = new TileLoadModel()

  object Deff {
    def unapply(e: Exp[Any]): Option[Any] = e match {
      case Def(Reflect(inner, _, _)) => Some(inner)
      case Def(d) => Some(d)
      case _ => None
    }
  }

  private var silentModel = false
  private def warn(x: => String) { if (!silentModel) stageWarn(x) }
  def silenceLatencyModel() { silentModel = true }

  def latencyOf(s: Exp[Any], inReduce: Boolean) = s match {
    case Def(d) if inReduce  => latencyOfNodeInReduce(s, d)
    case Def(d) if !inReduce => latencyOfNode(s, d)
    case _ => 0
  }

  private def latencyOfNodeInReduce(s: Exp[Any], d: Def[Any]): Long = d match {
    case Add_flt(_,_) => 1
    case RegWrite(_,_) => 0
    case Reflect(d,_,_) => latencyOfNodeInReduce(s,d)
    case _ => latencyOfNode(s, d)
  }


  // TODO: These need new numbers after Raghu's changes
  // c - contention (1 to 13) (number of readers/writers in parallel)
  // r - number of commands (>= 1)
  // b - number of words per command (step of 96)
  // p - number of words read into buffer in parallel
  def memoryModel(c: Int, r: Int, b: Int, p: Int) = {
    val cols = if (b < 96) 96 else b
    val overhead12 = cols match {
      case 96  => 0.307/(1 + Math.exp(-0.096*r + 0.21))   // Logistic, bounded by 30.7%
      case 192 => 0.185/(1 + Math.exp(-0.24*r - 0.8))     // Logistic, bounded by 18.5%
      case _ => 0.165
    }
    val overhead = ((1/Math.log(12))*Math.log(c))*overhead12
    val base = Math.ceil( (1+overhead)*(110 + r*(53 + cols)) )

    val parSpeedup = memModel.evaluate(c, r, cols, p)

    //System.out.println(s"Base: $base, par: $parSpeedup")

    (parSpeedup*base).toLong
  }


  // Super hacky way of checking for dotproduct-like fusion
  // Last operation in map was a multiply, only operation in reduce is add
  def canFuse(map: Block[Any], reduce: Block[Any], rV: (Sym[Any],Sym[Any]), p: Int) = {
    val rV1 = rV._1
    val rV2 = rV._2
    val m = getBlockResult(map)
    val r = getBlockResult(reduce)
    //System.out.println(s"Map: $m, Reduce: $r, rV: ($rV1, $rV2)")
    val canFuse = (m,r) match {
      case (Deff(Mul_flt(_,_)), Deff(Add_flt(`rV1`,`rV2`))) => true
      case _ => false
    }
    canFuse && isPow2(p)
  }

  private def latencyOfNode(s: Exp[Any], d: Def[Any]): Long = s match {
    case Fixed(_) => 0
    case Exact(_) => 0
    case _ => d match {
    case ConstBit(_) => 0
    case ConstFix(_) => 0
    case ConstFlt(_) => 0
    case Reg_new(_) => 0
    case Bram_new(_,_) => 0

    // TODO: Not a function of number of banks?
    case Bram_load(ram, _) => if (isDblBuf(ram)) 2 else 1
    case Bram_store(ram, _, _) => if (isDblBuf(ram)) 2 else 1

    case _:Counter_new => 0
    case _:Counterchain_new => 0

    // TODO: Have to get numbers for non-32 bit multiplies and divides
    case Neg_fix(_)   => 1

    // HACK: Fused multiply-add in index calculation
    case Add_fix(Deff(Mul_fix(_,_)),Exact(_)) => 0
    case Add_fix(Deff(Mul_fix(_,_)),Def(_)) => 1
    case Add_fix(Deff(Mul_fix(_,_)),_) => 0
    case Add_fix(Exact(_),Deff(Mul_fix(_,_))) => 0
    case Add_fix(Def(_),Deff(Mul_fix(_,_))) => 1
    case Add_fix(_,Deff(Mul_fix(_,_))) => 0

    case Add_fix(_,_) => 1

    case Sub_fix(_,_) => 1
    case Mul_fix(Exact(_),_) => 1
    case Mul_fix(_,Exact(_)) => 1
    case Mul_fix(_,_) =>
      if (nbits(s) > 32) warn(s"Don't know latency for $d - using default")
      if (nbits(s) <= 18) 1 else 2

    case Div_fix(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      if (sign(s)) 35 else 38

    case Mod_fix(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      if (sign(s)) 35 else 38

    case Lt_fix(_,_)  => 1
    case Leq_fix(_,_) => 1
    case Neq_fix(_,_) => 1
    case Eql_fix(_,_) => 1
    case And_fix(_,_) => 1
    case Or_fix(_,_)  => 1

    //case Lsh_fix(_,_) => // ???
    //case Rsh_fix(_,_) => // ???

    // TODO: Floating point for things besides single precision
    case Neg_flt(_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (1)

    case Add_flt(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (14)

    case Sub_flt(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (14)

    // MaxJ does floating point multiply-add fusion
    case Mul_flt(Deff(Add_flt(_,_)),Deff(Add_flt(_,_))) => 0
    case Mul_flt(Deff(Add_flt(_,_)),Deff(Sub_flt(_,_))) => 0
    case Mul_flt(Deff(Sub_flt(_,_)),Deff(Add_flt(_,_))) => 0
    case Mul_flt(Deff(Sub_flt(_,_)),Deff(Sub_flt(_,_))) => 0

    case Mul_flt(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (11)

    case Div_flt(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (33)

    case Lt_flt(a,_)  =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case Leq_flt(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case Neq_flt(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case Eql_flt(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case Not_bit(_)   => 1
    case And_bit(_,_) => 1
    case Or_bit(_,_)  => 1
    case Xor_bit(_,_)  => 1
    case Xnor_bit(_,_)  => 1

    case Abs_fix(_) => 1
    case Abs_flt(_) => 1
    case Log_flt(_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (35)

    case Exp_flt(_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (27)

    case Sqrt_flt(_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (28)

    case Mux2(_,_,_) => 1

    case Convert_fixpt(_) => 1
    //case Convert_fltpt(_) => // ???
    case Fixpt_to_fltpt(x) =>
      if (nbits(s) != 32 && nbits(x) != 32) warn(s"Don't know latency for $d - using default")
      (6)

    case Fltpt_to_fixpt(x) =>
      if (nbits(s) != 32 && nbits(x) != 32) warn(s"Don't know latency for $d - using default")
      (6)

    case TileTransfer(mem,local,_,_,_,cc,_,false) =>
      val c = contentionOf(s)
      val ts = dimsOf(local).map(d => bound(d).getOrElse(stageError("Cannot resolve bound of BRAM dimension")))
      val r = if (ts.length == 1) 1 else ts(0)
      val b = if (ts.length == 1) ts(0) else ts.drop(1).reduce{_*_}
      val p = parOf(cc).reduce{_*_}

      //System.out.println(s"Tile transfer $s: c = $c, r = $r, b = $b, p = $p")
      memoryModel(c,r.toInt,b.toInt,p)

    case TileTransfer(mem,local,_,_,_,cc,_,true) =>
      val c = contentionOf(s)
      val p = parOf(cc).reduce{_*_}
      val sizes = dimsOf(local).map(d => bound(d).getOrElse(stageError("Cannot resolve bound of BRAM dimension")))

      val baseCycles = sizes.reduce{_*_} / p.toDouble

      val oFactor = 0.02*c - 0.019
      val smallOverhead = if (c < 8) 0.0 else 0.0175
      val overhead = if (p < 8) 1.0 + smallOverhead*p else oFactor*p + (1 - (8*oFactor)) + smallOverhead*8

      //System.out.println(s"Sizes: $sizes, base cycles: $baseCycles, ofactor: $oFactor, smallOverhead: $smallOverhead, overhead: $overhead")

      Math.ceil(baseCycles*overhead).toLong

    case _:Pipe_parallel => 1
    case _:Pipe_foreach  => 1
    case _:Pipe_fold[_,_] => 1
    case _:Accum_fold[_,_]  => 1
    case _:Unit_pipe => 0

    case Reg_read(s) if regType(s) == ArgumentIn => 0
    case Reg_read(s) => 0

    case Reg_write(_,_) => 1
    case Reg_reset(_)   => 0
    case Offchip_new(_) => 0

    case Reflect(d,_,_) => latencyOfNode(s, d)
    case Reify(_,_,_) => 0
    case _ =>
      warn(s"Don't know latency of $d")
      (0)
  }}
}

