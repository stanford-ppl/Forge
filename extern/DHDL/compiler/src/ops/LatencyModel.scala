package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait LatencyModel extends PipeStageToolsExp {
  this: DHDLExp =>

  lazy val memModel = new TileLoadModel()

  private var silentModel = false
  private def warn(x: => String) { if (!silentModel) stageWarn(x) }
  def silenceLatencyModel() { silentModel = true }

  def latencyOf(s: Exp[Any], inReduce: Boolean) = s match {
    case Def(d) if inReduce  => latencyOfNodeInReduce(s, d)
    case Def(d) if !inReduce => latencyOfNode(s, d)
    case _ => 0
  }

  private def latencyOfNodeInReduce(s: Exp[Any], d: Def[Any]): Long = d match {
    case FltPt_Add(_,_) => 1
    case Reg_write(_,_) => 0
    case Reflect(d,_,_) => latencyOfNodeInReduce(s,d)
    case _ => latencyOfNode(s, d)
  }


  // TODO: These need new numbers after template changes
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
      case (Deff(FltPt_Mul(_,_)), Deff(FltPt_Mul(`rV1`,`rV2`))) => true
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
    case FixPt_Neg(_)   => 1

    // HACK: Fused multiply-add in index calculation
    case FixPt_Add(Deff(FixPt_Mul(_,_)),Exact(_)) => 0
    case FixPt_Add(Deff(FixPt_Mul(_,_)),Def(_)) => 1
    case FixPt_Add(Deff(FixPt_Mul(_,_)),_) => 0
    case FixPt_Add(Exact(_),Deff(FixPt_Mul(_,_))) => 0
    case FixPt_Add(Def(_),Deff(FixPt_Mul(_,_))) => 1
    case FixPt_Add(_,Deff(FixPt_Mul(_,_))) => 0

    case FixPt_Add(_,_) => 1

    case FixPt_Sub(_,_) => 1
    case FixPt_Mul(Exact(_),_) => 1
    case FixPt_Mul(_,Exact(_)) => 1
    case FixPt_Mul(_,_) =>
      if (nbits(s) > 32) warn(s"Don't know latency for $d - using default")
      if (nbits(s) <= 18) 1 else 2

    case FixPt_Div(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      if (sign(s)) 35 else 38

    case FixPt_Mod(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      if (sign(s)) 35 else 38

    case FixPt_Lt(_,_)  => 1
    case FixPt_Leq(_,_) => 1
    case FixPt_Neq(_,_) => 1
    case FixPt_Eql(_,_) => 1
    case FixPt_And(_,_) => 1
    case FixPt_Or(_,_)  => 1

    //case Lsh_fix(_,_) => // ???
    //case Rsh_fix(_,_) => // ???

    // TODO: Floating point for things besides single precision
    case FltPt_Neg(_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (1)

    case FltPt_Add(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (14)

    case FltPt_Sub(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (14)

    // MaxJ does floating point multiply-add fusion
    case FltPt_Mul(Deff(FltPt_Add(_,_)),Deff(FltPt_Add(_,_))) => 0
    case FltPt_Mul(Deff(FltPt_Add(_,_)),Deff(FltPt_Sub(_,_))) => 0
    case FltPt_Mul(Deff(FltPt_Sub(_,_)),Deff(FltPt_Add(_,_))) => 0
    case FltPt_Mul(Deff(FltPt_Sub(_,_)),Deff(FltPt_Sub(_,_))) => 0

    case FltPt_Mul(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (11)

    case FltPt_Div(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (33)

    case FltPt_Lt(a,_)  =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case FltPt_Leq(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case FltPt_Neq(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case FltPt_Eql(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case Bit_Not(_)   => 1
    case Bit_And(_,_) => 1
    case Bit_Or(_,_)  => 1
    case Bit_Xor(_,_)  => 1
    case Bit_Xnor(_,_)  => 1

    case FixPt_Abs(_) => 1
    case FltPt_Abs(_) => 1
    case FltPt_Log(_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (35)

    case FltPt_Exp(_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (27)

    case FltPt_Sqrt(_) =>
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

    // TODO
    case Offchip_store_vector(mem,ofs,vec) =>
      val c = contentionOf(s)
      val p = 1
      val sizes = dimsOf(vec).map(d => bound(d).getOrElse(stageError("Cannot resolve bound of Vector dimension")))

      val baseCycles = sizes.reduce{_*_} / p.toDouble

      val oFactor = 0.02*c - 0.019
      val smallOverhead = if (c < 8) 0.0 else 0.0175
      val overhead = if (p < 8) 1.0 + smallOverhead*p else oFactor*p + (1 - (8*oFactor)) + smallOverhead*8

      //System.out.println(s"Sizes: $sizes, base cycles: $baseCycles, ofactor: $oFactor, smallOverhead: $smallOverhead, overhead: $overhead")

      Math.ceil(baseCycles*overhead).toLong


    case Offchip_load_vector(mem,ofs,len) =>
      val c = contentionOf(s)
      val ts = bound(len).getOrElse(stageError("Cannot resolve bound of Vector dimension"))
      val b = ts  // TODO - max of this and max command size
      val r = 1.0 // TODO - number of commands needed (probably 1)
      val p = 1   // TODO - need to recharacterize this...
      //System.out.println(s"Tile transfer $s: c = $c, r = $r, b = $b, p = $p")
      memoryModel(c,r.toInt,b.toInt,p)


    case _:Bram_store_vector[_] => 0
    case _:Bram_load_vector[_] => 0

    case _:Pipe_parallel => 1
    case _:Unit_pipe => 0
    case _:Pipe_foreach  => 1
    case _:Pipe_fold[_,_] => 1
    case _:Accum_fold[_,_]  => 1

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

