package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait LatencyModel {
  this: DHDLExp with CounterToolsExp =>

  private var silentModel = false
  private def warn(x: => String) { if (!silentModel) stageWarn(x) }
  def silenceLatencyModel() { silentModel = true }

  def latencyOf(s: Exp[Any], inReduce: Boolean) = s match {
    case Def(d) if inReduce  => latencyOfNodeInReduce(s, d)
    case Def(d) if !inReduce => latencyOfNode(s, d)
    case _ => 0
  }

  private def latencyOfNodeInReduce(s: Exp[Any], d: Def[Any]): Long = d match {
    case DHDLPrim_Add_flt(_,_) => 1
    case Reg_write(_,_) => 0
    case Reflect(d,_,_) => latencyOfNodeInReduce(s,d)
    case _ => latencyOfNode(s, d)
  }


  // TODO: These need new numbers after Raghu's changes
  // c - contention (1 to 13) (number of readers/writers in parallel)
  // r - number of commands (>= 1)
  // b - number of words per command (step of 96)
  // p - number of words read into buffer in parallel
  def memoryModel(c: Int, r: Int, b: Int, p: Int) = {
    val overhead12 = b match {
      case 96  => 0.307/(1 + Math.exp(-0.096*r + 0.21))   // Logistic, bounded by 30.7%
      case 192 => 0.185/(1 + Math.exp(-0.24*r - 0.8))     // Logistic, bounded by 18.5%
      case _ => 0.165
    }
    val overhead = ((1/Math.log(12))*Math.log(c))*overhead12
    Math.ceil( (1+overhead)*(110 + r*(53 + b)) ).toLong
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
    case DHDLPrim_Neg_fix(_)   => 1
    case DHDLPrim_Add_fix(_,_) => 1
    case DHDLPrim_Sub_fix(_,_) => 1
    case DHDLPrim_Mul_fix(_,_) =>
      if (nbits(s) > 32) warn(s"Don't know latency for $d - using default")
      if (nbits(s) <= 18) 1 else 2

    case DHDLPrim_Div_fix(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      if (sign(s)) 35 else 38

    case DHDLPrim_Mod_fix(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      if (sign(s)) 35 else 38

    case DHDLPrim_Lt_fix(_,_)  => 1
    case DHDLPrim_Leq_fix(_,_) => 1
    case DHDLPrim_Neq_fix(_,_) => 1
    case DHDLPrim_Eql_fix(_,_) => 1
    case DHDLPrim_And_fix(_,_) => 1
    case DHDLPrim_Or_fix(_,_)  => 1

    //case DHDLPrim_Lsh_fix(_,_) => // ???
    //case DHDLPrim_Rsh_fix(_,_) => // ???

    // TODO: Floating point for things besides single precision
    case DHDLPrim_Neg_flt(_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (1)

    case DHDLPrim_Add_flt(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (14)

    case DHDLPrim_Sub_flt(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (14)

    case DHDLPrim_Mul_flt(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (11)

    case DHDLPrim_Div_flt(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (33)

    case DHDLPrim_Lt_flt(a,_)  =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case DHDLPrim_Leq_flt(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case DHDLPrim_Neq_flt(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case DHDLPrim_Eql_flt(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know latency for $d - using default")
      (3)

    case DHDLPrim_Not_bit(_)   => 1
    case DHDLPrim_And_bit(_,_) => 1
    case DHDLPrim_Or_bit(_,_)  => 1
    case DHDLPrim_Xor_bit(_,_)  => 1
    case DHDLPrim_Xnor_bit(_,_)  => 1

    case DHDLPrim_Abs_fix(_) => 1
    case DHDLPrim_Abs_flt(_) => 1
    case DHDLPrim_Log_flt(_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (35)

    case DHDLPrim_Exp_flt(_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (27)

    case DHDLPrim_Sqrt_flt(_) =>
      if (nbits(s) != 32) warn(s"Don't know latency for $d - using default")
      (28)

    case BasicCtrl1_Mux(_,_,_) => 1

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
      val p = parOf(cc).reduce{_*_}
      val s = dimsOf(local).map(d => bound(d).getOrElse(stageError("Cannot resolve bound of BRAM dimension")))
      Math.ceil(s.reduce{_*_}/p.toDouble).toLong

    case _:Pipe_parallel => 1
    case _:Pipe_foreach  => 1
    case _:Pipe_reduce[_,_] => 1
    case _:Block_reduce[_]  => 1
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

