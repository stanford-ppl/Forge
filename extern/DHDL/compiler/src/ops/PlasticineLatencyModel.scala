package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait PlasticineLatencyModel {
  val IR: DHDLExp with NodeMetadataOpsExp
  import IR._

  var CLK = 1500.0f // Clock frequency in MHz
  var LANES = 16

  lazy val memModel = new TileLoadModel()

  private var silentModel = false
  private def warn(x: => String) { if (!silentModel) stageWarn(x) }
  def silence() { silentModel = true }

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
  private def memoryModel(c: Int, r: Int, b: Int, p: Int) = {
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
    case Argin_new(_) => 0
    case Argout_new(_) => 0
    case Bram_new(_,_) => 0
    case Fifo_new(_,_) => 0
    case Cam_new(_,_) => 0

    // TODO
    case Push_fifo(fifo,_,_) => 0
    case Pop_fifo(fifo) => 0
    case Count_fifo(fifo) => 0

    // TODO
    case Cam_load(cam,key) => 1
    case Cam_store(cam,key,value) => 1

    // TODO: Not a function of number of banks?
    case Bram_load(ram, _) => 0 //if (isDblBuf(ram)) 2 else 1
    case Bram_store(ram, _, _) => 0 //if (isDblBuf(ram)) 2 else 1

    case _:Counter_new => 0
    case _:Counterchain_new => 0

    // TODO: Have to get numbers for non-32 bit multiplies and divides
    case FixPt_Neg(_)   => 1

    case FixPt_Add(_,_) => 1
    case FixPt_Sub(_,_) => 1
    case FixPt_Mul(_,_) => 1
    case FixPt_Div(_,_) => 1
    case FixPt_Mod(_,_) => 1  // Supported?

    case FixPt_Lt(_,_)  => 1
    case FixPt_Leq(_,_) => 1
    case FixPt_Neq(_,_) => 1
    case FixPt_Eql(_,_) => 1
    case FixPt_And(_,_) => 1
    case FixPt_Or(_,_)  => 1

    case FltPt_Neg(_)   => 1
    case FltPt_Add(_,_) => 1
    case FltPt_Sub(_,_) => 1
    case FltPt_Mul(_,_) => 1
    case FltPt_Div(_,_) => 1

    case FltPt_Lt(a,_)  => 1
    case FltPt_Leq(a,_) => 1
    case FltPt_Neq(a,_) => 1
    case FltPt_Eql(a,_) => 1

    case Bit_Not(_)   => 1
    case Bit_And(_,_) => 1
    case Bit_Or(_,_)  => 1
    case Bit_Xor(_,_)  => 1
    case Bit_Xnor(_,_)  => 1

    case FixPt_Abs(_) => 1
    case FltPt_Abs(_) => 1
    case FltPt_Log(_) => 1  // Supported?
    case FltPt_Exp(_)  => 1 // Supported?
    case FltPt_Sqrt(_) => 1 // Supported?
    case Mux2(_,_,_) => 1
    case Min2(_,_) => 1
    case Max2(_,_) => 1

    case Convert_fixpt(_) => 1
    case Convert_fltpt(_) => 1  // Supported?
    case Fixpt_to_fltpt(x) => 1 // Supported?

    case Fltpt_to_fixpt(x) => 1 // Supported?

    // TODO
    case Offchip_store_cmd(mem,stream,ofs,len,par) =>
      val c = contentionOf(s)
      val p = 16.0 //bound(par).get
      val size = bound(len).getOrElse{stageWarn("Cannot resolve bound of offchip store"); 96.0}

      val baseCycles = size / p.toDouble

      val oFactor = 0.02*c - 0.019
      val smallOverhead = if (c < 8) 0.0 else 0.0175
      val overhead = if (p < 8) 1.0 + smallOverhead*p else oFactor*p + (1 - (8*oFactor)) + smallOverhead*8

      //System.out.println(s"Sizes: $sizes, base cycles: $baseCycles, ofactor: $oFactor, smallOverhead: $smallOverhead, overhead: $overhead")
      Math.ceil(baseCycles*overhead).toLong

    case Offchip_load_cmd(mem,stream,ofs,len,par) =>
      val c = contentionOf(s)
      val ts = bound(len).getOrElse{stageWarn("Cannot resolve bound of offchip load"); 96.0}
      val b = ts  // TODO - max of this and max command size
      val r = 1.0 // TODO - number of commands needed (probably 1)
      val p = 16.0 //bound(par).get
      //System.out.println(s"Tile transfer $s: c = $c, r = $r, b = $b, p = $p")
      memoryModel(c,r.toInt,b.toInt,p.toInt)

    case _:Pipe_parallel => 1
    case _:Unit_pipe => 0
    case _:Pipe_foreach  => 1
    case _:Pipe_fold[_,_] => 1
    case _:Accum_fold[_,_]  => 1

    case Reg_read(s) => 0
    case Reg_write(_,_) => 0
    case Reg_reset(_)   => 0
    case Offchip_new(_) => 0

    case Reflect(d,_,_) => latencyOfNode(s, d)
    case Reify(_,_,_) => 0
    case _ =>
      warn(s"Don't know latency of $d")
      (0)
  }}
}

