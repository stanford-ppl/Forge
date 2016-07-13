package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

case class AppStatistics(
  insts: Long = 0,
  flops: Long = 0,
  onChipIn: Long = 0,     // In bits
  onChipOut: Long = 0,    // In bits
  dataIn: Long = 0,       // In bits
  dataOut: Long = 0       // In bits
) {
  def +(that: AppStatistics) = AppStatistics(
    this.insts + that.insts,
    this.flops + that.flops,
    this.onChipIn + that.onChipIn,
    this.onChipOut + that.onChipOut,
    this.dataIn + that.dataIn,
    this.dataOut + that.dataOut
  )

  def *(b: Long) = AppStatistics(
    this.insts * b,
    this.flops * b,
    this.onChipIn * b,
    this.onChipOut * b,
    this.dataIn * b,
    this.dataOut * b
  )

  override def toString() = s"Instructions: $insts, FLOPs: $flops, OnChip(In): $onChipIn, OnChip(Out): $onChipOut, OffChip(In): $dataIn, OffChip(Out): $dataOut"
}

// Helps to provide an estimate of ops and bandwidth
trait OpsModel extends PipeStageToolsExp {
  this: DHDLExp =>

  object NoOps extends AppStatistics()
  object Instruction extends AppStatistics(insts=1)
  object FLOP extends AppStatistics(insts=1, flops=1)

  def opsIn(e: Exp[Any], inHwScope: Boolean): AppStatistics = e match {
    case Def(EatReflect(d: Reg_new[_])) if isArgIn(e) => AppStatistics(dataIn = nbits(d._mT))
    case Def(EatReflect(d: Reg_new[_])) if isArgOut(e) => AppStatistics(dataOut = nbits(d._mT))
    case Def(d) if inHwScope => opsInNode(e, d)
    case _ => NoOps
  }

  private def opsInNode(s: Exp[Any], d: Def[Any]): AppStatistics = s match {
    case Fixed(_) => NoOps
    case _ => d match {

    case e: Push_fifo[_] => AppStatistics(insts=2, onChipIn = nbits(e._mT))
    case e: Pop_fifo[_] => AppStatistics(insts=2, onChipOut = nbits(e._mT))
    case e: Count_fifo[_] => Instruction

    case e: Cam_load[_,_] => AppStatistics(insts=1, onChipOut = nbits(e._mV))
    case e: Cam_store[_,_] => AppStatistics(insts=1, onChipIn = nbits(e._mV))

    // TODO: Should this count if ram will be implemented as regs?
    case e: Bram_load[_] => AppStatistics(insts=1,onChipOut = nbits(e._mT))
    case e: Bram_store[_] => AppStatistics(insts=1,onChipIn = nbits(e._mT))

    case e: Reg_read[_] => Instruction
    case e: Reg_write[_] => Instruction

    case e: Counterchain_new => AppStatistics(insts=lenOf(s))

    case FixPt_Neg(_)   => Instruction
    case FixPt_Add(_,_) => Instruction
    case FixPt_Sub(_,_) => Instruction
    case FixPt_Mul(_,_) => Instruction
    case FixPt_Div(_,_) => Instruction
    case FixPt_Mod(_,_) => Instruction
    case FixPt_Lt(_,_)  => Instruction
    case FixPt_Leq(_,_) => Instruction
    case FixPt_Neq(_,_) => Instruction
    case FixPt_Eql(_,_) => Instruction
    case FixPt_And(_,_) => Instruction
    case FixPt_Or(_,_)  => Instruction
    case FixPt_Lsh(_,_) => Instruction
    case FixPt_Rsh(_,_) => Instruction

    case FltPt_Neg(_) => Instruction // Floating point negation is super easy
    case FltPt_Add(_,_) => FLOP
    case FltPt_Sub(_,_) => FLOP
    case FltPt_Mul(_,_) => FLOP
    case FltPt_Div(_,_) => AppStatistics(insts=1, flops=4) // Apparently standard
    case FltPt_Lt(_,_)  => FLOP // because why not
    case FltPt_Leq(_,_) => FLOP
    case FltPt_Neq(_,_) => FLOP
    case FltPt_Eql(_,_) => FLOP

    case Bit_Not(_)   => Instruction
    case Bit_And(_,_) => Instruction
    case Bit_Or(_,_)  => Instruction
    case Bit_Xor(_,_) => Instruction
    case Bit_Xnor(_,_) => Instruction

    case FixPt_Abs(_) => Instruction
    case FltPt_Abs(_) => Instruction
    case FltPt_Log(_) => AppStatistics(insts=1, flops=5) // ???
    case FltPt_Exp(_) => AppStatistics(insts=1, flops=3) // ???
    case FltPt_Sqrt(_) => AppStatistics(insts=1, flops=3) // ???

    case Mux2(_,_,_) => Instruction
    case Min2(_,_) => AppStatistics(insts=2)
    case Max2(_,_) => AppStatistics(insts=2)
    case Convert_fixpt(_) => Instruction
    case Convert_fltpt(_) => FLOP // ???
    case Fixpt_to_fltpt(x) => FLOP // ???
    case Fltpt_to_fixpt(_) => FLOP // ???

    case e@Offchip_store_cmd(mem,stream,ofs,len,p) =>
      val bits = nbits(e._mT)
      val size = bound(len).getOrElse{stageError(s"Cannot resolve bound of tile vector store size $len")}
      AppStatistics(dataOut=bits*size.toLong)

    case e@Offchip_load_cmd(mem,stream,ofs,len,p) =>
      val bits = nbits(e._mT)
      val size = bound(len).getOrElse{stageError(s"Cannot resolve bound of tile vector load size $len")}
      AppStatistics(dataIn=bits*size.toLong)

    case Reflect(d,_,_) => opsInNode(s, d)
    case _ => NoOps
  }}
}
