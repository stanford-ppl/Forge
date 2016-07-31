package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait PIRScheduler extends Traversal with SubstQuotingExp {
  val IR: DHDLExp with PIRScheduleAnalysisExp
  import IR._

  override val name = "PIR Scheduler"
  override val recurse = Always
  debugMode = true

  val cus = HashMap[Exp[Any], ComputeUnit]()

  case class CUContext(regs: HashMap[Exp[Any], LocalMem], cu: ComputeUnit) {
    var stages: List[PIRStage] = Nil
    def stageNum = stages.length
    def currentStage = stages.head
    def addStage(stage: PIRStage, isReduce: Boolean = false) { stages ::= stage }

    def apply(x: Exp[Any]) = {
      val in = cu.scalarIn.find(_.name == quote(x))
      if (regs.contains(x)) regs(x)
      else if (in.isDefined) InputReg(in.get)
      else x match {
        case Exact(c) => ConstReg(s"${c.toLong}l")
        case _ => stageError(s"No register lookup available for $x")
      }
    }
    def mem(x: Exp[Any]) = cu.srams.find{_.name == quote(x)}.get
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && cus.contains(lhs))
      scheduleCU(lhs, cus(lhs))
  }

  def scheduleCU(pipe: Exp[Any], cu: ComputeUnit) {
    debug(s"Scheduling $pipe CU: $cu")
    cu.stages.foreach{
      case DefStage(op@Def(d), isReduce, isWrite) => debug(s"  $op = $d")
      case PseudoStage(op, inputs, isReduce, isWrite) => debug(s"  $op [Pseduo]")
      case DefStage(op, isReduce, isWrite) =>
        stageError(s"Found op without def $op")
    }
    val regs = HashMap[Exp[Any], LocalMem]()
    regs ++= cu.iterators.toList.map{case (i,p) => i -> CounterReg(p._1,p._2) }

    val ctx = CUContext(regs, cu)
    // TODO: Should ensure that reduction stages come immediately after their dependencies

    cu.stages.foreach{
      case DefStage(lhs@Deff(rhs), isReduce, isWrite) => nodeToStage(lhs, rhs, ctx, isReduce)
    }

    debug("Generated stages: ")
    ctx.stages.reverse.foreach(stage => debug(s"  $stage"))
  }

  def nodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext, isReduce: Boolean) = {
    if (isReduce) reduceNodeToStage(lhs,rhs,ctx)
    else          mapNodeToStage(lhs,rhs,ctx)
  }

  def mapNodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = rhs match {
    case Pop_fifo(fifo) =>
      ctx.regs += lhs -> InputMem(ctx.mem(fifo))
    case Par_pop_fifo(fifo, len) =>
      ctx.regs += lhs -> InputMem(ctx.mem(fifo))
    case Bram_load(bram, addr) =>
      ctx.regs += lhs -> InputMem(ctx.mem(bram))
    case Par_bram_load(bram, addrs) =>
      ctx.regs += lhs -> InputMem(ctx.mem(bram))
    case Vec_apply(vec, idx) =>
      if (idx != 0) stageError("Cannot generate non-zero vector apply")
      ctx.regs += lhs -> ctx(vec)

    case Reg_read(reg) =>
      ctx.regs += lhs -> ctx(reg)

    case Reg_write(reg, value) =>
      if (ctx.cu.scalarOut.exists(_.name == quote(lhs))) {
        val out = ctx.cu.scalarOut.find(_.name == quote(lhs)).get
        ctx.currentStage match {
          case stage: Stage => stage.out = OutputReg(ctx.stageNum-1, out)
          case _: ReduceStage =>
            // Can't directly write scalar output from reduction stage
            val stage = Stage(Bypass, List(ctx(value)), OutputReg(ctx.stageNum,out))
            ctx.addStage(stage)
        }
      }
      else
        ctx.regs += reg -> ctx(value)

    case _ => nodeToOp(rhs) match {
      case Some(op) =>
        val inputs = syms(rhs).map{sym => ctx(sym) }
        val stage = Stage(op, inputs, TempReg(ctx.stageNum))
        ctx.addStage(stage)

      case None => stageWarn(s"No PIR scheduling rule known for $lhs = $rhs")
    }
  }

  // Seems hacky..
  def reduceNodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = nodeToOp(rhs) match {
    case Some(op) =>
      val stage = ReduceStage(op)
      ctx.regs += lhs -> ReduceReg(ctx.stageNum)
      ctx.addStage(stage)
    case _ => stageWarn(s"No PIR reduce rule known for $lhs = $rhs")
  }

  def nodeToOp(node: Def[Any]): Option[PIROp] = node match {
    case Mux2(_,_,_) => Some(ALUMux)
    case FixPt_Add(_,_) => Some(FixAdd)
    case FixPt_Sub(_,_) => Some(FixSub)
    case FixPt_Mul(_,_) => Some(FixMul)
    case FixPt_Div(_,_) => Some(FixDiv)
    case FltPt_Add(_,_) => Some(FltAdd)
    case FltPt_Sub(_,_) => Some(FltSub)
    case FltPt_Mul(_,_) => Some(FltMul)
    case FltPt_Div(_,_) => Some(FltDiv)
    case _ => None
  }

}
