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

  case class CUContext(pipe: Exp[Any], cu: ComputeUnit) {
    private val regs = HashMap[Exp[Any], LocalMem]()  // Exp to Reg mapping
    private val exps = HashMap[LocalMem, List[Exp[Any]]]()  // Reg to Exp mapping
    def addReg(exp: Exp[Any], reg: LocalMem) {
      regs += exp -> reg
      if (exps.contains(reg))
        exps += reg -> (exps(reg) :+ exp)
      else
        exps += reg -> List(exp)
    }
    cu.iterators.toList.foreach{case (i,p) => addReg(i, CounterReg(p._1,p._2)) }

    var stages: List[PIRStage] = Nil
    def stageNum = stages.filter(_.isInstanceOf[Stage]).length
    def currentStage = stages.head
    def addStage(stage: PIRStage) { stages ::= stage }

    def apply(x: Exp[Any]): LocalMem = regs.getOrElseUpdate(x, {
      val in = cu.scalarIn.find(_.name == quote(x))
      if (in.isDefined)
        InputReg(stageNum, in.get)
      else if (isAccum(x) && isRegister(x.tp)) {
        val init = apply(resetValue(x.asInstanceOf[Exp[Reg[Any]]])) match {
          case const: ConstReg => const
          case _ => stageError(s"No initial value lookup available for $x")
        }
        cu.tempWithInit(stageNum, init)
      }
      else x match {
        case Exact(c) => ConstReg(s"${c.toLong}l")
        case _ => stageError(s"No register lookup available for $x")
      }
    })
    def mem(x: Exp[Any]) = cu.srams.find{_.name == quote(x)}.get
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && cus.contains(lhs))
      scheduleCU(lhs, cus(lhs))
  }

  def scheduleCU(pipe: Exp[Any], cu: ComputeUnit) {
    debug(s"Scheduling $pipe CU: $cu")
    val ctx = CUContext(pipe, cu)
    // TODO: Should ensure that reduction stages come immediately after their dependencies
    cu.stages.foreach{
      case DefStage(lhs@Deff(rhs), isReduce, isWrite) => nodeToStage(lhs, rhs, ctx, isReduce)
    }
    debug("Generated stages: ")
    ctx.stages.reverse.foreach(stage => debug(s"  $stage"))
    cu.stages = ctx.stages.reverse
  }

  def nodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext, isReduce: Boolean) {
    debug(s"  $op = $d")
    if (isReduce) reduceNodeToStage(lhs,rhs,ctx)
    else          mapNodeToStage(lhs,rhs,ctx)
  }

  def mapNodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = rhs match {
    // --- Reads
    case Pop_fifo(fifo)             => ctx.addReg(lhs, InputMem(ctx.mem(fifo)))
    case Par_pop_fifo(fifo, len)    => ctx.addReg(lhs, InputMem(ctx.mem(fifo)))
    case Bram_load(bram, addr)      => ctx.addReg(lhs, InputMem(ctx.mem(bram)))
    case Par_bram_load(bram, addrs) => ctx.addReg(lhs, InputMem(ctx.mem(bram)))
    case Vec_apply(vec, idx)        =>
      if (idx != 0) stageError("Cannot generate non-zero vector apply")
      ctx.addReg(lhs, ctx(vec))

    case Reg_read(reg) => ctx.addReg(lhs, ctx(reg))

    // --- Writes
    case Push_fifo(fifo,value) =>
    case Par_push_fifo(fifo,values) =>
    case Bram_store(bram, addr, value) =>
    case Par_bram_store(bram, addrs, values) =>

    case Reg_write(reg, value) =>
      if ((isAccum(reg) || ctx.regs.contains(reg)) && !isInnerAccum(reg)) {
        val out = ctx(reg)
        ctx.currentStage match {
          case stage: Stage =>
            // HACK: Update reg mapping
            val lhs = ctx.regs.find{case (lhs,reg) => reg == stage.out}.map(_._1)
            lhs.foreach{exp => ctx.regs += exp -> out}
            ctx.cu.unuseReg(stage.out)
            stage.out = out
          case _:ReduceStage => // Should already be handled?
        }
      }
      if (ctx.cu.scalarOut.exists(_.name == quote(lhs))) {
        val outName = ctx.cu.scalarOut.find(_.name == quote(lhs)).get
        ctx.currentStage match {
          case stage: Stage if !isAccum(reg) =>
            // HACK: Update reg mapping
            val out = OutputReg(ctx.stageNum-1, outName)
            val lhs = ctx.regs.find{case (lhs,reg) => reg == stage.out}.map(_._1)
            lhs.foreach{exp => ctx.regs += exp -> out}
            ctx.cu.unuseReg(stage.out)
            stage.out = out

          case _ =>
            // Can't directly write scalar output from reduction stage
            val stage = Stage(Bypass, List(ctx(value)), OutputReg(ctx.stageNum,outName))
            ctx.addStage(stage)
        }
      }

      else
        ctx.regs += reg -> ctx(value)

    case _ => nodeToOp(rhs) match {
      case Some(op) =>
        val inputs = syms(rhs).map{sym => ctx(sym) }
        val output = ctx.cu.temp(ctx.stageNum)
        val stage = Stage(op, inputs, output)
        ctx.regs += lhs -> output
        ctx.addStage(stage)

      case None => stageWarn(s"No PIR scheduling rule known for $lhs = $rhs")
    }
  }

  def reduceNodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = nodeToOp(rhs) match {
    case Some(op) =>
      // By convention, the inputs to the reduction tree is the first argument to the node
      // This input must be in the previous stage's reduction register
      // Ensure this either by adding a bypass register for raw inputs or changing the output
      // of the previous stage from a temporary register to the reduction register
      // FIXME: Rather hacky. Should guarantee ordering to avoid stage error with map stage
      val input = ctx(syms(rhs).head)
      // HACK: Get initial value of accumulator
      val zero = syms(rhs).last match {
        case Deff(Reg_read(acc)) => ctx(resetValue(acc))
        case _ => ConstReg("0l")
      }
      input match {
        case _:ReduceReg | _: AccumReg | _: TempAccumReg => // ok?
        case _:InputReg | _:InputMem | _:CounterReg | _:ConstReg =>
          val output = ReduceReg(ctx.stageNum)
          val bypass = Stage(Bypass, List(input), output)
          ctx.addStage(bypass)

        case t: TempReg =>
          ctx.currentStage match {
            case stage@Stage(op,inputs,`t`) =>
              ctx.cu.tempRegs -= t
              stage.out = ReduceReg(ctx.stageNum-1)
            case _ => stageError("Input to reduction must be from immediately preceeding map tage")
          }
      }
      val acc = ctx.cu.acc()
      val stage = ReduceStage(op, zero, acc)
      ctx.regs += lhs -> acc
      ctx.addStage(stage)
    case _ => stageWarn(s"No PIR reduce rule known for $lhs = $rhs")
  }

  def nodeToOp(node: Def[Any]): Option[PIROp] = node match {
    case Mux2(_,_,_)    => Some(ALUMux)
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
