package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait PIRScheduler extends Traversal with PIRCommon {
  val IR: DHDLExp with PIRScheduleAnalysisExp
  import IR._

  override val name = "PIR Scheduler"
  override val recurse = Always
  debugMode = true

  val cus = HashMap[Exp[Any], ComputeUnit]()

  // TODO: Awkward extension of ComputeUnit. May want to move all this to CU later?
  abstract class CUContext(val pipe: Exp[Any], val cu: ComputeUnit) {
    private val refs = HashMap[Exp[Any],LocalRef]()

    def pseudoStages: List[PseudoStage]
    def stages: List[Stage]
    def addStage(stage: Stage): Unit
    def finalizeCU: Unit

    def stageNum = stages.filter(_.isInstanceOf[MapStage]).length
    def currentStage = stages.head

    def mem(x: Exp[Any]) = cu.srams.find(_.name == quote(x)).get
    def addReg(x: Exp[Any], reg: LocalMem) { cu.addReg(x, reg) }
    def replaceReg(x: Exp[Any], reg: LocalMem) { cu.replaceReg(x, reg) }
    def addRef(x: Exp[Any], ref: LocalRef) { refs += x -> ref }
    def getReg(x: Exp[Any]) = cu.get(x)
    def reg(x: Exp[Any]) = cu.get(x).getOrElse(stageError(s"No register defined for $x"))

    // Add a stage which bypasses x to y
    def bypass(x: LocalMem, y: LocalMem) {
      val stage = MapStage(Bypass, List(refIn(x)), refOut(y))
      addStage(stage)
    }

    def ref(reg: LocalMem, out: Boolean, stage: Int = stageNum) = {
      if (reg.isInstanceOf[ScalarIn] || out)
        LocalRef(stage, reg)
      else
        LocalRef(stage-1, reg)
    }
    def refIn(reg: LocalMem, stage: Int = stageNum) = ref(reg, false, stage)
    def refOut(reg: LocalMem, stage: Int = stageNum) = ref(reg, true, stage)
  }
  case class ComputeContext(override val pipe: Exp[Any], override val cu: ComputeUnit) extends CUContext(pipe,cu) {
    def pseudoStages = cu.computePseudoStages
    def stages = cu.stages
    def addStage(stage: Stage) { cu.stages ::= stage }
    def finalizeCU { cu.stages = cu.stages.reverse }
  }
  case class WriteContext(override val pipe: Exp[Any], override val cu: ComputeUnit, mem: CUMemory) extends CUContext(pipe,cu) {
    cu.writeStages += mem -> Nil

    def pseudoStages = cu.writePseudoStages(mem)
    def stages = cu.writeStages(mem)
    def addStage(stage: Stage) { cu.writeStages(mem) = cu.writeStages(mem) :+ stage }
    def finalizeCU {}
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && cus.contains(lhs))
      scheduleCU(lhs, cus(lhs))
  }

  def scheduleCU(pipe: Exp[Any], cu: ComputeUnit) {
    debug(s"Scheduling $pipe CU: $cu")
    for (mem <- cu.writePseudoStages.keys) {
      val writes = WriteContext(pipe, cu, mem)
      scheduleContext(writes)
    }
    val compute = ComputeContext(pipe, cu)
    scheduleContext(compute)

    for (mem <- cu.writePseudoStages.keys) {
      debug(s"Generated write stages (${mem.name}): ")
      cu.writeStages(mem).foreach(stage => debug(s"  $stage"))
    }
    debug("Generated compute stages: ")
    cu.stages.foreach(stage => debug(s"  $stage"))
  }
  def scheduleContext(ctx: CUContext) {
    ctx.pseudoStages.foreach{
      case DefStage(lhs@Deff(rhs), isReduce) => nodeToStage(lhs, rhs, ctx, isReduce)
      case WriteAddrStage(lhs@Deff(rhs)) => writeAddrToStage(lhs, rhs, ctx)
    }
    ctx.finalizeCU
  }



  def nodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext, isReduce: Boolean) {
    debug(s"  $lhs = $rhs")
    if (isReduce) reduceNodeToStage(lhs,rhs,ctx)
    else          mapNodeToStage(lhs,rhs,ctx)
  }

  def allocateAddrReg(addr: Exp[Any], ctx: CUContext, write: Boolean) {
    lazy val reg = if (write) WriteAddrReg(addr) else ReadAddrReg(addr)

    val addrReg = ctx.cu.getOrUpdate(addr){ allocateLocal(addr, ctx.pipe) }
    addrReg match {
      // Replace temporary registers with address register
      case _:TempReg => ctx.replaceReg(addr, reg)
      // Forward specialized registers to needed register type
      case _:ReduceReg | _:AccumReg | _:InputReg | _:ScalarIn =>
        ctx.bypass(addrReg, reg)
        ctx.addReg(addr, reg)
      case _:WriteAddrReg if !write =>
        ctx.bypass(addrReg, reg)
        ctx.addReg(addr, reg)
      case _:ReadAddrReg if write =>
        ctx.bypass(addrReg, reg)
        ctx.addReg(addr, reg)

      // Keep counters and constants as is
      case _:WriteAddrReg if write => // Ok, do nothing
      case _:ReadAddrReg if !write => // Ok, do nothing
      case _:CounterReg | _:ConstReg => // Ok, do nothing
      case _ => stageError(s"Don't know how to handle register type $addrReg for address")
    }
  }

  // Addresses only, not values
  def writeAddrToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = rhs match {
    case Bram_store(bram, addr, value) =>
      allocateAddrReg(addr, ctx, true)
    case Par_bram_store(bram, addrs, values) =>
      allocateAddrReg(addrs, ctx, true)

    case _ => stageError(s"Unrecognized write address node $lhs = $rhs")
  }

  def mapNodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = rhs match {
    // --- Reads
    case Pop_fifo(fifo) =>
      ctx.addReg(lhs, InputReg(ctx.mem(fifo)))

    case Par_pop_fifo(fifo, len) =>
      ctx.addReg(lhs, InputReg(ctx.mem(fifo)))

    // Create a reference to this BRAM and
    case Bram_load(bram, addr) =>
      ctx.addReg(lhs, InputReg(ctx.mem(bram)))
      allocateAddrReg(addr, ctx, false)

    case Par_bram_load(bram, addrs) =>
      ctx.addReg(lhs, InputReg(ctx.mem(bram)))
      allocateAddrReg(addrs, ctx, false)

    case Vector_from_list(elems) =>
      if (elems.length != 1) stageError("Expected parallelization of 1 in inner loop in PIR generation")
      ctx.addReg(lhs, ctx.reg(elems.head))

    case Vec_apply(vec, idx) =>
      if (idx != 0) stageError("Expected parallelization of 1 in inner loop in PIR generation")
      ctx.addReg(lhs, ctx.reg(vec))

    // Register read is not a "true" node in PIR
    // True register reads happen at the beginning of ALU operations
    // So just make this node alias with the register
    case Reg_read(reg) =>
      val input = ctx.cu.getOrUpdate(reg){ allocateLocal(reg, ctx.pipe, read=Some(lhs)) }
      ctx.addReg(lhs, input)

    // --- Writes
    // No WriteAddrStage wrapper here, so these are for the values, not the addresses
    case Push_fifo(fifo,value,en) =>
    case Par_push_fifo(fifo,values,ens,_) =>
    case Bram_store(bram, addr, value) =>
    case Par_bram_store(bram, addrs, values) =>

    // Cases: 1. Inner Accumulator (read -> write)
    //        2. Outer Accumulator (read -> write)
    //        3. Local register but not accumulator (e.g. write -> read)
    //        4. Scalar output (read globally)
    // (1, 2, and 3 are mutually exclusive)
    // - 1: (Nothing, inner reductions are handled differently)
    // - 2: Update producer of value to have accumulator as output
    // - 3: Update reg to map to register of value (for later use in reads)
    // - 4: If any of first 3 options, add bypass value to scalar out, otherwise update producer
    case Reg_write(reg, value) =>
      if (isAccum(reg) && !isInnerAccum(reg)) { // Case 2
        val out = ctx.cu.getOrUpdate(reg){ allocateLocal(reg, ctx.pipe) }
        ctx.replaceReg(value, out)
      }
      if (isReadInPipe(reg, ctx.pipe) && !isAccum(reg)) { // Case 3
        ctx.addReg(reg, ctx.reg(value)) // Forward refs to reg to value
      }
      if (isReadOutsidePipe(reg, ctx.pipe)) { // Case 4
        val scalar = allocateGlobal(reg)
        val out = ScalarOut(reg, scalar)

        if (isReadInPipe(reg, ctx.pipe)) { // Cases 1, 2, or 3
          val local = ctx.reg(value)
          ctx.bypass(local, out)
        }
        else ctx.replaceReg(value, out)
      }

    case _ => nodeToOp(rhs) match {
      case Some(op) =>
        val inputs = syms(rhs).map{sym => ctx.refIn(ctx.reg(sym)) }
        val output = ctx.cu.getOrUpdate(lhs){ TempReg(lhs) }
        val stage = MapStage(op, inputs, ctx.refOut(output))
        ctx.addStage(stage)

      case None => stageWarn(s"No ALU scheduling rule known for $lhs = $rhs")
    }
  }

  // FIXME: Assumes a single node reduction function right now, change to multi-node later
  def reduceNodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = nodeToOp(rhs) match {
    case Some(op) =>
      // By convention, the inputs to the reduction tree is the first argument to the node
      // This input must be in the previous stage's reduction register
      // Ensure this either by adding a bypass register for raw inputs or changing the output
      // of the previous stage from a temporary register to the reduction register
      // FIXME: Rather hacky. Should guarantee ordering to avoid stage error with map stage
      val input = syms(rhs).head
      val inputReg = ctx.reg(input)
      // HACK: Get initial value of accumulator
      val zero = syms(rhs).last match {
        case Deff(Reg_read(acc)) => allocateConst(resetValue(acc))
        case _ => ConstReg("0l")
      }
      inputReg match {
        case _:ReduceReg | _: AccumReg =>
          if (ctx.currentStage.outputReg != inputReg) {
            val out = ReduceReg(fresh[Any]) // HACK: already have accumulator with name lhs
            ctx.bypass(inputReg, out)
            ctx.addReg(input, out)
          }
        case _:InputReg | _:CounterReg | _:ConstReg | _:ReadAddrReg | _:WriteAddrReg | _:ScalarIn =>
          val out = ReduceReg(input)
          ctx.bypass(inputReg, out)
          ctx.addReg(input, out)
        case t: TempReg =>
          if (ctx.currentStage.outputReg == t) {
            val out = ReduceReg(input)
            ctx.replaceReg(input, out)
          }
          else {
            val out = ReduceReg(input)
            ctx.bypass(inputReg, out)
            ctx.addReg(input, out)
          }
        case _:VectorOut | _:ScalarOut | _:VectorLocal => // Never happens (FIXME: should have way to type this better)
      }
      val acc = ReduceReg(lhs)
      val stage = ReduceStage(op, zero, acc)
      ctx.addReg(lhs, acc)
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
