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
    private val readAccums = HashSet[AccumReg]()

    // HACK: Keep track of first read of accum reg (otherwise can use the wrong stage)
    def isUnreadAccum(reg: LocalMem) = reg match {
      case reg: AccumReg => !readAccums.contains(reg)
      case _ => false
    }

    def pseudoStages: List[PseudoStage]
    def stages: List[Stage]
    def addStage(stage: Stage): Unit
    def isWriteContext: Boolean

    def mapStages = stages.flatMap{case stage:MapStage => Some(stage); case _ => None}
    def stageNum = mapStages.length+1
    def prevStage = stages.headOption

    def mem(x: Exp[Any]) = cu.mem(x)
    def addReg(x: Exp[Any], reg: LocalMem) { cu.addReg(x, reg) }
    //def replaceReg(x: Exp[Any], reg: LocalMem) { cu.replaceReg(x, reg) }
    def addRef(x: Exp[Any], ref: LocalRef) { refs += x -> ref }
    def getReg(x: Exp[Any]) = cu.get(x)
    def reg(x: Exp[Any]) = cu.get(x).getOrElse(throw new Exception(s"No register defined for $x"))

    // Add a stage which bypasses x to y
    def bypass(x: LocalMem, y: LocalMem) {
      val stage = MapStage(Bypass, List(refIn(x)), List(refOut(y)))
      addStage(stage)
    }

    def ref(reg: LocalMem, out: Boolean, stage: Int = stageNum): LocalRef = reg match {
      // If the previous stage computed the read address for this load, use the registered output
      // of the memory directly. Otherwise, use the previous stage
      case InputReg(mem) =>
        if (!prevStage.isDefined || prevStage.get.outputMems.contains(mem.readAddr))
          LocalRef(-1, reg)
        else
          LocalRef(stage-1, reg)

      case reg: CounterReg if isWriteContext && !prevStage.isDefined =>
        LocalRef(-1, reg)

      case reg: AccumReg if isUnreadAccum(reg) =>
        readAccums += reg
        LocalRef(stage, reg)
      case _ if out => LocalRef(stage, reg)
      case _        => LocalRef(stage-1, reg)
    }
    def refIn(reg: LocalMem, stage: Int = stageNum) = ref(reg, false, stage)
    def refOut(reg: LocalMem, stage: Int = stageNum) = ref(reg, true, stage)

    def addOutput(e: Exp[Any], prev: LocalMem, out: LocalMem, add: Boolean = true) {
      mapStages.find{stage => stage.outputMems.contains(prev) } match {
        case Some(stage) =>
          stage.outs ::= refOut(out, mapStages.indexOf(stage)+1)
        case None =>
          bypass(prev, out)
      }
      if (add) addReg(e, out)
    }

    def finalizeCU() = {
      // Remove all temporary registers from outputs when they are not used in any input
      val stages = mapStages
      val tempIns = stages.flatMap{stage => stage.inputMems.flatMap{case t: TempReg => Some(t); case _ => None}}
      val tempOuts = stages.flatMap{stage => stage.outputMems.flatMap{case t: TempReg => Some(t); case _ => None}}
      val unusedTemps = tempOuts.filterNot(tempIns contains _)
      stages.foreach{stage => stage.outs = stage.outs.filterNot{ref => unusedTemps contains ref.reg}}
      // Also remove from set of registers
      cu.regs --= unusedTemps
    }
  }
  case class ComputeContext(override val pipe: Exp[Any], override val cu: ComputeUnit) extends CUContext(pipe,cu) {
    def pseudoStages = cu.computePseudoStages
    def stages = cu.stages
    def addStage(stage: Stage) { cu.stages ::= stage }
    override def finalizeCU() {
      super.finalizeCU()
      cu.stages = cu.stages.reverse
    }
    def isWriteContext = false
  }
  case class WriteContext(override val pipe: Exp[Any], override val cu: ComputeUnit, mem: CUMemory) extends CUContext(pipe,cu) {
    cu.writeStages += mem -> Nil

    def pseudoStages = cu.writePseudoStages(mem)
    def stages = cu.writeStages(mem)
    def addStage(stage: Stage) { cu.writeStages(mem) = cu.writeStages(mem) :+ stage }
    def isWriteContext = true
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
      debug(s"Generated write stages ($mem): ")
      cu.writeStages(mem).foreach(stage => debug(s"  $stage"))
    }
    debug("Generated compute stages: ")
    cu.stages.foreach(stage => debug(s"  $stage"))
  }
  def scheduleContext(ctx: CUContext) {
    ctx.pseudoStages.foreach {
      case DefStage(lhs@Deff(rhs), isReduce) => nodeToStage(lhs, rhs, ctx, isReduce)
      case WriteAddrStage(lhs@Deff(rhs))     => writeAddrToStage(lhs, rhs, ctx)
      case OpStage(op, ins, out, isReduce)   => opStageToStage(op, ins, out, ctx, isReduce)
    }
    ctx.finalizeCU
  }

  // Given result register type A, reroute to type B as necessary
  def propagateReg(exp: Exp[Any], a: LocalMem, b: LocalMem, ctx: CUContext) = (a,b) match {
    // TODO: Check if used locally if is temp register?
    case (a:ScalarOut, b:ScalarOut) => a
    case (a:VectorOut, b:VectorOut) => a
    case (a:VectorLocal, b:VectorLocal) => a
    case (a:ScalarOut, b) => throw new Exception("Cannot propagate from output register")
    case (a:VectorOut, b) => throw new Exception("Cannot propagate from output register")
    case (a:VectorLocal, b) => throw new Exception("Cannot propagate from output register")

    case (a, ScalarOut(x,_)) => ctx.addOutput(x,a,b); b
    case (a, VectorOut(x,_)) => ctx.addOutput(x,a,b); b
    case (a, VectorLocal(x,_)) => ctx.addOutput(x,a,b); b

    case (a, b:TempReg) => a

    case (_:CounterReg | _:ConstReg, _:WriteAddrWire | _:ReadAddrWire) => a // Ignore prop from counters/consts to write/read addrs
    case (_:ReduceReg | _:AccumReg, _:ReduceReg | _:AccumReg) => a        // No need for prop from reduce to reduce regs

    case (a,b) => ctx.addOutput(exp,a,b); b
  }

  def nodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext, isReduce: Boolean) {
    debug(s"  $lhs = $rhs")
    if (isReduce) reduceNodeToStage(lhs,rhs,ctx)
    else          mapNodeToStage(lhs,rhs,ctx)
  }

  def allocateAddrReg(mem: CUMemory, addr: Exp[Any], ctx: CUContext, write: Boolean) = {
    val reg = if (write) WriteAddrWire(mem) else ReadAddrWire(mem)
    val addrReg = ctx.reg(addr)
    // If addr is a counter or const, just returns that register back. Otherwise returns reg
    propagateReg(addr, addrReg, reg, ctx)
  }

  // Addresses only, not values
  def writeAddrToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = rhs match {
    case Bram_store(bram, addr, value) =>
      val sram = ctx.mem(bram)
      sram.writeAddr = allocateAddrReg(sram, addr, ctx, true)
    case Par_bram_store(bram, addrs, values) =>
      val sram = ctx.mem(bram)
      sram.writeAddr = allocateAddrReg(sram, addrs, ctx, true)

    case _ => stageError(s"Unrecognized write address node $lhs = $rhs")
  }

  // TODO: What is syntax for accumulator?
  def bufferWrite(mem: Exp[Any], value: Exp[Any], ctx: CUContext) {
    if (isAccum(mem)) {
      val local = ctx.mem(mem)
      propagateReg(value, ctx.reg(value), VectorLocal(fresh[Any], local), ctx)
    }
    if (isReadOutsidePipe(mem, ctx.pipe)) { // Should always be true?
      val vector = allocateGlobal(mem)
      propagateReg(value, ctx.reg(value), VectorOut(fresh[Any], vector), ctx)
    }
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
      val sram = ctx.mem(bram)
      sram.readAddr = allocateAddrReg(sram, addr, ctx, false)

    case Par_bram_load(bram, addrs) =>
      ctx.addReg(lhs, InputReg(ctx.mem(bram)))
      val sram = ctx.mem(bram)
      sram.readAddr = allocateAddrReg(sram, addrs, ctx, false)

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
      val input = ctx.cu.getOrAddReg(reg){ allocateLocal(reg, ctx.pipe, read=Some(lhs)) }
      ctx.addReg(lhs, input)

    // --- Writes
    // No WriteAddrStage wrapper here, so these are for the values, not the addresses
    // TODO: Filters
    case Push_fifo(fifo,value,en)            => bufferWrite(fifo,value,ctx)
    case Par_push_fifo(fifo,values,ens,_)    => bufferWrite(fifo,values,ctx)
    case Bram_store(bram, addr, value)       => bufferWrite(bram,value,ctx)
    case Par_bram_store(bram, addrs, values) => bufferWrite(bram,values,ctx)

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
      val isLocallyRead = isReadInPipe(reg, ctx.pipe)
      val isLocallyWritten = isWrittenInPipe(reg, ctx.pipe, Some(lhs)) // Always true?
      val isInnerAcc = isInnerAccum(reg) && isLocallyRead && isLocallyWritten
      val isOuterAcc = isAccum(reg) && !isInnerAcc && isLocallyRead && isLocallyWritten
      val isRemotelyRead = isReadOutsidePipe(reg, ctx.pipe)

      if (isOuterAcc) { // Case 2
        val out = ctx.cu.getOrAddReg(reg){ allocateLocal(reg, ctx.pipe) }
        propagateReg(value, ctx.reg(value), out, ctx)
        //ctx.replaceReg(value, out)
      }
      else if (!isInnerAcc) { // Case 3
        ctx.addReg(reg, ctx.reg(value)) // Forward refs to reg to value
      }
      if (isRemotelyRead) { // Case 4
        val scalar = allocateGlobal(reg)
        val out = ScalarOut(fresh[Any], scalar)
        if (isInnerAcc)
          propagateReg(reg, ctx.reg(value), out, ctx) // Bypass
        else
          propagateReg(reg, ctx.reg(reg), out, ctx)
      }

    case _ => lhs match {
      case Fixed(_) => ctx.cu.getOrAddReg(lhs){ allocateLocal(lhs, ctx.pipe) }

      case _ => nodeToOp(rhs) match {
        case Some(op) => opStageToStage(op, syms(rhs), lhs, ctx, false)
        case None => stageWarn(s"No ALU operation known for $lhs = $rhs")
      }
    }
  }

  def reduceNodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = nodeToOp(rhs) match {
    case Some(op) => opStageToStage(op, syms(rhs), lhs, ctx, true)
    case _ => stageWarn(s"No ALU operation known for $lhs = $rhs")
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

  // FIXME: Assumes a single node reduction function right now, change to allow multi-node later
  def opStageToStage(op: PIROp, ins: List[Exp[Any]], out: Exp[Any], ctx: CUContext, isReduce: Boolean) = {
    if (isReduce) {
      // By convention, the inputs to the reduction tree is the first argument to the node
      // This input must be in the previous stage's reduction register
      // Ensure this either by adding a bypass register for raw inputs or changing the output
      // of the previous stage from a temporary register to the reduction register
      val input = ins.head
      val accum = ins.last
      val inputReg = ctx.reg(input)
      propagateReg(input, inputReg, ReduceReg(fresh[Any]), ctx)
      val zero = accum match {
        case Deff(Reg_read(acc)) => allocateConst(resetValue(acc))
        case _ => ConstReg("0l")
      }
      val acc = ReduceReg(out)
      val stage = ReduceStage(op, zero, acc)
      ctx.addReg(out, acc)
      ctx.addStage(stage)
    }
    else {
      val inputs = ins.map{in => ctx.refIn(ctx.reg(in)) }
      val output = ctx.cu.getOrAddReg(out){ TempReg(out) }
      val stage = MapStage(op, inputs, List(ctx.refOut(output)))
      ctx.addStage(stage)
    }
  }

}
