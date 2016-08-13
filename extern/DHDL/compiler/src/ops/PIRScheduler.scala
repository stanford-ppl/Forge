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
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val cus = HashMap[Exp[Any], ComputeUnit]()

  def allocateCU(pipe: Exp[Any]): ComputeUnit = cus(pipe)

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && cus.contains(lhs))
      scheduleCU(lhs, cus(lhs))
  }

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

    def mem(mem: Exp[Any], reader: Exp[Any]) = allocateMem(mem, reader, cu)

    // A CU can have multiple SRAMs for a given mem symbol, one for each local read
    def memories(mem: Exp[Any]) = readersOf(mem).filter(_._1 == pipe).map{read => allocateMem(mem, read._3, cu) }

    def addReg(x: Exp[Any], reg: LocalMem) { cu.addReg(x, reg) }
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
          stage.outs ::= refOut(out, mapStages.length - mapStages.indexOf(stage)) // stage idx + 1
        case None =>
          bypass(prev, out)
      }
      if (add) addReg(e, out)
      else cu.regs += out // No mapping, only list
    }

    def finalizeContext() { }
  }
  case class ComputeContext(override val pipe: Exp[Any], override val cu: ComputeUnit) extends CUContext(pipe,cu) {
    def pseudoStages = cu.computePseudoStages
    def stages = cu.stages
    def addStage(stage: Stage) { cu.stages ::= stage }
    override def finalizeContext() { cu.stages = cu.stages.reverse }
    def isWriteContext = false
  }
  case class WriteContext(override val pipe: Exp[Any], override val cu: ComputeUnit, srams: List[CUMemory]) extends CUContext(pipe,cu) {
    cu.writeStages += srams -> Nil

    def pseudoStages = cu.writePseudoStages(srams)
    def stages = cu.writeStages(srams)
    def addStage(stage: Stage) { cu.writeStages(srams) = cu.writeStages(srams) :+ stage }
    def isWriteContext = true
  }

  def scheduleCU(pipe: Exp[Any], cu: ComputeUnit) {
    debug(s"Scheduling $pipe CU: $cu")
    val origRegs = cu.regs
    var writeStageRegs = cu.regs

    // --- Schedule write contexts
    for (srams <- cu.writePseudoStages.keys) {
      val writes = WriteContext(pipe, cu, srams)
      scheduleContext(writes)
      writeStageRegs ++= cu.regs // Reset view of registers each time
      cu.regs = origRegs
    }
    // --- Schedule compute context
    val compute = ComputeContext(pipe, cu)
    scheduleContext(compute)
    cu.regs ++= writeStageRegs

    for (srams <- cu.writePseudoStages.keys) {
      debug(s"Generated write stages ($srams): ")
      cu.writeStages(srams).foreach(stage => debug(s"  $stage"))
    }
    debug("Generated compute stages: ")
    cu.stages.foreach(stage => debug(s"  $stage"))
  }
  def scheduleContext(ctx: CUContext) {
    debug(s"  Scheduling context $ctx")
    ctx.pseudoStages.foreach {stage => scheduleStage(stage, ctx) }
    ctx.finalizeContext()
  }

  // Given result register type A, reroute to type B as necessary
  def propagateReg(exp: Exp[Any], a: LocalMem, b: LocalMem, ctx: CUContext) = (a,b) match {
    case (a:ScalarOut, b:ScalarOut) => a
    case (a:VectorOut, b:VectorOut) => a
    case (a:VectorLocal, b:VectorLocal) => a
    case (_:ReduceReg | _:AccumReg, _:ReduceReg | _:AccumReg) => a

    // Propagating from read addr wire to another read addr wire is ok (but should usually never happen)
    case (a:ReadAddrWire, b:ReadAddrWire) => ctx.addOutput(exp,a,b); b
    case (a,b) if !isReadable(a) => throw new Exception(s"Cannot propagate for $exp from output-only $a")
    case (a,b) if !isWritable(b) => throw new Exception(s"Cannot propagate for $exp to input-only $b")

    // TODO: Are these necessary? Do we ever see multiple writes to the same output?
    case (a, ScalarOut(x,_)) => ctx.addOutput(x,a,b); b
    case (a, VectorOut(x,_)) => ctx.addOutput(x,a,b); b
    case (a, VectorLocal(x,_)) => ctx.addOutput(x,a,b); b

    case (a, b:TempReg) => a

    // Special cases: don't propagate to write/read wires from counters or constants
    case (_:CounterReg | _:ConstReg, _:WriteAddrWire | _:ReadAddrWire) => a
    // General case for outputs: Don't add mapping for exp to output
    case (a,b) if !isReadable(b) => ctx.addOutput(exp,a,b,add=false); b

    case (a,b) => ctx.addOutput(exp,a,b); b
  }

  // If addr is a counter or const, just returns that register back. Otherwise returns address wire
  def allocateAddrReg(sram: CUMemory, addr: Exp[Any], ctx: CUContext, write: Boolean, local: Boolean = false) = {
    val wire = if (write && local) LocalWriteReg(sram)
               else if (write)     WriteAddrWire(sram)
               else                ReadAddrWire(sram)
    val addrReg = ctx.reg(addr)
    propagateReg(addr, addrReg, wire, ctx)
  }


  def scheduleStage(stage: PseudoStage, ctx: CUContext) = stage match {
    case DefStage(lhs@Deff(rhs), isReduce) =>
      debug(s"""    $lhs = $rhs ${if (isReduce) "[REDUCE]" else ""}""")
      if (isReduce) reduceNodeToStage(lhs,rhs,ctx)
      else          mapNodeToStage(lhs,rhs,ctx)

    case WriteAddrStage(lhs@Deff(rhs)) =>
      debug(s"""    $lhs = $rhs [WRITE]""")
      writeAddrToStage(lhs, rhs, ctx)

    case OpStage(op, ins, out, isReduce) =>
      debug(s"""    $out = $op(${ins.mkString(",")}) [OP]""")
      opStageToStage(op, ins, out, ctx, isReduce)
  }


  // Addresses only, not values
  def writeAddrToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = rhs match {
    case Bram_store(bram, addr, value) =>
      ctx.memories(bram).foreach{sram =>
        sram.writeAddr = Some(allocateAddrReg(sram, addr, ctx, write=true))
      }

    case Par_bram_store(bram, addrs, values) =>
      ctx.memories(bram).foreach{sram =>
        sram.writeAddr = Some(allocateAddrReg(sram, addrs, ctx, write=true))
      }

    case _ => stageError(s"Unrecognized write address node $lhs = $rhs")
  }

  def bufferWrite(mem: Exp[Any], value: Exp[Any], addr: Option[Exp[Any]], ctx: CUContext) {
    if (isReadInPipe(mem, ctx.pipe)) {
      // TODO: Should we allow multiple versions of local accumulator?
      ctx.memories(mem).foreach{sram =>
        propagateReg(value, ctx.reg(value), VectorLocal(fresh[Any], sram), ctx)

        if (addr.isDefined)
          sram.writeAddr = Some(allocateAddrReg(sram, addr.get, ctx, write=true, local=true))
      }
    }
    if (isReadOutsidePipe(mem, ctx.pipe)) { // Should always be true?
      val vector = allocateGlobal(mem)
      propagateReg(value, ctx.reg(value), VectorOut(fresh[Any], vector), ctx)
    }
  }

  def mapNodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = rhs match {
    // --- Reads
    case Pop_fifo(EatAlias(fifo)) =>
      val vector = allocateGlobal(fifo).asInstanceOf[VectorMem]
      ctx.addReg(lhs, VectorIn(vector))

    case Par_pop_fifo(EatAlias(fifo), len) =>
      val vector = allocateGlobal(fifo).asInstanceOf[VectorMem]
      ctx.addReg(lhs, VectorIn(vector))

    // Create a reference to this BRAM and
    case Bram_load(EatAlias(bram), addr) =>
      val sram = ctx.mem(bram,lhs)
      ctx.addReg(lhs, InputReg(sram))
      sram.readAddr = Some(allocateAddrReg(sram, addr, ctx, write=false, local=true))

    case Par_bram_load(EatAlias(bram), addrs) =>
      val sram = ctx.mem(bram,lhs)
      ctx.addReg(lhs, InputReg(sram))
      sram.readAddr = Some(allocateAddrReg(sram, addrs, ctx, write=false, local=true))

    case Vector_from_list(elems) =>
      if (elems.length != 1) stageError("Expected parallelization of 1 in inner loop in PIR generation")
      ctx.addReg(lhs, ctx.reg(elems.head))

    case Vec_apply(vec, idx) =>
      if (idx != 0) stageError("Expected parallelization of 1 in inner loop in PIR generation")
      ctx.addReg(lhs, ctx.reg(vec))

    // Register read is not a "true" node in PIR
    // True register reads happen at the beginning of ALU operations
    // So just make this node alias with the register
    case Reg_read(EatAlias(reg)) =>
      val input = ctx.cu.getOrAddReg(reg){ allocateLocal(reg, ctx.pipe, read=Some(lhs)) }
      ctx.addReg(lhs, input)

    // --- Writes
    // Only for the values, not the addresses UNLESS these are local accumulators
    // TODO: Filters
    case Push_fifo(EatAlias(fifo),value,en) =>
      bufferWrite(fifo,value,None,ctx)
    case Par_push_fifo(EatAlias(fifo),values,ens,_) =>
      bufferWrite(fifo,values,None,ctx)

    case Bram_store(EatAlias(bram), addr, value) =>
      bufferWrite(bram,value,Some(addr),ctx)
    case Par_bram_store(EatAlias(bram), addrs, values) =>
      bufferWrite(bram,values,Some(addrs),ctx)

    // Cases: 1. Inner Accumulator (read -> write)
    //        2. Outer Accumulator (read -> write)
    //        3. Local register but not accumulator (e.g. write -> read)
    //        4. Scalar output (read globally)
    // (1, 2, and 3 are mutually exclusive)
    // - 1: (Nothing, inner reductions are handled differently)
    // - 2: Update producer of value to have accumulator as output
    // - 3: Update reg to map to register of value (for later use in reads)
    // - 4: If any of first 3 options, add bypass value to scalar out, otherwise update producer
    case Reg_write(EatAlias(reg), value) =>
      val isLocallyRead = isReadInPipe(reg, ctx.pipe)
      val isLocallyWritten = isWrittenInPipe(reg, ctx.pipe, Some(lhs)) // Always true?
      val isInnerAcc = isInnerAccum(reg) && isLocallyRead && isLocallyWritten
      val isOuterAcc = isAccum(reg) && !isInnerAcc && isLocallyRead && isLocallyWritten
      val isRemotelyRead = isReadOutsidePipe(reg, ctx.pipe)

      if (isOuterAcc) { // Case 2
        val out = ctx.cu.getOrAddReg(reg){ allocateLocal(reg, ctx.pipe) }
        propagateReg(value, ctx.reg(value), out, ctx)
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
      case Def(ConstBit(_)) => ctx.cu.getOrAddReg(lhs){ allocateLocal(lhs, ctx.pipe) }

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
    case FixPt_Lt(_,_)  => Some(FixLt)
    case FixPt_Leq(_,_) => Some(FixLeq)
    case FixPt_Eql(_,_) => Some(FixEql)
    case FixPt_Neq(_,_) => Some(FixNeq)

    // Float ops currently assumed to be single op
    case FltPt_Add(_,_) => Some(FltAdd)
    case FltPt_Sub(_,_) => Some(FltSub)
    case FltPt_Mul(_,_) => Some(FltMul)
    case FltPt_Div(_,_) => Some(FltDiv)
    case FltPt_Lt(_,_)  => Some(FltLt)
    case FltPt_Leq(_,_) => Some(FltLeq)
    case FltPt_Eql(_,_) => Some(FltEql)
    case FltPt_Neq(_,_) => Some(FltNeq)

    case Bit_And(_,_)   => Some(BitAnd)
    case Bit_Or(_,_)    => Some(BitOr)
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
