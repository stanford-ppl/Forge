package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.HashMap

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import java.io.PrintWriter
import ppl.delite.framework.Config

trait PIRGen extends Traversal with PIRCommon {
  val IR: DHDLExp with PIRScheduleAnalysisExp
  import IR._

  override val name = "PIR Generation"
  override val recurse = Always
  debugMode = true

  val dir = sys.env("PIR_HOME") + "/apps/"
  val app = Config.degFilename.take(Config.degFilename.length - 4)
  val filename = app + ".scala"

  lazy val prescheduler = new PIRScheduleAnalyzer{val IR: PIRGen.this.IR.type = PIRGen.this.IR}
  lazy val scheduler = new PIRScheduler{val IR: PIRGen.this.IR.type = PIRGen.this.IR}
  lazy val top       = prescheduler.top
  lazy val cus       = prescheduler.cuMapping

  var stream: PrintWriter = null
  var indent = 0
  def emit(x: => Any) { stream.println("  "*indent + x) }
  def open(x: => Any) { emit(x); indent += 1 }
  def close(x: => Any) { indent -= 1; emit(x) }

  override def quote(x: Exp[Any]) = x match {
    case Const(c: Int) => "Const(" + c + "l)"
    case Const(c: Long) => "Const(" + c + "l)"
    case Const(c: Double) => "Const(" + c + ".toLong)"  // TODO
    case Const(c: Float) => "Const(" + c + ".toLong)"   // TODO
    case Exact(c) => "Const(" + c.toLong + "l)" // TODO
    case Param(p) => "Const(" + p + ".toLong)"  // TODO
    case _ => super.quote(x)
  }

  override def run[A:Manifest](b: Block[A]) = {
    if (SpatialConfig.genCGRA) {
      stream = new PrintWriter(dir + filename)
      try {
        emitPIR(b)
      }
      catch { case e: Throwable =>
        stageWarn("Exception during PIR generation: " + e)
        if (debugMode) e.printStackTrace;
      }
      stream.flush()
      stream.close()
    }
    (b)
  }

  def emitPIR(b: Block[Any]) {
    prescheduler.run(b)
    subst ++= prescheduler.subst.toList
    scheduler.subst ++= subst.toList
    scheduler.cus ++= cus.toList
    scheduler.run(b)
    globals ++= (prescheduler.globals ++ scheduler.globals)

    debug("Scheduling complete. Generating...")
    generateHeader()
    generateGlobals()
    traverseBlock(b)
    generateFooter()
  }

  def generateHeader() {
    emit("import pir.graph._")
    emit("import pir.graph")
    emit("import pir.codegen._")
    emit("import pir.plasticine.config._")
    emit("import pir.Design")
    emit("import pir.PIRMisc._")
    emit("")
    open(s"""object ${app}Design extends Design {""")
    emit(s"""override val arch = Config0""")
    emit(s"""top = Top()""")
  }

  def generateGlobals() {
    val (mems, memCtrls) = globals.partition{case MemCtrl(_,_,_) => false; case _ => true}
    mems.foreach(emitComponent(_))
    memCtrls.foreach(emitComponent(_))
  }

  def generateFooter() {
    val args = globals.flatMap{case InputArg(name)=>Some(name); case OutputArg(name)=>Some(name); case _ => None}.mkString(", ")
    val mcs  = globals.flatMap{case MemCtrl(name,_,_)=>Some(name); case _ => None}.mkString(", ")
    emit(s"top.updateFields(List(${cus(top.get).name}), List($args), List($mcs))")
    emit(s"")
    emit("def main(args: Array[String]): Unit = run")
    close("}")
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && cus.contains(lhs))
      generateCU(lhs, cus(lhs))
  }

  def cuDeclaration(cu: ComputeUnit) = {
    val parent = cu.parent.map(_.name).getOrElse("top")
    val deps = cu.deps.map{dep => cus(dep).name }
    cu match {
      case cu: BasicComputeUnit =>
        s"""ComputeUnit(name=Some("${cu.name}"), tpe = ${quoteControl(cu.tpe)}, deps=$deps parent=$parent)"""
      case cu: TileTransferUnit =>
        s"""TileTransfer(name=Some("${cu.name}"), memctrl=${cu.ctrl.name}, mctpe=${cu.mode}, deps=$deps, parent=$parent)"""
    }
  }
  def quoteControl(tpe: ControlType) = tpe match {
    case InnerPipe => "Pipe"
    case CoarsePipe => "MetaPipeline"
    case SequentialPipe => "Sequential"
    case StreamPipe => throw new Exception("Stream pipe not yet supported in PIR")
  }

  def generateCU(pipe: Exp[Any], cu: ComputeUnit, suffix: String = "") {
    debug(s"Generating CU for $pipe")
    debug(cu.dumpString)

    open(s"val ${cu.name} = ${cuDeclaration(cu)} { implicit CU => ")
    preallocateRegisters(cu)                // Includes scalar inputs/outputs, read/write addresses
    cu.cchains.foreach(emitComponent(_))    // Allocate all counterchains
    cu.srams.foreach(emitComponent(_))      // Allocate all SRAMs

    emitAllStages(cu)

    close("}")
  }

  def preallocateRegisters(cu: ComputeUnit) = cu.regs.foreach{
    case ReadAddrReg(x) => emit(s"val ${quote(x)} = CU.rdAddr()")
    case WriteAddrReg(x) => emit(s"val ${quote(x)} = CU.wtAddr()")
    case TempReg(x) => emit(s"val ${quote(x)} = CU.temp()")
    case AccumReg(x,init) => emit(s"val ${quote(x)} = CU.accum(init = Some(${quote(init)}))")
    case ScalarIn(x,mem) => emit(s"val ${quote(x)} = ScalarIn(${mem.name})")
    case ScalarOut(x,mem) => emit(s"val ${quote(x)} = ScalarOut(${mem.name})")
    case _ => // No preallocation
  }

  def emitComponent(x: Any) = x match {
    case CounterChainCopy(name, owner) =>
      emit(s"""val $name = CounterChain.copy(${owner.name}, "$name")""")

    case CounterChainInstance(name, ctrs) =>
      //for (ctr <- ctrs) emitComponent(ctr)
      val bnds = ctrs.map{case CUCounter(name,start,end,stride) => s"(${quote(start)}, ${quote(end)}, ${quote(stride)})"}.mkString(", ")
      emit(s"""val $name = CounterChain(name = "$name", $bnds)""")

    case CUCounter(name,start,end,stride) =>
      // TODO: Currently unused
      emit(s"""val $name = Counter(${quote(start)}, ${quote(end)}, ${quote(stride)})""")

    case CUMemory(name, size, Some(writer), Some(readAddr), Some(writeAddr)) =>
      emit(s"""val $name = SRAM(size = $size, vector = ${writer.name}, readAddr = ${quote(readAddr)}, writeAddr = ${quote(writeAddr)})""")

    case MemCtrl(name,region,mode) => emit(s"val $name = MemoryController($mode, ${region.name})")
    case Offchip(name) => emit(s"val $name = Offchip()")
    case InputArg(name) => emit(s"val $name = ArgIn()")
    case OutputArg(name) => emit(s"val $name = ArgOut()")
    case ScalarMem(name) => emit(s"val $name = Scalar()")
    case VectorMem(name) => emit(s"val $name = Vector()")
    case _ => throw new Exception(s"Don't know how to generate CGRA component: $x")
  }

  def quote(reg: LocalMem): String = reg match {
    // Always directly quotable
    case ConstReg(c) => s"Const($c)"                        // Constant

    // Context dependent (sometimes quotable)
    case CounterReg(cchain, idx) => s"${cchain.name}($idx)" // Counter
    case ScalarIn(x, mem)        => quote(x)                // Inputs to counterchains
    case ReduceReg(x)            => quote(x)                // Uses only, not assignments
    case AccumReg(x, init)       => quote(x)                // After preallocation
    case InputReg(mem)           => s"${mem.name}.load"     // Local read
    case WriteAddrReg(x)         => quote(x)                // Write address register
    case ReadAddrReg(x)          => quote(x)                // Read address register

    // Other cases require stage context
    case _ => throw new Exception(s"Cannot quote local memory $reg without context")
  }

  var allocatedReduce: Set[ReduceReg] = Set.empty

  def quote(ref: LocalRef): String = ref match {
    // Stage invariant register types
    case LocalRef(stage, reg: ConstReg) => quote(reg)

    // Delayed or forwarded registers
    case LocalRef(stage, reg: CounterReg) => if (stage > 0) s"CU.ctr(stage($stage), ${quote(reg)})" else quote(reg)
    case LocalRef(stage, reg: InputReg)   => if (stage > 0) s"CU.load(stage($stage), ${reg.mem.name})" else quote(reg)

    // Delayed registers
    case LocalRef(stage, reg: VectorLocal) => s"CU.store(stage($stage), ${reg.mem.name})"
    case LocalRef(stage, reg: VectorOut)   => s"CU.vecOut(stage($stage), ${reg.mem.name})"

    case LocalRef(stage, reg: ScalarIn)  => s"CU.scalarIn(stage($stage), ${quote(reg.x)})"
    case LocalRef(stage, reg: ScalarOut) => s"CU.scalarOut(stage($stage), ${quote(reg.x)})"
    case LocalRef(stage, reg: TempReg)   => s"CU.temp(stage($stage), ${quote(reg.x)})"
    case LocalRef(stage, reg: AccumReg) => s"CU.accum(stage($stage), ${quote(reg.x)})"
    case LocalRef(stage, reg: WriteAddrReg) => s"CU.wtAddr(stage($stage), ${quote(reg.x)})"
    case LocalRef(stage, reg: ReadAddrReg)  => s"CU.rdAddr(stage($stage), ${quote(reg.x)})"

    // Weird cases
    case LocalRef(stage, reg: ReduceReg) if allocatedReduce.contains(reg) => quote(reg)
    case LocalRef(stage, reg: ReduceReg) => s"CU.reduce(stage($stage))"
  }

  def emitAllStages(cu: ComputeUnit) {
    var i = 0
    var r = 0
    def emitStages(stages: List[Stage]) = stages.foreach{
      case MapStage(op,inputs,out) =>
        val ins = inputs.map(quote(_)).mkString(", ")
        emit(s"""Stage(stage($i), opds=List($ins), o=$op, r=${quote(out)})""")
        i += 1

      case ReduceStage(op,init,acc) =>
        emit(s"""val (rs$r, ${quote(acc)}) = Stage.reduce(op=$op, init=${quote(init)})""")
        allocatedReduce += acc
        r += 1
    }

    if (cu.stages.nonEmpty || cu.writeStages.nonEmpty) {
      emit(s"var stage: List[Stage] = Nil")

      for ((mem,stages) <- cu.writeStages) {
        i = 0
        val nWrites  = stages.filter{_.isInstanceOf[Stage]}.length
        emit(s"stage = WAStages(${mem.name}, ${nWrites})")
        emitStages(stages)
      }

      i = 0
      val nCompute = cu.stages.filter{_.isInstanceOf[Stage]}.length
      emit(s"stage = Stages(${nCompute})")
      emitStages(cu.stages)
    }
  }
}
