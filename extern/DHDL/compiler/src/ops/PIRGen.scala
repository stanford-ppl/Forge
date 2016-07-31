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

trait PIRGen extends Traversal with SubstQuotingExp {
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
  lazy val collector = new SymbolCollector{val IR: PIRGen.this.IR.type = PIRGen.this.IR}
  lazy val constants = collector.constants
  lazy val globals   = prescheduler.globals
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
    collector.run(b)
    prescheduler.run(b)
    subst ++= prescheduler.subst.toList
    scheduler.subst ++= subst.toList
    scheduler.cus ++= cus.toList
    scheduler.run(b)

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
    constants.foreach{
      case Const(c) =>
      case c@Exact(d) => emit(s"val ${quote(c)} = Const(${d.toLong}l)") // TODO
    }
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

  // HACK: Ignore parallels
  def childrenOfHack(e: Exp[Any]): List[Exp[Any]] = childrenOf(e).flatMap{
    case child@Deff(Pipe_parallel(_)) => childrenOfHack(child)
    case child => List(child)
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && cus.contains(lhs))
      generateCU(lhs, cus(lhs))
  }

  def cuDeclaration(cu: ComputeUnit) = cu match {
    case cu: BasicComputeUnit =>
      s"""ComputeUnit(name=Some("${cu.name}"), tpe = ${quoteControl(cu.tpe)})"""
    case cu: TileTransferUnit =>
      s"""TileTransfer(name=Some("${cu.name}"), memctrl=${cu.ctrl.name}, mctpe=${cu.mode})"""
  }
  def quoteControl(tpe: ControlType) = tpe match {
    case InnerPipe => "Pipe"
    case CoarsePipe => "MetaPipeline"
    case SequentialPipe => "Sequential"
    case StreamPipe => stageError("Stream pipe not yet supported in PIR")
  }

  def generateCU(pipe: Exp[Any], cu: ComputeUnit, suffix: String = "") {
    debug(s"Generating CU for $pipe")
    debug(cu.dumpString)

    val parent = cu.parent.map(_.name).getOrElse("top")

    open(s"val ${cu.name} = {")
    emit(s"implicit val CU = ${cuDeclaration(cu)}")
    emit(s"CU.updateParent($parent)")
    // Inputs and outputs
    cu.scalarIn.foreach(emitComponent(_))
    cu.scalarOut.foreach(emitComponent(_))
    cu.vectorIn.foreach(emitComponent(_))
    cu.vectorOut.foreach(emitComponent(_))

    // Counterchains and iterators
    cu.cchains.foreach(emitComponent(_))

    for ((iter,ccIdx) <- cu.iterators) {
      emit(s"val ${quote(iter)} = ${ccIdx._1.name}(${ccIdx._2})")
    }

    // TODO: How to communicate a non-iter address to SRAM?
    cu.srams.foreach(emitComponent(_))
    // TODO: Stages

    open(s"""CU.updateFields(""")
    emit(s"""cchains = List(${cu.cchains.map(_.name).mkString(", ")}), """)
    emit(s"""srams   = List(${cu.srams.map(_.name).mkString(", ")}), """)
    emit(s"""sins    = List(${cu.scalarIn.map(_.name).mkString(", ")}), """)
    emit(s"""souts   = List(${cu.scalarOut.map(_.name).mkString(", ")}), """)
    emit(s"""vins    = List(${cu.vectorIn.map(_.name).mkString(", ")}), """)
    emit(s"""vouts   = List(${cu.vectorOut.map(_.name).mkString(", ")}) """)
    close(")")
    close("}")
  }

  def emitComponent(x: Any) = x match {
    case CounterChainCopy(name, owner) =>
      emit(s"""val $name = CounterChain.copy(${owner.name}, "$name")""")

    case CounterChainInstance(name, ctrs) =>
      //for (ctr <- ctrs) emitComponent(ctr)
      val bnds = ctrs.map{case PIRCounter(name,start,end,stride,par) => s"(${quote(start)}, ${quote(end)}, ${quote(stride)})"}.mkString(", ")
      emit(s"""val $name = CounterChain(name = "$name", $bnds)""")

    case PIRCounter(name,start,end,stride,par) =>
      // TODO: Currently unused
      emit(s"""val $name = Counter(${quote(start)}, ${quote(end)}, ${quote(stride)})""")

    case PIRMemory(name, size, Some(writer), Some(readAddr), Some(writeAddr)) =>
      emit(s"""val $name = SRAM(size = $size, writer = ${writer.name}, readAddr = ${quote(readAddr)}, writeAddr = ${quote(writeAddr)})""")

    case ScalarIn(name, mem)  => emit(s"val $name = ScalarIn(${mem.name})")
    case ScalarOut(name, mem) => emit(s"val $name = ScalarOut(${mem.name})")
    case VectorIn(name, mem)  => emit(s"val $name = VecIn(${mem.name})")
    case VectorOut(name, mem) => emit(s"val $name = VecOut(${mem.name})")

    case MemCtrl(name,region,mode) => emit(s"val $name = MemoryController($mode, ${region.name})")
    case Offchip(name) => emit(s"val $name = Offchip()")
    case InputArg(name) => emit(s"val $name = ArgIn()")
    case OutputArg(name) => emit(s"val $name = ArgOut()")
    case ScalarMem(name) => emit(s"val $name = Scalar()")
    case VectorMem(name) => emit(s"val $name = Vector()")

    case _ => stageError(s"Don't know how to generate CGRA component: $x")
  }
}
