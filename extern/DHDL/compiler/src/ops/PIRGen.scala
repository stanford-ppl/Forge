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

trait PIRGen extends Traversal with QuotingExp {
  val IR: DHDLExp with PIRScheduleAnalysisExp
  import IR._

  override val name = "PIR Generation"
  debugMode = true

  val dir = sys.env("PIR_HOME") + "/apps/"
  val app = Config.degFilename.drop(4)
  val filename = app + ".scala"

  lazy val scheduler = new PIRScheduleAnalyzer{val IR: PIRGen.this.IR.type = PIRGen.this.IR}
  lazy val collector = new SymbolCollector{val IR: PIRGen.this.IR.type = PIRGen.this.IR}
  lazy val constants = collector.constants
  lazy val argIns    = collector.argIns
  lazy val argOuts   = collector.argOuts
  lazy val top       = scheduler.top
  lazy val cus       = scheduler.cuMapping

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
      finally {
        stream.flush()
        stream.close()
      }
    }
    (b)
  }

  def emitPIR(b: Block[Any]) {
    collector.run(b)
    scheduler.run(b)

    debug("Scheduling complete. Generating...")
    generateHeader()
    generateGlobals()
    generateCompute(top.get)
    generateFooter()
  }

  def generateHeader() {
    emit("import pir.graph._")
    emit("import pir.graph")
    emit("import pir.codegen._")
    emit("import pir.plasticine.config._")
    emit("import pir.Design")
    emit("import pir.PIRMisc._")
    open(s"""object ${app}Design extends Design {""")
    emit(s"""override val arch = Config0""")
  }

  def generateGlobals() {
    constants.foreach{
      case Const(c) =>
      case c@Exact(d) => emit(s"val ${quote(c)} = Const(${d.toLong}l)") // TODO
    }
    argIns.foreach{argIn => emit(s"val ${quote(argIn)} = ArgIn()")}
    argOuts.foreach{argOut => emit(s"val ${quote(argOut)} = ArgOut()")}
  }

  def generateFooter() {
    emit("def main(args: Array[String]): Unit = run")
    close("}")
  }

  def generateCompute(top: Exp[Any]) {
    var frontier = List(top)
    while (frontier.nonEmpty) {
      for (pipe <- frontier) generateCU(pipe, cus(pipe))

      frontier = frontier.flatMap{case pipe => childrenOf(pipe) }
    }
  }

  def cuVariant(cu: PIRCompute) = cu match {
    case cu: BasicComputeUnit => "ComputeUnit"
    case cu: MemoryComputeUnit => "MemoryController"
  }
  def controlType(tpe: ControlType) = tpe match {
    case InnerPipe => "Pipe"
    case CoarsePipe => "MetaPipeline"
    case SequentialPipe => "Sequential"
    case StreamPipe => stageError("Stream pipe not yet supported in PIR")
  }

  def generateCU(pipe: Exp[Any], cu: PIRCompute, suffix: String = "") {
    val parentName = cu.parent.map(_.name).getOrElse("Top")

    open(s"val ${cu.name} = {")
    for (cc <- cu.cchains) emitComponent(cc)
    for (mem <- cu.srams) emitComponent(mem)
    for ((iter,ccIdx) <- cu.iterators) {
      emit(s"val $iter = ${ccIdx._1.name}(${ccIdx._2})")
    }
    emit(s"val PL = Pipeline(None)")

    open(s"""${cuVariant(cu)}(""")
    emit(s"""name    = Some("${cu.name}"), """)
    emit(s"""parent  = "$parentName", """)
    emit(s"""cchains = List(${cu.cchains.map(_.name).mkString(", ")}), """)
    emit(s"""srams   = List(${cu.srams.map(_.name).mkString(", ")}), """)
    emit(s"""sins    = List(${cu.scalarIn.map(quote(_)).mkString(", ")}), """)
    emit(s"""souts   = List(${cu.scalarOut.map(quote(_)).mkString(", ")}), """)
    emit(s"""pipeline = PL,""")

    cu match {
      case BasicComputeUnit(name,parent,tpe) =>
        emit(s"""tpe = ${controlType(tpe)}""")

      case MemoryComputeUnit(name,parent,dram) =>
        emit(s"""dram = "$dram" """)
    }
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

    case _ => stageError(s"Don't know how to generate CGRA component: $x")
  }

  /*override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {

  }*/
}
