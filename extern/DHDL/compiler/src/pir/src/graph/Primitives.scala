package dhdl.graph

import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.math.max
import dhdl.Design
import dhdl.graph._


class Primitive(nameStr:Option[String], typeStr:String)(implicit design: Design) 
  extends Node(nameStr, typeStr) { 
  var ctrler:Controller = _
} 
/** Counter node. Represents a chain of counters where each counter counts upto a certain max value. When
 *  the topmost counter reaches its maximum value, the entire counter chain ''saturates'' and does not
 *  wrap around.
 *  @param maxNStride: An iterable [[scala.Seq]] tuple of zero or more values.
 *  Each tuple represents the (max, stride) for one level in the loop.
 *  Maximum values and strides are specified in the order of topmost to bottommost counter.
 */
trait CounterChain extends Primitive {
  var counters:List[Counter] = Nil 
  var dep:Option[CounterChain] = None
  var copy:Option[CounterChain] = None
  def apply(i: Int)(implicit design: Design):Counter = {
    if (counters.size == 0) {
      // Speculatively allocate wires base on need
      this.counters = (0 to i).map { j => 
        Counter() }.toList
    }
    counters(i)
  }
  def copy(cp:CounterChain)(implicit design: Design):Unit = {
    // Check whether speculative wire allocation was correct
    assert(counters.size <= cp.counters.size, 
      s"Accessed counter ${counters.size-1} of ${this} is out of bound")
    val addiCtrs = (counters.size until cp.counters.size).map {i => Counter()}
    counters = counters ++ addiCtrs
    counters.zipWithIndex.foreach { case(c,i) => 
      c.copy(cp.counters(i))
      c.ctrler = this.ctrler
    }
    this.copy = Some(cp)
    toUpdate = false
  }
  def update(bds: Seq[(Wire, Wire, Wire)])(implicit design: Design):Unit = {
    counters = bds.zipWithIndex.map {case (bd,i) => Counter(bd)}.toList
    this.copy = None 
    toUpdate = false
  }
}
object CounterChain {
  val typeStr = "CC"
  def apply(name:Option[String])(implicit design: Design):CounterChain =
    new Primitive(name, typeStr) with CounterChain { toUpdate = true }
  def apply(bds: (Wire, Wire, Wire)*)(implicit design: Design):CounterChain =
    {val c = CounterChain(None); c.update(bds); c}
  def apply(name:String, bds: (Wire, Wire, Wire)*)(implicit design: Design):CounterChain =
    {val c = CounterChain(Some(name)); c.update(bds); c}
  def copy(from:String, name:String) (implicit design: Design):CounterChain = {
    val cc = CounterChain(Some(s"${from}_${name}_copy"))
    def updateFunc(cp:Node) = cc.copy(cp.asInstanceOf[CounterChain])
    design.updateLater(s"${from}_${name}", updateFunc)
    cc
  }
  def copy(from:Controller, name:String) (implicit design: Design):CounterChain = {
    copy(from.toString, name)
  }
}

trait Counter extends Primitive with Wire {
  var bound: (Wire, Wire, Wire) = _
  def copy(c:Counter)(implicit design: Design) = {
    assert(bound==null, s"Overriding existing counter ${this} with bound (${bound._1}, ${bound._2}, ${bound._3})")
    bound = (c.bound._1.copy, c.bound._2.copy, c.bound._3.copy)
  } 
  def create(bd: (Wire, Wire, Wire))(implicit design: Design):Unit = {
    bound = bd
    toUpdate = false
  }
}
object Counter{
  val typeStr = "Ctr"
  def apply(name:Option[String])(implicit design: Design):Counter =
    new Primitive(name, typeStr) with Counter { toUpdate = true }
  def apply(bd: (Wire, Wire, Wire))(implicit design: Design):Counter =
    { val c = Counter(None); c.create(bd); c }
  def apply(name:String, bd: (Wire, Wire, Wire))(implicit design: Design):Counter =
    { val c = Counter(Some(name)); c.create(bd); c }
  def apply()(implicit design: Design):Counter = Counter(None)
}

trait MemPort extends Primitive with Wire {
  val id:Int
  var mem:SRAM = _
}
object MemPort {
  def apply(i:Int)(implicit design: Design) = { 
    new {
      override val id = i
    } with Primitive(None, "Port") with MemPort
  }
  def apply(m:SRAM, i:Int)(implicit design: Design) = { 
    val name = if (m.name.isDefined) Some(s"${m.name.get}_port${i}") else None
    new {
      override val id = i
    } with Primitive(name, "Port") with MemPort { mem = m }
  }
}
/** SRAM 
 *  @param nameStr: user defined name of SRAM 
 *  @param Size: size of SRAM in all dimensions 
 */
trait SRAM extends Primitive {
  val size:Int
  var readAddr: Wire = _
  var writeAddr: Wire = _
  val writePort: Controller
  val readPort: MemPort 
  def update (ra:Wire, wa:Wire) = {
    this.readAddr = ra
    this.writeAddr = wa
    toUpdate = false
  }
  def load = readPort
}
object SRAM {
  def apply(nameStr: Option[String], s: Int, wp:Controller)(implicit design: Design) = { 
    val sram:SRAM = new {
      override val size = s
      override val writePort = wp 
      override val readPort = MemPort(0) //TODO
    } with Primitive(nameStr, "SRAM") with SRAM { toUpdate = true }
    sram.readPort.mem = sram
    sram
  }
  def apply(size:Int, write:Controller)(implicit design: Design): SRAM
    = SRAM(None, size, write)
  def apply(name:String, size:Int, write:Controller)(implicit design: Design): SRAM
    = SRAM(Some(name), size, write)
  def apply(size:Int, write:Controller, readAddr:Wire, writeAddr:Wire)(implicit design: Design): SRAM
    = { val s = SRAM(None, size, write); s.update(readAddr, writeAddr); s } 
  def apply(name:String, size:Int, write:Controller, readAddr:Wire, writeAddr:Wire)(implicit design: Design): SRAM
    = { val s = SRAM(Some(name), size, write); s.update(readAddr, writeAddr); s } 
}

/** SRAM 
 *  @param nameStr: user defined name of SRAM 
 *  @param Size: size of SRAM in all dimensions 
 */
trait Pipeline extends Primitive {
  lazy val stages:List[Stage] = ctrler.nodes.filter(n => n.isInstanceOf[Stage]).asInstanceOf[List[Stage]]
}
object Pipeline {
  val typeStr = "Pipeline"
  def apply(name:Option[String])(block: => Any) (implicit design: Design):Pipeline = 
    { block; new Primitive(name, typeStr) with Pipeline }
  def apply(block: => Any) (implicit design: Design):Pipeline =
    Pipeline(None)(block)
  def apply(name:String) (block: => Any) (implicit design: Design):Pipeline =
    Pipeline(Some(name))(block)
}

trait Stage extends Primitive {
  var operands:List[Wire] = _
  var op:Op = _
  var result:Wire = _
  val mapping:PipeRegMapping
} 
object Stage {
  val typeStr = "Stage"
  def apply(name:Option[String], prm:PipeRegMapping)(implicit design: Design):Stage = 
      new { override val mapping = prm } with Primitive(name, typeStr) with Stage

  def apply(stage:Stage, opds:List[Wire], o:Op, r:Wire, prm:PipeRegMapping)
    (implicit design: Design):Unit= {
    opds.foreach { opd => opd match {
        case pr:PipeReg => prm.addUse(pr)
        case _ =>
      } 
    }
    r match {
      case pr:PipeReg => prm.addDef(pr)
      case _ =>
    }
    stage.operands = opds
    stage.op = o
    stage.result = r 
  }
  def apply(stage:Stage, op1:Wire, op:Op, result:Wire)
           (implicit prm:PipeRegMapping, design: Design):Unit =
    Stage(stage, List(op1), op, result, prm)
  def apply(stage:Stage, op1:Wire, op2:Wire, op:Op, result:Wire)
           (implicit prm:PipeRegMapping, design: Design):Unit = 
    Stage(stage, List(op1, op2), op, result, prm)
  def apply(stage:Stage, op1:Wire, op2:Wire, op3:Wire, op:Op, result:Wire)
           (implicit prm:PipeRegMapping, design: Design):Unit =
    Stage(stage, List(op1, op2, op3), op, result, prm)
  //TODO
  def reduce(stage:Stage, op:Op) (implicit prm:PipeRegMapping, design: Design):Unit =
    Stage(stage, List(prm.reduce(stage), prm.reduce(stage)), op, prm.reduce(stage), prm)
}
object Stages{
  def apply(n:Int) (implicit prm:PipeRegMapping, design: Design):List[Stage] = {
    List.tabulate(n) {i => Stage(None, prm)}
  }
}

trait PipeRegMapping extends Primitive {
  var regId = 0
  private def newTemp = {val temp = regId; regId +=1; temp}

  /* Register Mapping */
  val reduceReg = newTemp
  val networkIns = ListBuffer[Int]() 
  //val networkOut = ListBuffer[Int]() 
  val networkOut = newTemp 
  val loadRegs  = HashMap[SRAM, Int]()
  val ctrRegs   = HashMap[Counter, Int]()
  val tempRegs  = ListBuffer[Int]()

  val stageUses = HashMap[Stage, PipeReg]()
  val stageDefs = HashMap[Stage, PipeReg]()
  val stagePRs  = HashMap[Stage, HashMap[Int,PipeReg]]()
  def reset     = { regId = 0; loadRegs.clear; ctrRegs.clear; stageUses.clear; stageDefs.clear }

  def addUse(p:PipeReg) = stageUses += (p.stage -> p) 
  def addDef(p:PipeReg) = stageDefs += (p.stage -> p) 

  def load(stage:Stage, s:SRAM)(implicit design: Design):PipeReg = {
    if (!loadRegs.contains(s))
      loadRegs += (s -> newTemp)
    getPR(stage, loadRegs(s))
  }
  def ctr(stage:Stage, c:Counter)(implicit design: Design):PipeReg = {
    if (!ctrRegs.contains(c))
      ctrRegs += (c -> newTemp)
    getPR(stage, ctrRegs(c))
  }
  def reduce(stage:Stage)(implicit design: Design):PipeReg = {
    getPR(stage, reduceReg)
  }
  //def networkIn(stage:Stage, c:Controller)(implicit design: Design):PipeReg = {
  //  if (!networkIns.contains(c))
  //    networkIns += (c -> newTemp)
  //  getPR(stage, ctrRegs(c))
  //}
  def networkOut(stage:Stage)(implicit design: Design):PipeReg = {
    getPR(stage, networkOut)
  }
  def temp = newTemp
  def temp(stage:Stage, regId:Int)(implicit design: Design):PipeReg = {
    getPR(stage, regId)
  }
  def temp(stage:Stage)(implicit design: Design):PipeReg = {
    PipeReg(stage, newTemp)
  }
  def getPR(stage:Stage, regId:Int)(implicit design: Design):PipeReg = {
    if (!stagePRs.contains(stage))
      stagePRs += (stage -> HashMap[Int, PipeReg]())
    val prs = stagePRs(stage)
    if (!prs.contains(regId))
      prs += (regId -> PipeReg(stage, regId))
    prs(regId)
  }

}
object PipeRegMapping {
  def apply(nameStr:Option[String])(implicit design: Design):PipeRegMapping = {
    new Primitive(nameStr, "PipelineRegisters") with PipeRegMapping 
  }
}

trait Reg extends Primitive with Wire{
  var in:Option[Wire] = None
}

trait PipeReg extends Reg {
  var stage:Stage = _
  val mapping:Int
  override def toString = s"${super.toString}_${stage.name.getOrElse("")}${mapping}"
}
object PipeReg {
  def apply(nameStr:Option[String], s:Option[Stage], m:Int)(implicit design: Design):PipeReg = new {
    override val mapping = m
  } with Primitive(nameStr, "PR") with PipeReg {
    if (s.isDefined)
      stage = s.get
    else
      toUpdate = true 
  } 
  def apply(s:Stage, m:Int) (implicit design: Design):PipeReg = PipeReg(None, Some(s), m)
  def apply(name:String, s:Stage, m:Int) (implicit design: Design):PipeReg = 
    PipeReg(Some(name), Some(s), m)
  def apply(m:Int) (implicit design: Design):PipeReg = PipeReg(None, None, m)
}

trait ArgIn extends Reg 
object ArgIn {
  def apply(nameStr:Option[String], w:Option[Int])(implicit design: Design):ArgIn = new {
    //override val out = Wire(if (nameStr.isDefined) Some(s"${nameStr.get}_out") else None, w)
  } with Primitive(nameStr, "ArgIn") with ArgIn
  def apply() (implicit design: Design):ArgIn = ArgIn(None, None)
  def apply(w:Int) (implicit design: Design):ArgIn = ArgIn(None, Some(w))
  def apply(name:String, w:Int) (implicit design: Design):ArgIn = ArgIn(Some(name), Some(w))
}

/** Scalar values passed from accelerator to host CPU code via memory-mapped registers.
 *  Represent scalar outputs from the accelerator, and are write-only from accelerator.
 *  @param value: Combinational node whose value is to be output to CPU
 */
trait ArgOut extends Reg
object ArgOut {
  def apply(nameStr:Option[String], value:Wire, w:Option[Int])(implicit design: Design) = new {
    //override val out = Wire(if (nameStr.isDefined) Some(s"${nameStr.get}_out") else None, w)
  } with Primitive(nameStr, "ArgOut") with ArgOut { in = Some(value) }
  def apply(value:Wire) (implicit design: Design):ArgOut = ArgOut(None, value, None)
  def apply(value:Wire, w:Int) (implicit design: Design):ArgOut = ArgOut(None, value, Some(w))
  def apply(name:String, value:Wire, w:Int) (implicit design: Design):ArgOut = ArgOut(Some(name), value, Some(w))
}
