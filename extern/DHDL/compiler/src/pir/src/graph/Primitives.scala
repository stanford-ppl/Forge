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
  def updateCtrler(c:Controller) = ctrler = c
} 
/** Counter node. Represents a chain of counters where each counter counts upto a certain max value. When
 *  the topmost counter reaches its maximum value, the entire counter chain ''saturates'' and does not
 *  wrap around.
 *  @param maxNStride: An iterable [[scala.Seq]] tuple of zero or more values.
 *  Each tuple represents the (max, stride) for one level in the loop.
 *  Maximum values and strides are specified in the order of topmost to bottommost counter.
 */
trait CounterChain extends Primitive {
  /* Fields */
  var counters:List[Counter] = Nil 
  /* Pointers */
  var dep:Option[CounterChain] = None
  var copy:Option[CounterChain] = None

  override def updateCtrler(c:Controller) = {super.updateCtrler(c); counters.foreach(_.updateCtrler(c))} 
  def apply(i: Int)(implicit design: Design):Counter = {
    if (counters.size == 0) {
      // Speculatively create counters base on need and check index out of bound during update
      this.counters = (0 to i).map { j => Counter() }.toList
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
  def update(bds: Seq[(Port, Port, Port)])(implicit design: Design):Unit = {
    counters = bds.zipWithIndex.map {case ((mi, ma, s),i) => Counter(mi, ma, s)}.toList
    this.copy = None 
    toUpdate = false
  }
}
object CounterChain {
  val typeStr = "CC"
  def apply(name:Option[String])(implicit design: Design):CounterChain =
    new Primitive(name, typeStr) with CounterChain { toUpdate = true }
  def apply(bds: (Port, Port, Port)*)(implicit design: Design):CounterChain =
    {val c = CounterChain(None); c.update(bds); c}
  def apply(name:String, bds: (Port, Port, Port)*)(implicit design: Design):CounterChain =
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

case class Counter(n:Option[String])(implicit design: Design) extends Primitive(n, "Ctr"){
  /* Fields */
  var min:Port = _
  var max:Port = _
  var step:Port = _
  val out:Port = Port(this, {s"${this}.out"}) 
  toUpdate = true

  def update(mi:Port, ma:Port, s:Port)(implicit design: Design):Unit = {
    min = mi
    max  = ma
    step = s
    toUpdate = false
  }
  def copy(c:Counter)(implicit design: Design) = {
    assert(min==null, 
      s"Overriding existing counter ${this} with min ${min}")
    assert(max==null, 
      s"Overriding existing counter ${this} with min ${max}")
    assert(step==null, 
      s"Overriding existing counter ${this} with min ${step}")
    update(c.min.copy, c.max.copy, c.step.copy)
  } 
}
object Counter{
  def apply(min:Port, max:Port, step:Port)(implicit design: Design):Counter =
    { val c = Counter(None); c.update(min, max, step); c }
  def apply(name:String, min:Port, max:Port, step:Port)(implicit design: Design):Counter =
    { val c = Counter(Some(name)); c.update(min, max, step); c }
  def apply()(implicit design: Design):Counter = Counter(None)
}

/** SRAM 
 *  @param nameStr: user defined name of SRAM 
 *  @param Size: size of SRAM in all dimensions 
 */
case class SRAM(n: Option[String], size: Int, writer:Controller)(implicit design: Design) 
  extends Primitive(n, "SRAM") {
  var readAddr: Port = _
  var writeAddr: Port = _
  val readPort: Port = Port(this, s"s${this}.rp") 

  toUpdate = true
  def update (ra:Port, wa:Port) = {
    this.readAddr = ra
    this.writeAddr = wa
    toUpdate = false
  }
  def load = readPort
}
object SRAM {
  def apply(size:Int, write:Controller)(implicit design: Design): SRAM
    = SRAM(None, size, write)
  def apply(name:String, size:Int, write:Controller)(implicit design: Design): SRAM
    = SRAM(Some(name), size, write)
  def apply(size:Int, write:Controller, readAddr:Port, writeAddr:Port)(implicit design: Design): SRAM
    = { val s = SRAM(None, size, write); s.update(readAddr, writeAddr); s } 
  def apply(name:String, size:Int, write:Controller, readAddr:Port, writeAddr:Port)(implicit design: Design): SRAM
    = { val s = SRAM(Some(name), size, write); s.update(readAddr, writeAddr); s } 
}

case class Stage(n:Option[String], pipeline:Pipeline)(implicit design: Design) extends Primitive(n, "Stage") {
  var operands:List[Port] = _
  var op:Op = _
  var result:Port = _
} 
object Stage {
  def apply(stage:Stage, opds:List[Port], o:Op, r:Port, prm:Pipeline)
    (implicit design: Design):Unit= {
    stage.operands = opds
    stage.op = o
    stage.result = r 
    prm.addStage(stage)
  }
  //TODO
  def reduce(stage:Stage, op:Op) (implicit prm:Pipeline, design: Design):Unit = {
    Stage(stage, List(prm.reduce(stage).read, prm.reduce(stage).read), op, prm.reduce(stage).read, prm)
  }

  def apply(stage:Stage, op1:Port, op:Op, result:Port)
           (implicit prm:Pipeline, design: Design):Unit =
    Stage(stage, List(op1), op, result, prm)
  def apply(stage:Stage, op1:Port, op2:Port, op:Op, result:Port)
           (implicit prm:Pipeline, design: Design):Unit = 
    Stage(stage, List(op1, op2), op, result, prm)
  def apply(stage:Stage, op1:Port, op2:Port, op3:Port, op:Op, result:Port)
           (implicit prm:Pipeline, design: Design):Unit =
    Stage(stage, List(op1, op2, op3), op, result, prm)
}
object Stages {
  def apply(n:Int) (implicit prm:Pipeline, design: Design):List[Stage] = {
    List.tabulate(n) {i => Stage(None, prm)}
  }
}

case class Pipeline(n:Option[String])(implicit design: Design) extends Primitive(n, "Pipeline"){
  var regId = 0
  private def newTemp = {val temp = regId; regId +=1; temp}

  /* Fields */
  val stages = ListBuffer[Stage]()
  override def updateCtrler(c:Controller) = {super.updateCtrler(c); stages.foreach(_.updateCtrler(c))} 

  /* Register Mapping */
  val reduceReg = newTemp
  val vecIn = newTemp
  val vecOut = newTemp
  val scalarIns = ListBuffer[Int]() 
  val scalarOuts = ListBuffer[Int]() 
  val loadRegs  = HashMap[SRAM, Int]()
  val writeRegs  = HashMap[SRAM, Int]()
  val ctrRegs   = HashMap[Counter, Int]()
  val tempRegs  = ListBuffer[Int]()

  val stageUses = HashMap[Stage, PipeReg]()
  val stageDefs = HashMap[Stage, PipeReg]()
  val stagePRs  = HashMap[Stage, HashMap[Int,PipeReg]]()
  def reset     = { regId = 0; loadRegs.clear; writeRegs.clear; ctrRegs.clear; stageUses.clear; stageDefs.clear }

  def addStage(s:Stage):Unit = {
    stages += s
    s.operands.foreach { opd => opd match {
        case pr:PipeReg => addUse(pr)
        case _ =>
      } 
    }
    s.result match {
      case pr:PipeReg => addDef(pr)
      case _ =>
    }
  }
  private def addUse(p:PipeReg) = stageUses += (p.stage -> p) 
  private def addDef(p:PipeReg) = stageDefs += (p.stage -> p) 

 /** Create a pipeline register for a stage corresponding to 
  *  the register that loads from the sram
  * @param stage: Stage for the pipeline register 
  * @param s: sram to load from 
  */
 def load(stage:Stage, s:SRAM)(implicit design: Design):PipeReg = {
    if (!loadRegs.contains(s))
      loadRegs += (s -> newTemp)
    getPR(stage, loadRegs(s))
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that writes to the sram
  * @param stage: Stage for the pipeline register 
  * @param s: sram to load from 
  */
  def write(stage:Stage, s:SRAM)(implicit design: Design):PipeReg = {
    if (!writeRegs.contains(s))
      writeRegs += (s -> newTemp)
    getPR(stage, writeRegs(s))
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to the counter 
  * @param stage: Stage for the pipeline register 
  * @param c: counter 
  */
  def ctr(stage:Stage, c:Counter)(implicit design: Design):PipeReg = {
    if (!ctrRegs.contains(c))
      ctrRegs += (c -> newTemp)
    getPR(stage, ctrRegs(c))
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to the reduction network 
  * @param stage: Stage for the pipeline register 
  */
  def reduce(stage:Stage)(implicit design: Design):PipeReg = {
    getPR(stage, reduceReg)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to 1 scalarIn buffer 
  * @param stage: Stage for the pipeline register 
  */
  def scalarIn(stage:Stage)(implicit design: Design):PipeReg = {
    val id = newTemp
    scalarIns += id 
    getPR(stage, id)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to the scalarIn buffer with register id
  * @param stage: Stage for the pipeline register 
  * @param id: reg id of scalar input 
  */
  def scalarIn(stage:Stage, id:Int)(implicit design: Design):PipeReg = {
    if (!scalarIns.contains(id))
      scalarIns += id 
    getPR(stage, id)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to 1 scalarOut buffer 
  * @param stage: Stage for the pipeline register 
  */
  def scalarOut(stage:Stage)(implicit design: Design):PipeReg = {
    val id = newTemp
    scalarOuts += id 
    getPR(stage, id)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to the scalarOut buffer with register id
  * @param stage: Stage for the pipeline register 
  * @param id: reg id of scalar input 
  */
  def scalarOut(stage:Stage, id:Int)(implicit design: Design):PipeReg = {
    if (!scalarOuts.contains(id))
      scalarOuts += id 
    getPR(stage, id)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that directly connects to CU input ports in streaming communication 
  * @param stage: Stage for the pipeline register 
  */
 //TODO 
  def vecIn(stage:Stage)(implicit design: Design):PipeReg = {
    getPR(stage, vecIn)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that directly connects to CU output ports 
  * @param stage: Stage for the pipeline register 
  */
  def vecOut(stage:Stage)(implicit design: Design):PipeReg = {
    getPR(stage, vecOut)
  }
  def temp = newTemp
 /** Get the pipeline register for stage with regId 
  * @param stage: Stage for the pipeline register 
  */
  def temp(stage:Stage, regId:Int)(implicit design: Design):PipeReg = {
    getPR(stage, regId)
  }
 /** Allocate a new pipeline register in the stage 
  * @param stage: Stage for the pipeline register 
  */
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

trait Reg extends Primitive {
  var in:Option[Port] = None
  val out:Port = Port(this, {s"${this}.out"}) 
  def read:Port = out
}

case class PipeReg(n:Option[String], mapping:Int)(implicit design: Design) 
  extends Primitive(n, "PR") with Reg {
  var stage:Stage = _
  toUpdate = true
  override def toString = s"${super.toString}_${stage.name.getOrElse("")}${mapping}"

  def this(n:Option[String], m:Int, s:Stage)(implicit design: Design) = {
    this(n, m)
    stage = s
    toUpdate = false
  }
}
object PipeReg {
  def apply(s:Stage, m:Int) (implicit design: Design):PipeReg = new PipeReg(None, m, s)
  def apply(name:String, s:Stage, m:Int) (implicit design: Design):PipeReg = new PipeReg(Some(name), m, s)
  def apply(m:Int) (implicit design: Design):PipeReg = PipeReg(None, m)
}

trait ArgIn extends Reg 
object ArgIn {
  def apply(nameStr:Option[String], w:Option[Int])(implicit design: Design):ArgIn = new {
    //override val out = Port(if (nameStr.isDefined) Some(s"${nameStr.get}_out") else None, w)
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
  def apply(nameStr:Option[String], value:Port, w:Option[Int])(implicit design: Design) = new {
    //override val out = Port(if (nameStr.isDefined) Some(s"${nameStr.get}_out") else None, w)
  } with Primitive(nameStr, "ArgOut") with ArgOut { in = Some(value) }
  def apply(value:Port) (implicit design: Design):ArgOut = ArgOut(None, value, None)
  def apply(value:Port, w:Int) (implicit design: Design):ArgOut = ArgOut(None, value, Some(w))
  def apply(name:String, value:Port, w:Int) (implicit design: Design):ArgOut = ArgOut(Some(name), value, Some(w))
}
