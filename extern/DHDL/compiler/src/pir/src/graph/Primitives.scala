package dhdl.graph

import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
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
  val readPort: Port = Port(this, s"${this}.rp") 

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
  /* No Sugar API */
  def apply(stage:Stage, opds:List[Port], o:Op, r:Port, prm:Pipeline)
    (implicit design: Design):Unit= {
    stage.operands = opds
    stage.op = o
    stage.result = r 
    prm.addStage(stage)
  }
  /* Sugar API */
  def apply(stage:Stage, op1:Port, op:Op, result:Port)
           (implicit prm:Pipeline, design: Design):Unit =
    Stage(stage, List(op1), op, result, prm)
  def apply(stage:Stage, op1:Port, op2:Port, op:Op, result:Port)
           (implicit prm:Pipeline, design: Design):Unit = 
    Stage(stage, List(op1, op2), op, result, prm)
  def apply(stage:Stage, op1:Port, op2:Port, op3:Port, op:Op, result:Port)
           (implicit prm:Pipeline, design: Design):Unit =
    Stage(stage, List(op1, op2, op3), op, result, prm)
  //TODO
  def reduce(stage:Stage, op:Op) (implicit prm:Pipeline, design: Design):Unit = {
    Stage(stage, List(prm.reduce(stage).read), op, prm.reduce(stage).read, prm)
  }

}
object Stages {
  def apply(n:Int) (implicit prm:Pipeline, design: Design):List[Stage] = {
    List.tabulate(n) {i => 
      val s = Stage(None, prm)
      prm.stageUses += (s -> Set[Int]())
      prm.stageDefs += (s -> Set[Int]())
      prm.stagePRs += (s -> HashMap[Int, PipeReg]())
      s
    }
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
  val storeRegs  = HashMap[SRAM, Int]()
  val ctrRegs   = HashMap[Counter, Int]()
  val tempRegs  = ListBuffer[Int]()

  val stageUses = HashMap[Stage, Set[Int]]()
  val stageDefs = HashMap[Stage, Set[Int]]()
  val stagePRs  = HashMap[Stage, HashMap[Int,PipeReg]]()
  def reset     = { regId = 0; loadRegs.clear; storeRegs.clear; ctrRegs.clear; stageUses.clear; stageDefs.clear }

  def addStage(s:Stage):Unit = {
    stages += s
    s.operands.foreach { opd => opd.src match {
        case pr:PipeReg => addUse(pr)
        case _ =>
      } 
    }
    s.result.src match {
      case pr:PipeReg => addDef(pr)
      case _ =>
    }
  }
  private def addUse(p:PipeReg) = stageUses(p.stage) += p.regId 
  private def addDef(p:PipeReg) = stageDefs(p.stage) += p.regId 

 /** Create a pipeline register for a stage corresponding to 
  *  the register that loads from the sram
  * @param stage: Stage for the pipeline register 
  * @param s: sram to load from 
  */
 def load(stage:Stage, s:SRAM)(implicit design: Design):PipeReg = {
    if (!loadRegs.contains(s)) loadRegs += (s -> newTemp)
    val prs = stagePRs(stage); val rid = loadRegs(s)
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with LoadPR)
    prs(rid)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that stores to the sram
  * @param stage: Stage for the pipeline register 
  * @param s: sram to load from 
  */
  def stores(stage:Stage, s:SRAM)(implicit design: Design):PipeReg = {
    if (!storeRegs.contains(s)) storeRegs += (s -> newTemp)
    val prs = stagePRs(stage); val rid = storeRegs(s)
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with StorePR)
    prs(rid)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to the counter 
  * @param stage: Stage for the pipeline register 
  * @param c: counter 
  */
  def ctr(stage:Stage, c:Counter)(implicit design: Design):PipeReg = {
    if (!ctrRegs.contains(c)) ctrRegs += (c -> newTemp)
    val prs = stagePRs(stage); val rid = ctrRegs(c)
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with CtrPR)
    prs(rid)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to the reduction network 
  * @param stage: Stage for the pipeline register 
  */
  def reduce(stage:Stage)(implicit design: Design):PipeReg = {
    val prs = stagePRs(stage); val rid = reduceReg
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with ReducePR)
    prs(rid)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to 1 scalarIn buffer 
  * @param stage: Stage for the pipeline register 
  */
  def scalarIn(stage:Stage)(implicit design: Design):PipeReg = {
    val rid = newTemp; scalarIns += rid 
    val prs = stagePRs(stage)
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with ScalarInPR)
    prs(rid)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to the scalarIn buffer with register rid
  * @param stage: Stage for the pipeline register 
  * @param rid: reg rid of scalar input 
  */
  def scalarIn(stage:Stage, rid:Int)(implicit design: Design):PipeReg = {
    if (!scalarIns.contains(rid)) scalarIns += rid 
    val prs = stagePRs(stage)
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with ScalarInPR)
    prs(rid)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to 1 scalarOut buffer 
  * @param stage: Stage for the pipeline register 
  */
  def scalarOut(stage:Stage)(implicit design: Design):PipeReg = {
    val rid = newTemp; scalarOuts += rid 
    val prs = stagePRs(stage)
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with ScalarOutPR)
    prs(rid)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that connects to the scalarOut buffer with register rid
  * @param stage: Stage for the pipeline register 
  * @param rid: reg rid of scalar input 
  */
  def scalarOut(stage:Stage, rid:Int)(implicit design: Design):PipeReg = {
    if (!scalarOuts.contains(rid)) scalarOuts += rid 
    val prs = stagePRs(stage)
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with ScalarOutPR)
    prs(rid)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that directly connects to CU input ports in streaming communication 
  * @param stage: Stage for the pipeline register 
  */
  def vecIn(stage:Stage)(implicit design: Design):PipeReg = {
    val prs = stagePRs(stage); val rid = vecIn
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with VecInPR)
    prs(rid)
  }
 /** Create a pipeline register for a stage corresponding to 
  *  the register that directly connects to CU output ports 
  * @param stage: Stage for the pipeline register 
  */
  def vecOut(stage:Stage)(implicit design: Design):PipeReg = {
    val prs = stagePRs(stage); val rid = vecOut 
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with VecOutPR)
    prs(rid)
  }
  def temp = newTemp
 /** Get the pipeline register for stage with rid 
  * @param stage: Stage for the pipeline register 
  */
  def temp(stage:Stage, rid:Int)(implicit design: Design):PipeReg = {
    val prs = stagePRs(stage)
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with TempPR)
    prs(rid)
  }
 /** Allocate a new pipeline register in the stage 
  * @param stage: Stage for the pipeline register 
  */
  def temp(stage:Stage)(implicit design: Design):PipeReg = {
    val prs = stagePRs(stage); val rid = newTemp
    if (!prs.contains(rid)) prs += (rid -> new PipeReg(stage, rid) with TempPR)
    prs(rid)
  }

}

trait Reg extends Primitive {
  var in:Option[Port] = None
  val out:Port = Port(this, {s"${this}"}) 
  def read:Port = out
}

/*
 * A Pipeline Register keeping track of which stage (column) and which logical register (row)
 * the PR belongs to
 * @param n Optional user defined name
 * @param regId Register ID the PipeReg mapped to
 **/
case class PipeReg(n:Option[String], stage:Stage, regId:Int)(implicit design: Design) 
  extends Primitive(n, "PR") with Reg {
  override def toString = s"${super.toString}_${stage.name.getOrElse("")}${regId}"
  def this (stage:Stage, regId:Int)(implicit design:Design) = this(None, stage, regId)
}
trait LoadPR      extends PipeReg {override val typeStr = "PRld"}
trait StorePR     extends PipeReg {override val typeStr = "PRst"}
trait CtrPR       extends PipeReg {override val typeStr = "PRct"}
trait ReducePR    extends PipeReg {override val typeStr = "PRrd"}
trait VecInPR     extends PipeReg {override val typeStr = "PRvi"}
trait VecOutPR    extends PipeReg {override val typeStr = "PRvo"}
trait ScalarInPR  extends PipeReg {override val typeStr = "PRsi"}
trait ScalarOutPR extends PipeReg {override val typeStr = "PRso"}
trait TempPR      extends PipeReg {override val typeStr = "PRtp"}

case class Const(n:Option[String], value:Long)(implicit design: Design) 
  extends Primitive(n, "Const") with Reg{
  override val out:Port = Port(this, {s"Const(${value})"}, {Const(name, value).out})
}
object Const {
  def apply(v:Long) (implicit design:Design):Const = Const(None, v)
  def apply(name:String, v:Long) (implicit design:Design):Const = Const(Some(name), v)
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
