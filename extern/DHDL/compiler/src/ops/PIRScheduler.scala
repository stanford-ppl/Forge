package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.HashMap

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait PIRScheduleAnalysisExp extends NodeMetadataOpsExp with ReductionAnalysisExp {
  this: DHDLExp =>

  case class PIRStage(op: Exp[Any], isReduce: Boolean = false, isWrite: Boolean = false)

  sealed abstract class PIRCompute(val name: String, val parent: Option[PIRCompute]) {
    var cchains: List[PIRCounterChain] = Nil
    var iterators: Map[Exp[Any], (PIRCounterChain, Int)] = Map.empty
    var srams: List[PIRMemory] = Nil
    var stages: List[PIRStage] = Nil
    var scalarIn: List[Exp[Any]] = Nil
    var scalarOut: List[Exp[Any]] = Nil
  }

  case class BasicComputeUnit(
    override val name: String,
    override val parent: Option[PIRCompute],
    val tpe: ControlType
  ) extends PIRCompute(name,parent)

  case class MemoryComputeUnit(
    override val name: String,
    override val parent: Option[PIRCompute],
    val dram: String
  ) extends PIRCompute(name,parent)

  // TODO: Parallelism?
  case class PIRCounter(name: String, start: Exp[Any], end: Exp[Any], stride: Exp[Any], par: Exp[Any])

  sealed abstract class PIRCounterChain(val name: String)
  case class CounterChainCopy(override val name: String, owner: PIRCompute) extends PIRCounterChain(name)
  case class CounterChainInstance(override val name: String, ctrs: List[PIRCounter]) extends PIRCounterChain(name)

  case class PIRMemory(
    name: String,
    size: Int,
    var writer: Option[PIRCompute] = None,
    var readAddr: Option[Exp[Any]] = None,
    var writeAddr: Option[Exp[Any]] = None
  )

  type RemoteWrite = LocalWrite // Same information, just for code clarity

  private def remoteWriterUnapply(d: Def[Any]): Option[List[RemoteWrite]] = d match {
    case EatReflect(Bram_store(bram,addr,value)) => Some(RemoteWrite(bram,value,addr))
    case EatReflect(Push_fifo(fifo,value,_))     => Some(RemoteWrite(fifo,value))
    case _ => None
  }

  object RemoteWrite {
    def apply(mem: Exp[Any]): List[RemoteWrite] = List( (mem, None, None) )
    def apply(mem: Exp[Any], value: Exp[Any]): List[RemoteWrite] = List( (mem, Some(value), None) )
    def apply(mem: Exp[Any], value: Exp[Any], addr: Exp[Any]): List[RemoteWrite] = List( (mem, Some(value), Some(addr)) )
  }
  object RemoteWriter {
    def unapply(s: Exp[Any]): Option[List[RemoteWrite]] = s match {
      case Def(d) => remoteWriterUnapply(d)
      case _ => None
    }
    def unapply(d: Def[Any]): Option[List[RemoteWrite]] = remoteWriterUnapply(d)
  }
}

trait SymbolCollector extends Traversal {
  val IR: DHDLExp
  import IR._
  override val recurse = Always
  var constants: List[Exp[Any]] = Nil
  var argIns: List[Exp[Any]] = Nil
  var argOuts: List[Exp[Any]] = Nil

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = {
    constants :::= syms(rhs).filter{case Exact(_) => true; case _ => false}

    rhs match {
      case Argin_new(_) => argIns ::= lhs
      case Argout_new(_) => argOuts ::= lhs
      case _ =>
    }
  }
}

trait PIRScheduleAnalyzer extends Traversal with SpatialTraversalTools with QuotingExp {
  val IR: DHDLExp with PIRScheduleAnalysisExp
  import IR._

  override val name = "PIR Scheduling"
  override val eatReflect = true
  debugMode = true

  // --- State
  // TODO: Is there always only one CU per pipe?
  var pipes = List[Exp[Any]]()
  val cuMapping = HashMap[Exp[Any], PIRCompute]()
  var top: Option[Exp[Any]] = None

  // --- CounterChains

  def copyParentCChains(x: Option[Exp[Any]]): List[CounterChainCopy] = x match {
    case Some(ctrl) =>
      val cu = cuMapping(ctrl)
      syms(ctrl).flatMap {
        case cc@Deff(Counterchain_new) => Some(CounterChainCopy(quote(cc), cu))
        case _ => None
      } ++ copyParentCChains(parentOf(ctrl))
    case None => Nil
  }
  def allocateCChains(pipe: Exp[Any]): List[PIRCounterChain] = {
    syms(pipe).flatMap {
      case cc@Deff(Counterchain_new(ctrs,nIter)) =>
        val ctrInsts = ctrs.map{case ctr@Deff(Counter_new(start,end,stride,par)) => PIRCounter(quote(ctr),start,end,stride,par) }
        Some(CounterChainInstance(quote(cc),ctrInsts))
      case _ => None
    } ++ copyParentCChains(parentOf(pipe))
  }

  // --- Compute Units

  def initCU[T<:PIRCompute](cu: T, pipe: Exp[Any]): T = {
    cuMapping(pipe) = cu
    pipes ::= pipe
    cu.cchains ++= allocateCChains(pipe)
    cu.iterators ++= parentOf(pipe).map{parent => cuMapping(parent).iterators.toList}.getOrElse(Nil)
    cu
  }

  // Get associated CU (or create a new one if not allocated yet)
  def allocateBasicCU(pipe: Exp[Any]): BasicComputeUnit = {
    if (cuMapping.contains(pipe)) cuMapping(pipe).asInstanceOf[BasicComputeUnit]
    else {
      if (top.isEmpty && parentOf(pipe).isEmpty) top = Some(pipe)

      val parent = parentOf(pipe).map(cuMapping(_))
      val cu = BasicComputeUnit(quote(pipe), parent, styleOf(pipe))
      initCU(cu, pipe)
    }
  }

  def allocateMemoryCU(pipe: Exp[Any], mem: Exp[Any]): MemoryComputeUnit = {
    if (cuMapping.contains(pipe)) cuMapping(pipe).asInstanceOf[MemoryComputeUnit]
    else {
      val parent = parentOf(pipe).map(cuMapping(_))
      val cu = MemoryComputeUnit(quote(pipe), parent, quote(mem))
      initCU(cu, pipe)
    }
  }

  def memSize(mem: Exp[Any]) = mem match {
    case Deff(Bram_new(depth,_)) => bound(depth).get.toInt
    case Deff(Fifo_new(depth,_)) => bound(depth).get.toInt
    case _ => stageError(s"Disallowed local memory $mem")
  }
  def isBuffer(mem: Exp[Any]) = isFIFO(mem.tp) || isBRAM(mem.tp)

  def scheduleStages(pipe: Exp[Any], func: Block[Any]) = {
    val stms = getStmsInBlock(func)
    val stages = getStages(func)
    val cu = allocateBasicCU(pipe)

    val remoteWriters = stages.filter{case RemoteWriter(_) => true; case _ => false}
    val writeAddrStms = remoteWriters.flatMap{case RemoteWriter(writes) =>
      writes.flatMap{case (mem,value,addr) =>
        val addrComputation = getSchedule(stms)(addr,false)
        readersOf(mem).map{case (ctrl,_,_) =>
          val readerCU = allocateBasicCU(ctrl)
          readerCU.stages ++= addrComputation.map{case TP(s,d) => PIRStage(s, isWrite = true) }

          val remoteMemory = readerCU.srams.find{_.name == quote(mem)}
          if (remoteMemory.isDefined) {
            remoteMemory.get.writeAddr = addr
            remoteMemory.get.writer = Some(cu)
          }
          else
            readerCU.srams ::= PIRMemory(quote(mem), memSize(mem), writer = Some(cu), writeAddr=addr)
        }
        addrComputation // Readers can include this pipe (primarily for accumulators)
      }
    }

    val localCompute = stms filter{case stm@TP(s,d) => !isAllocation(s) && !writeAddrStms.contains(stm) }

    cu.stages ++= localCompute.map{case TP(s,d) => PIRStage(s, isReduce = reduceType(s).isDefined) }

    // -- Define read memories
    val localReaders = stages.filter{case LocalReader(_) => true; case _ => false}
    val localMemories = localReaders.foreach{case LocalReader(readers) =>
      readers.foreach{ case (mem,addr) if isBuffer(mem) =>
        val localMemory = cu.srams.find{_.name == quote(mem)}
        if (localMemory.isDefined)
          localMemory.get.readAddr = addr
        else
          cu.srams ::= PIRMemory(quote(mem), memSize(mem), readAddr = addr)
      }
    }
  }

  def addIterators(cu: PIRCompute, cc: Exp[CounterChain], inds: List[List[Exp[Index]]]) {
    val cchain = cu.cchains.find(_.name == quote(cc)).get
    inds.zipWithIndex.foreach{case (indSet, i) =>
      indSet.foreach{ind => cu.iterators += ind -> (cchain, i) }
    }
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case ParPipeForeach(cc, func, inds) =>
      val cu = allocateBasicCU(lhs)
      addIterators(cu, cc, inds)

      if (isInnerControl(lhs)) scheduleStages(lhs, func)
      else traverseBlock(func)

    case ParPipeReduce(cc, accum, func, rFunc, inds, acc, rV) =>
      val cu = allocateBasicCU(lhs)
      addIterators(cu, cc, inds)

      if (isInnerControl(lhs)) scheduleStages(lhs, func)
      else traverseBlock(func)

    case Unit_pipe(func) =>
      val cu = allocateBasicCU(lhs)
      if (isInnerControl(lhs)) scheduleStages(lhs, func)
      else traverseBlock(func)

    // NOTE: Need to generate offset calculation as a codegen hack right now.
    case Offchip_load_cmd(mem,stream,ofs,len,p) =>
      val cu = allocateMemoryCU(lhs, mem)
      cu.cchains ++= List(CounterChainInstance(quote(lhs)+"_cc", List(PIRCounter(quote(lhs)+"_ctr",Const(0),len,Const(1),p))))
      stageError("Offchip loading in PIR is not yet fully supported")

    case Offchip_store_cmd(mem,stream,ofs,len,p) =>
      val cu = allocateMemoryCU(lhs, mem)
      cu.cchains ++= List(CounterChainInstance(quote(lhs)+"_cc", List(PIRCounter(quote(lhs)+"_ctr",Const(0),len,Const(1),p))))
      stageError("Offchip storing in PIR is not yet fully supported")

    case _ => super.traverse(lhs, rhs)
  }
}
