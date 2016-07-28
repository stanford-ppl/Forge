package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait SymbolCollector extends Traversal {
  val IR: DHDLExp
  import IR._
  override val recurse = Always
  var constants: List[Exp[Any]] = Nil

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = {
    constants :::= (rhs match {
      case p: Product =>
        p.productIterator.toList.flatMap{
          case e: Exp[_] => e match {case Exact(_) => Some(e); case _ => None}
          case _ => None
        }
      case _ => Nil
    })
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
  var top: Option[Exp[Any]] = None
  var pipes = List[Exp[Any]]()
  val cuMapping = HashMap[Exp[Any], ComputeUnit]()
  val globals = HashSet[CommMem]()

  // HACK: Skip parallel pipes in PIR gen
  def parentOfHack(x: Exp[Any]): Option[Exp[Any]] = parentOf(x) match {
    case Some(pipe@Deff(Pipe_parallel(_))) => parentOfHack(pipe)
    case parentOpt => parentOpt
  }

  // --- CounterChains
  def deps(x: Exp[Any]) = x match {
    case Def(rhs) => syms(rhs).distinct
    case _ => Nil
  }

  def copyParentCChains(x: Option[Exp[Any]]): List[CounterChainCopy] = x match {
    case Some(ctrl) =>
      val cu = cuMapping(ctrl)
      deps(ctrl).flatMap {
        case cc@Deff(Counterchain_new) => Some(CounterChainCopy(quote(cc), cu))
        case _ => None
      } ++ copyParentCChains(parentOfHack(ctrl))
    case None => Nil
  }
  def allocateCChains(pipe: Exp[Any]): List[PIRCounterChain] = {
    debug(s"Deps of $pipe = ${deps(pipe)}")
    deps(pipe).flatMap {
      case cc@Deff(Counterchain_new(ctrs,nIter)) =>
        val ctrInsts = ctrs.map{case ctr@Deff(Counter_new(start,end,stride,par)) => PIRCounter(quote(ctr),start,end,stride,par) }
        Some(CounterChainInstance(quote(cc),ctrInsts))
      case _ => None
    } ++ copyParentCChains(parentOfHack(pipe))
  }

  // --- Compute Units

  def initCU[T<:ComputeUnit](cu: T, pipe: Exp[Any]): T = {
    cuMapping(pipe) = cu
    pipes ::= pipe
    cu.cchains ++= allocateCChains(pipe)
    cu.iterators ++= parentOfHack(pipe).map{parent => cuMapping(parent).iterators.toList}.getOrElse(Nil)
    cu
  }

  // Get associated CU (or create a new one if not allocated yet)
  def allocateBasicCU(pipe: Exp[Any]): BasicComputeUnit = {
    if (cuMapping.contains(pipe)) cuMapping(pipe).asInstanceOf[BasicComputeUnit]
    else {
      if (top.isEmpty && parentOfHack(pipe).isEmpty) top = Some(pipe)

      val parent = parentOfHack(pipe).map(cuMapping(_))
      val cu = BasicComputeUnit(quote(pipe), parent, styleOf(pipe))
      initCU(cu, pipe)
    }
  }

  def allocateMemoryCU(pipe: Exp[Any], mem: Exp[Any], mode: MemoryMode): TileTransferUnit = {
    if (cuMapping.contains(pipe)) cuMapping(pipe).asInstanceOf[TileTransferUnit]
    else {
      val region = allocateRemoteMem(mem).asInstanceOf[Offchip]
      val mc = MemCtrl(quote(pipe)+"_mc", region, mode)
      globals += mc
      val parent = parentOfHack(pipe).map(cuMapping(_))
      val cu = TileTransferUnit(quote(pipe), parent, mc, mode)
      initCU(cu, pipe)
    }
  }

  /**
   * Get or create a CU which corresponds to the given pipe
   **/
  def allocateCU(pipe: Exp[Any]) = pipe match {
    case Deff(_:Hwblock)              => allocateBasicCU(pipe)
    case Deff(_:ParPipeForeach)       => allocateBasicCU(pipe)
    case Deff(_:ParPipeReduce[_,_])   => allocateBasicCU(pipe)
    case Deff(_:Unit_pipe)            => allocateBasicCU(pipe)
    case Deff(e:Offchip_load_cmd[_])  => allocateMemoryCU(pipe, e.mem, MemLoad)
    case Deff(e:Offchip_store_cmd[_]) => allocateMemoryCU(pipe, e.mem, MemStore)
  }
  def allocateRemoteMem(mem: Exp[Any]) = {
    val writer = writersOf(mem).headOption.map{writer => "_"+quote(writer._3)}.getOrElse("")
    val name = quote(mem) + writer
    val comm = mem match {
      case Deff(Offchip_new(_)) => Offchip(name)
      case Deff(Argin_new(_))   => InputArg(name)
      case Deff(Argout_new(_))  => OutputArg(name)
      case Deff(Reg_new(_))     => ScalarMem(name)
      case _                    => VectorMem(name)
    }
    globals += comm
    comm
  }

  def memSize(mem: Exp[Any]) = mem match {
    case Deff(Bram_new(depth,_)) => bound(depth).get.toInt
    case Deff(Fifo_new(depth,_)) => bound(depth).get.toInt
    case _ => stageError(s"Disallowed local memory $mem")
  }
  def isBuffer(mem: Exp[Any]) = isFIFO(mem.tp) || isBRAM(mem.tp)

  def allocateWrittenSRAM(writer: Exp[Any], mem: Exp[Any], addr: Option[Exp[Any]], writerCU: ComputeUnit, stages: List[PIRStage]): Boolean = {
    val isLocallyRead = readersOf(mem).exists{case (ctrl,_,_) =>
      val readerCU = allocateCU(ctrl)
      readerCU.srams.find{_.name == quote(mem)} match {
        case Some(readMem) =>
          readMem.writeAddr = addr
          readMem.writer = Some(writerCU)
        case None =>
          readerCU.srams ::= PIRMemory(quote(mem), memSize(mem), writer = Some(writerCU), writeAddr = addr)
      }
      val isLocalRead = readerCU == writerCU
      if (!isLocalRead) readerCU.stages ++= stages
      isLocalRead
    }
    if (!isLocallyRead) {
      val vector = allocateRemoteMem(mem)
      writerCU.vectorOut ::= VectorOut(quote(writer), vector)
    }
    isLocallyRead
  }
  def allocateReadSRAM(reader: Exp[Any], mem: Exp[Any], addr: Option[Exp[Any]], readerCU: ComputeUnit): Boolean = {
    readerCU.srams.find{_.name == quote(mem)} match {
      case Some(readMem) =>
        readMem.readAddr = addr
      case None =>
        readerCU.srams ::= PIRMemory(quote(mem), memSize(mem), readAddr = addr)
    }
    val isLocallyWritten = writersOf(mem).headOption.map{case (writeCtrl,_,_) => allocateCU(writeCtrl) == readerCU }.getOrElse(true)

    if (!isLocallyWritten) {
      val vector = allocateRemoteMem(mem)
      readerCU.vectorIn ::= VectorIn(quote(reader), vector)
    }
    isLocallyWritten
  }

  def scheduleStages(pipe: Exp[Any], func: Block[Any]) = {
    val stms = getStmsInBlock(func)
    val stages = getStages(func)
    val cu = allocateCU(pipe)

    val remoteMemStages = stages.flatMap{
      // NOTE: Writers always appear to occur in the associated writer controller
      // However, register reads may appear outside their corresponding controller
      /*
        For each memory written in this pipe:
          If the memory holds a scalar (i.e. register):
            If this memory is read remotely:
              record memory as scalar output
          Else if this memory is a buffer (i.e. BRAM, FIFO):
            For each reader of this memory:
              create an instance of this memory for that reader if it does not already exist
              set the write address of the memory copy
              If the reader is remote:
                add the address computation stages for this write to the reader's stages
            If the memory is not read locally:
              add the memory to the list of vector outputs
              remove address calculation nodes from the list of stages to be scheduled
          Else:
            [Nothing for now]
       */
      case writer@LocalWriter(writes) => writes.flatMap{
        case (mem,value,addr) if isRegister(mem.tp) =>
          if (readersOf(mem).exists(_._1 != pipe)) {// Read remotely
            val scalar = allocateRemoteMem(mem)
            cu.scalarOut ::= ScalarOut(quote(writer), scalar)
          }
          Nil
        case (mem,value,addr) if isBuffer(mem) =>
          val addrComputation = getSchedule(stms)(addr,false).map{case TP(s,d) => s}
          val stages = addrComputation.map{s => DefStage(s, isWrite = true) }

          val isLocallyRead = allocateWrittenSRAM(writer, mem, addr, cu, stages)

          if (isLocallyRead) stages.filterNot(_ == writer)
          else Nil

        case _ => Nil
      }
      /*
        For each memory [appearing to be] read in this pipe:
          If the memory holds a scalar:
            If the memory is not locally read or not locally written:
              add the reader to the list of scalar inputs
              remove the reader from the list of stages to be scheduled
          Else if the memory is a buffer (BRAM or FIFO):
            create or get the SRAM which corresponds to this memory
            set the read address for this SRAM
            If the memory is not locally written:
              add the memory to the list of vector inputs
              remove the reader from the list of stages to be scheduled
          Else:
            [Nothing]
       */
      case reader@LocalReader(reads) => reads.flatMap{case (mem,addr) =>
        if (isRegister(mem.tp)) {
          val readCtrl = readersOf(mem).find(_._3 == reader).get._1
          val writeCtrl = writersOf(mem).headOption.map(_._1) // ASSUMPTION: At least one writer
          val isLocallyRead = readCtrl == pipe
          val isLocallyWritten = writeCtrl.map(_ == pipe).getOrElse(true)
          val readerCU = allocateCU(readCtrl)

          if (!isLocallyRead || !isLocallyWritten) {
            val scalar = allocateRemoteMem(mem)
            readerCU.scalarIn ::= ScalarIn(quote(reader),scalar)
            List(reader)
          }
          else Nil
        }
        else if (isBuffer(mem)) {
          val isLocallyWritten = allocateReadSRAM(reader, mem, addr, cu)
          if (isLocallyWritten) Nil else List(reader)
        }
        else Nil
      }
      case _ => Nil
    }

    val localCompute = stms filter{case stm@TP(s,d) => !isAllocation(s) && !remoteMemStages.contains(s) }

    cu.stages ++= localCompute.map{case TP(s,d) => DefStage(s, isReduce = reduceType(s).isDefined) }
  }

  def addIterators(cu: ComputeUnit, cc: Exp[CounterChain], inds: List[List[Exp[Index]]]) {
    debug(s"Adding iterators for cu: ${cu.dumpString}")
    debug(s"Counterchain: $cc, inds: $inds")
    val cchain = cu.cchains.find(_.name == quote(cc)).get
    inds.zipWithIndex.foreach{case (indSet, i) =>
      indSet.foreach{ind => cu.iterators += ind -> (cchain, i) }
    }
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Hwblock(func) =>
      val cu = allocateBasicCU(lhs)
      if (isInnerControl(lhs)) scheduleStages(lhs, func)
      else traverseBlock(func)

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
      val cu = allocateCU(lhs)
      val cc = CounterChainInstance(quote(lhs)+"_cc", List(PIRCounter(quote(lhs)+"_ctr",Const(0),len,Const(1),p)))
      val i = fresh[Index]
      cu.cchains ++= List(cc)
      readersOf(stream).foreach{case (readCtrl,_,reader) =>
        val readCU = allocateCU(readCtrl)
        readCU.cchains ++= List(cc)
        readCU.iterators += i -> (cc, 0)
      }
      //allocateWrittenSRAM(lhs, stream, i, cu, stages)

      //stageError("Offchip loading in PIR is not yet fully supported")

    case Offchip_store_cmd(mem,stream,ofs,len,p) =>
      val cu = allocateCU(lhs)
      cu.cchains ++= List(CounterChainInstance(quote(lhs)+"_cc", List(PIRCounter(quote(lhs)+"_ctr",Const(0),len,Const(1),p))))
      //stageError("Offchip storing in PIR is not yet fully supported")

    case _ => super.traverse(lhs, rhs)
  }
}
