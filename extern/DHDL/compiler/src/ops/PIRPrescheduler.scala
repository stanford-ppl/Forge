package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

// For bound symbols
trait SubstQuotingExp extends QuotingExp {
  import IR._
  val subst = HashMap[Exp[Any], Exp[Any]]()
  override def quote(x: Exp[Any]) = super.quote(subst.getOrElse(x, x))
}

trait PIRScheduleAnalyzer extends Traversal with SpatialTraversalTools with SubstQuotingExp {
  val IR: DHDLExp with PIRScheduleAnalysisExp
  import IR._

  override val name = "PIR Preschedule Analysis"
  override val eatReflect = true
  debugMode = true

  // --- State
  // TODO: Is there always only one CU per pipe?
  var top: Option[Exp[Any]] = None
  var pipes = List[Exp[Any]]()
  val cuMapping = HashMap[Exp[Any], ComputeUnit]()
  val globals = HashSet[GlobalMem]()

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    top = None
    pipes = Nil
    cuMapping.clear()
    globals.clear()
    b
  }

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

  def copyIterators(destCU: ComputeUnit, srcCU: ComputeUnit) {
    val cchainCopies = srcCU.cchains.map{
      case cc@CounterChainCopy(name, owner) => cc -> cc
      case cc@CounterChainInstance(name, ctrs) => cc -> CounterChainCopy(name, srcCU)
    }
    val cchainMapping = Map[PIRCounterChain,PIRCounterChain](cchainCopies.toList:_*)
    destCU.cchains ++= cchainCopies.map(_._2)

    val iteratorCopies = srcCU.iterators.toList.map{
      case (iter,(cchain,ctrIdx)) => iter -> ((cchainMapping(cchain), ctrIdx))
    }
    destCU.iterators ++= iteratorCopies
  }
  def allocateCChains(cu: ComputeUnit, pipe: Exp[Any]) = {
    parentOfHack(pipe).foreach{parent => copyIterators(cu, allocateCU(parent)) }

    val ccs = deps(pipe).flatMap {
      case cc@Deff(Counterchain_new(ctrs)) =>
        val ctrInsts = ctrs.map{case ctr@Deff(Counter_new(start,end,stride,par)) => PIRCounter(quote(ctr),start,end,stride) }
        Some(CounterChainInstance(quote(cc),ctrInsts))
      case _ => None
    }
    cu.cchains ++= ccs
  }
  def addIterators(cu: ComputeUnit, cc: Exp[CounterChain], inds: List[List[Exp[Index]]]) {
    val cchain = cu.cchains.find(_.name == quote(cc)).get
    inds.zipWithIndex.foreach{case (indSet, i) =>
      indSet.foreach{ind => cu.iterators += ind -> (cchain, i) }
    }
  }

  // --- Compute Units

  def initCU[T<:ComputeUnit](cu: T, pipe: Exp[Any]): T = {
    cuMapping(pipe) = cu
    pipes ::= pipe
    allocateCChains(cu, pipe)
    debug(s"Allocated CU for control node $pipe: $cu")
    cu
  }

  // Get associated CU (or create a new one if not allocated yet)
  def allocateBasicCU(pipe: Exp[Any]): BasicComputeUnit = {
    if (cuMapping.contains(pipe)) cuMapping(pipe).asInstanceOf[BasicComputeUnit]
    else {
      val parent = parentOfHack(pipe).map(cuMapping(_))
      val cu = BasicComputeUnit(quote(pipe), parent, styleOf(pipe))
      initCU(cu, pipe)
      pipe match {
        case Deff(e:ParPipeForeach)     => addIterators(cu, e.cc, e.inds)
        case Deff(e:ParPipeReduce[_,_]) => addIterators(cu, e.cc, e.inds)
        case _ =>
      }
      if (top.isEmpty && parent.isEmpty) top = Some(pipe)

      cu
    }
  }

  def allocateMemoryCU(pipe: Exp[Any], mem: Exp[Any], mode: MemoryMode): TileTransferUnit = {
    if (cuMapping.contains(pipe)) cuMapping(pipe).asInstanceOf[TileTransferUnit]
    else {
      val region = allocateMem(mem).asInstanceOf[Offchip]
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

  // Create a vector with some naming convention for communication to/from a given memory
  def allocateMem(mem: Exp[Any]) = writersOf(mem).headOption match {
    case Some((_,_,writer@Deff(_:Offchip_load_cmd[_]))) =>
      TileTxVector(quote(writer)+".out")

    case writer =>
      val name = quote(mem) + writer.map{writer => "_"+quote(writer._3)}.getOrElse("")
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

  def allocateWrittenSRAM(writer: Exp[Any], mem: Exp[Any], addr: Option[Exp[Any]], writerCU: ComputeUnit, stages: List[PIRStage]) = {
    val vector = allocateMem(mem)

    val isLocallyRead = readersOf(mem).exists{case (ctrl,_,_) =>
      val readerCU = allocateCU(ctrl)
      readerCU.srams.find{_.name == quote(mem)} match {
        case Some(readMem) =>
          readMem.writeAddr = addr
          readMem.vector = Some(vector)
        case None =>
          readerCU.srams ::= PIRMemory(quote(mem), memSize(mem), vector = Some(vector), writeAddr = addr)
      }
      val isLocalRead = readerCU == writerCU
      if (!isLocalRead) {
        readerCU.stages ++= stages
        copyIterators(readerCU, writerCU)
      }
      isLocalRead
    }
    if (!isLocallyRead && !vector.isInstanceOf[TileTxVector])
      writerCU.vectorOut ::= VectorOut(quote(writer), vector)

    isLocallyRead
  }
  def allocateReadSRAM(reader: Exp[Any], mem: Exp[Any], addr: Option[Exp[Any]], readerCU: ComputeUnit) = {
    val vector = allocateMem(mem)

    readerCU.srams.find{_.name == quote(mem)} match {
      case Some(readMem) =>
        if (!readMem.readAddr.isDefined || addr.isDefined)
          readMem.readAddr = addr

      case None =>
        readerCU.srams ::= PIRMemory(quote(mem), memSize(mem), readAddr = addr)
    }
    val isLocallyWritten = writersOf(mem).headOption.map{case (writeCtrl,_,_) => allocateCU(writeCtrl) == readerCU }.getOrElse(true)

    if (!isLocallyWritten && !vector.isInstanceOf[TileTxVector])
      readerCU.vectorIn ::= VectorIn(quote(reader), vector)

    isLocallyWritten
  }

  def foreachSymInBlock(b: Block[Any])(func: Sym[Any] => Unit) {
    focusBlock(b){
      focusExactScope(b){ stms =>
        stms.foreach{ case TP(lhs,rhs) => func(lhs) }
      }
    }
  }

  def prescheduleStages(pipe: Exp[Any], func: Block[Any]) {
    val Def(d) = pipe
    debug(s"Traversing $pipe = $d")

    val stms = getStmsInBlock(func)
    val stages = getStages(func)
    val cu = allocateCU(pipe)
    var remoteStages: List[Exp[Any]] = Nil

    stms.foreach{case TP(lhs, rhs) => debug(s"  $lhs = $rhs")}

    foreachSymInBlock(func){
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
       */
      case writer@LocalWriter(writes) if !isControlNode(writer) =>
        debug(s"local writer: $writer")

        writes.foreach{case (mem, value, addr) =>
          if (isRegister(mem.tp)) {
            if (readersOf(mem).exists(_._1 != pipe) || isArgOut(mem)) {// Read remotely
              val scalar = allocateMem(mem)
              cu.scalarOut ::= ScalarOut(quote(writer), scalar)
            }
          }
          else if (isBuffer(mem)) {
            val addrComputation = addr.map{a => getSchedule(stms)(a,false)}.getOrElse(Nil)
            val addrSyms = addrComputation.map{case TP(s,d) => s} :+ writer
            val addrStages = addrSyms.map{s => DefStage(s, isWrite = true) }

            val isLocallyRead = allocateWrittenSRAM(writer, mem, addr, cu, addrStages)
            if (!isLocallyRead)
              remoteStages :::= addrSyms.filterNot(_ == writer)
          }
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
       */
      case reader@LocalReader(reads) if !isControlNode(reader) =>
        reads.foreach{case (mem,addr) =>
          if (isRegister(mem.tp)) {
            debug(s"local register read: $reader")
            val writeCtrl = writersOf(mem).headOption.map(_._1) // ASSUMPTION: At least one writer

            // Register reads may be used by more than one pipe
            readersOf(mem).filter(_._3 == reader).map(_._1).foreach{readCtrl =>
              val isCurrentPipe = readCtrl == pipe
              val isLocallyWritten = !isArgIn(mem) && writeCtrl.map(_ == readCtrl).getOrElse(true)

              if (!isCurrentPipe || !isLocallyWritten) {
                val readerCU = allocateCU(readCtrl)
                val scalar = allocateMem(mem)
                readerCU.scalarIn ::= ScalarIn(quote(reader),scalar)
              }
            }
            val isLocallyRead = readersOf(mem).exists{case (ctrl,_,read) => read == reader && ctrl == pipe}
            val isLocallyWritten = !isArgIn(mem) && writeCtrl.map(_ == pipe).getOrElse(true)
            debug(s"isLocallyRead: $isLocallyRead, isLocallyWritten: $isLocallyWritten")
            if (!isLocallyWritten || !isLocallyRead || isInnerAccum(mem)) remoteStages ::= reader
          }
          else if (isBuffer(mem)) {
            debug(s"local buffer read: $reader")
            val isLocallyWritten = allocateReadSRAM(reader, mem, addr, cu)
            //if (!isLocallyWritten) remoteStages ::= reader
          }
        }

      case lhs@Def(rhs) =>
        debug(s"other node: $lhs = $rhs")
        traverse(lhs.asInstanceOf[Sym[Any]], rhs)
    }

    val localCompute = stages.filter{s => (isPrimitiveNode(s) || isRegisterRead(s) || isGlobal(s)) && !remoteStages.contains(s) }

    // Sanity check
    if (isOuterControl(pipe) && localCompute.nonEmpty) {
      stageWarn(s"Outer control $pipe has compute stages: ")
      localCompute.foreach{case lhs@Def(rhs) => stageWarn(s"  $lhs = $rhs")}
    }

    cu.stages ++= localCompute.map{s => DefStage(s, isReduce = reduceType(s).isDefined) }
  }



  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Hwblock(func) =>
      val cu = allocateCU(lhs)
      prescheduleStages(lhs, func)

    case ParPipeForeach(cc, func, inds) =>
      val cu = allocateCU(lhs)
      prescheduleStages(lhs, func)

    case ParPipeReduce(cc, accum, func, rFunc, inds, acc, rV) =>
      subst += acc -> accum
      val cu = allocateCU(lhs)
      prescheduleStages(lhs, func)

    case Unit_pipe(func) =>
      val cu = allocateCU(lhs)
      prescheduleStages(lhs, func)

    // NOTE: Need to generate offset calculation as a codegen hack right now.
    case Offchip_load_cmd(mem,stream,ofs,len,p) =>
      debug(s"Traversing $lhs = $rhs")
      val cu = allocateCU(lhs)
      val cc = CounterChainInstance(quote(lhs)+"_cc", List(PIRCounter(quote(lhs)+"_ctr",Const(0),len,Const(1),p)))
      val i = fresh[Index]
      cu.cchains += cc
      cu.iterators += i -> (cc, 0)
      allocateWrittenSRAM(lhs, stream, Some(i), cu, Nil)

      // HACK!! Fake a read addresss for the FIFO stream
      readersOf(stream).foreach{case (ctrl,_,_) =>
        val readerCU = allocateCU(ctrl)
        debug(s"HACK: Adding read address to $stream for CU: ")
        debug(readerCU.dumpString)

        val chain = readerCU.cchains.filter(_.isInstanceOf[CounterChainInstance]).last
        val iters = readerCU.iterators.toList.filter{case (iter,pair) => pair._1 == chain}
        val iter = iters.reduce{(a,b) => if (a._2._2 > b._2._2) a else b}

        val readMem = readerCU.srams.find{_.name == quote(stream)}.get
        readMem.readAddr = Some(iter._1)
      }
      // TODO: Add stages for offset calculation

      //stageError("Offchip loading in PIR is not yet fully supported")

    case Offchip_store_cmd(mem,stream,ofs,len,p) =>
      debug(s"Traversing $lhs = $rhs")
      val cu = allocateCU(lhs)
      val cc = CounterChainInstance(quote(lhs)+"_cc", List(PIRCounter(quote(lhs)+"_ctr",Const(0),len,Const(1),p)))
      val i = fresh[Index]
      cu.cchains += cc
      cu.iterators += i -> (cc, 0)
      allocateReadSRAM(lhs, stream, Some(i), cu)
      //stageError("Offchip storing in PIR is not yet fully supported")

    case _ => super.traverse(lhs, rhs)
  }
}
