package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait PIRScheduleAnalyzer extends Traversal with SpatialTraversalTools with PIRCommon {
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
    if (destCU != srcCU) {
      val cchainCopies = srcCU.cchains.map{
        case cc@CounterChainCopy(name, owner) => cc -> cc
        case cc@CounterChainInstance(name, ctrs) => cc -> CounterChainCopy(name, srcCU)
      }
      val cchainMapping = Map[CUCounterChain,CUCounterChain](cchainCopies.toList:_*)
      destCU.cchains ++= cchainCopies.map(_._2)

      srcCU.iterators.foreach{ case (iter,CounterReg(cchain,idx)) =>
        destCU.addReg(iter, CounterReg(cchainMapping(cchain),idx))
      }
    }
  }
  def addIterators(cu: ComputeUnit, cc: Exp[CounterChain], inds: List[List[Exp[Index]]]) {
    val cchain = cu.cchains.find(_.name == quote(cc)).get
    inds.zipWithIndex.foreach{case (indSet, i) =>
      indSet.foreach{ind => cu.addReg(ind, CounterReg(cchain, i)) }
    }
  }

  def allocateCChains(cu: ComputeUnit, pipe: Exp[Any]) = {
    def allocateCounter(ctr: Exp[Counter], start: Exp[Any], end: Exp[Any], stride: Exp[Any]) = {
      val min = cu.getOrUpdate(start){ allocateLocal(start, pipe) }
      val max = cu.getOrUpdate(end){ allocateLocal(end, pipe) }
      val step = cu.getOrUpdate(stride){ allocateLocal(stride, pipe) }
      CUCounter(quote(ctr), min, max, step)
    }

    parentOfHack(pipe).foreach{parent => copyIterators(cu, allocateCU(parent)) }

    val ccs = deps(pipe).flatMap {
      case cc@Deff(Counterchain_new(ctrs)) =>
        val ctrInsts = ctrs.map{case ctr@Deff(Counter_new(start,end,stride,par)) => allocateCounter(ctr, start, end, stride) }
        Some(CounterChainInstance(quote(cc),ctrInsts))
      case _ => None
    }
    cu.cchains ++= ccs
  }


  // --- Compute Units

  def initCU[T<:ComputeUnit](cu: T, pipe: Exp[Any]): T = {
    cuMapping(pipe) = cu
    pipes ::= pipe
    allocateCChains(cu, pipe)
    debug(s"Allocated CU for control node $pipe: $cu")
    cu
  }

  def controllersHack(pipe: Exp[Any]): List[Exp[Any]] = pipe match {
    case Deff(_:Pipe_parallel) => childrenOf(pipe)
    case _ => List(pipe)
  }

  // TODO: This assumes linear stage order. Switch to more general dataflow graph after parallel is removed
  def pipeDependencies(pipe: Exp[Any]): List[Exp[Any]] = parentOf(pipe) match {
    case Some(parent@Deff(_:Pipe_parallel)) => pipeDependencies(parent)
    case Some(parent) =>
      val childs = childrenOf(parent)
      val idx = childs.indexOf(pipe)
      val deps = if (idx > 0) controllersHack(childs(idx-1)) else Nil
      debug(s"Found deps of $pipe: $deps")
      deps
    case None => Nil
  }

  // Get associated CU (or create a new one if not allocated yet)
  def allocateBasicCU(pipe: Exp[Any]): BasicComputeUnit = {
    if (cuMapping.contains(pipe)) cuMapping(pipe).asInstanceOf[BasicComputeUnit]
    else {
      val deps = pipeDependencies(pipe)
      val parent = parentOfHack(pipe).map(cuMapping(_))
      val cu = BasicComputeUnit(quote(pipe), parent, deps, styleOf(pipe))
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
      val region = allocateGlobal(mem).asInstanceOf[Offchip]
      val mc = MemCtrl(quote(pipe)+"_mc", region, mode)
      globals += mc
      val parent = parentOfHack(pipe).map(cuMapping(_))
      val deps = pipeDependencies(pipe)
      val cu = TileTransferUnit(quote(pipe), parent, deps, mc, mode)
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

  def memSize(mem: Exp[Any]) = mem match {
    case Deff(Bram_new(depth,_)) => bound(depth).get.toInt
    case Deff(Fifo_new(depth,_)) => bound(depth).get.toInt
    case _ => stageError(s"Unknown local memory type $mem")
  }
  def isBuffer(mem: Exp[Any]) = isFIFO(mem.tp) || isBRAM(mem.tp)

  def allocateWrittenSRAM(writer: Exp[Any], mem: Exp[Any], addr: Option[Exp[Any]], writerCU: ComputeUnit, stages: List[PseudoStage]) {
    val vector = allocateGlobal(mem)

    readersOf(mem).foreach{case (ctrl,_,_) =>
      val readerCU = allocateCU(ctrl)
      copyIterators(readerCU, writerCU)

      val addrReg = addr.map{a => readerCU.getOrUpdate(a){ WriteAddrReg(a) } }

      val sram = readerCU.srams.find{_.name == quote(mem)} match {
        case Some(readMem) =>
          readMem.writeAddr = addrReg
          readMem.vector = Some(vector)
          readMem
        case None =>
          val readMem = CUMemory(quote(mem), memSize(mem), vector = Some(vector), writeAddr = addrReg)
          readerCU.srams += readMem
          readMem
      }
      val writeStages = if (!isControlNode(writer) && addr.isDefined) stages ++ List(WriteAddrStage(writer)) else stages

      if (writeStages.nonEmpty) {
        debug(s"Added $sram to $readerCU write stages")
        readerCU.writePseudoStages += sram -> writeStages
      }
    }
  }
  def allocateReadSRAM(reader: Exp[Any], mem: Exp[Any], addr: Option[Exp[Any]], readerCU: ComputeUnit) {
    val vector = allocateGlobal(mem)
    readerCU.srams.find{_.name == quote(mem)} match {
      case Some(readMem) =>
        if (!readMem.readAddr.isDefined && addr.isDefined) {
          val addrReg = readerCU.getOrUpdate(addr.get){ ReadAddrReg(addr.get) }
          readMem.readAddr = Some(addrReg)
        }
      case None =>
        val addrReg = addr.map{a => readerCU.getOrUpdate(a){ ReadAddrReg(a) } }
        readerCU.srams += CUMemory(quote(mem), memSize(mem), readAddr = addrReg)
    }
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
      case writer@LocalWriter(writes) if !isControlNode(writer) =>
        debug(s"local writer: $writer")
        writes.foreach{case (mem, value, addr) =>
          if (isBuffer(mem)) {
            val addrComputation = addr.map{a => getSchedule(stms)(a,false)}.getOrElse(Nil)
            val addrSyms = addrComputation.map{case TP(s,d) => s}
            val addrStages = addrSyms.map{s => DefStage(s) }

            allocateWrittenSRAM(writer, mem, addr, cu, addrStages)
            remoteStages :::= addrSyms
          }
        }

      case reader@LocalReader(reads) if !isControlNode(reader) =>
        reads.foreach{case (mem,addr) =>
          if (isRegister(mem.tp)) {
            debug(s"local register read: $reader")
            // Register reads may be used by more than one pipe
            readersOf(mem).filter(_._3 == reader).map(_._1).foreach{readCtrl =>
              val isCurrentPipe = readCtrl == pipe
              val isLocallyWritten = isWrittenInPipe(mem, readCtrl)

              if (!isCurrentPipe || !isLocallyWritten) {
                val readerCU = allocateCU(readCtrl)
                readerCU.computePseudoStages ++= List(DefStage(reader))
              }
            }
            val isLocallyRead = isReadInPipe(mem, pipe, Some(reader))
            val isLocallyWritten = isWrittenInPipe(mem, pipe)
            debug(s"isLocallyRead: $isLocallyRead, isLocallyWritten: $isLocallyWritten")
            if (!isLocallyWritten || !isLocallyRead || isInnerAccum(mem)) remoteStages ::= reader
          }
          else if (isBuffer(mem)) {
            debug(s"local buffer read: $reader")
            allocateReadSRAM(reader, mem, addr, cu)
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

    cu.computePseudoStages ++= localCompute.map{s => DefStage(s, isReduce = reduceType(s).isDefined) }
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
      val lenIn = cu.getOrUpdate(len){ allocateLocal(len, lhs) }
      val ctr = CUCounter(quote(lhs)+"_ctr",ConstReg("0l"),lenIn,ConstReg("1l"))
      val cc = CounterChainInstance(quote(lhs)+"_cc", List(ctr))
      val i = fresh[Index]
      cu.cchains += cc
      cu.addReg(i, CounterReg(cc, 0))
      allocateWrittenSRAM(lhs, stream, Some(i), cu, Nil)

      // HACK!! Fake a read addresss for the FIFO stream
      readersOf(stream).foreach{case (ctrl,_,_) =>
        val readerCU = allocateCU(ctrl)
        debug(s"HACK: Adding read address to $stream for CU: ")
        debug(readerCU.dumpString)

        val chain = readerCU.cchains.filter(_.isInstanceOf[CounterChainInstance]).last
        val iters = readerCU.iterators.filter{case (iter,reg) => reg.cchain == chain}
        val iter = iters.reduce{(a,b) => if (a._2.idx < b._2.idx) a else b}

        val readMem = readerCU.srams.find{_.name == quote(stream)}.get
        readMem.readAddr = Some(iter._2)
      }
      // TODO: Add stages for offset calculation + offset output scalar write

    case Offchip_store_cmd(mem,stream,ofs,len,p) =>
      debug(s"Traversing $lhs = $rhs")
      val cu = allocateCU(lhs)
      val lenIn = cu.getOrUpdate(len){ allocateLocal(len, lhs) }
      val ctr = CUCounter(quote(lhs)+"_ctr",ConstReg("0l"),lenIn,ConstReg("1l"))
      val cc = CounterChainInstance(quote(lhs)+"_cc", List(ctr))
      val i = fresh[Index]
      cu.cchains += cc
      cu.addReg(i, CounterReg(cc, 0))
      allocateReadSRAM(lhs, stream, Some(i), cu)
      // TODO: Add stages for offset calculation + offset output scalar write

    case _ => super.traverse(lhs, rhs)
  }
}
