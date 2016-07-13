package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import scala.collection.mutable.{HashSet,HashMap}

trait ControlSignalAnalysisExp extends PipeStageToolsExp {this: DHDLExp => }
trait UnrolledControlSignalAnalysisExp extends ControlSignalAnalysisExp {this: DHDLExp => }

// TODO: This analyzer has a somewhat different pattern of "do x for all nodes" prior to the usual pattern matching traversal rules
// This is a small issue with existing traversal API, as traverse calls itself to eat reflect nodes
// Can this be generalized and added to the standard traversal API? Currently mimicking EatReflect traversal + some extra stuff
// (Probably want to put EatReflect in traverseStm in Traversal instead? But then to add exceptions to this behavior need to override traverseStm and traverse...)

// (1)  Sets parent control nodes of local memories
// (2)  Sets parent control nodes of controllers
// (3)  Sets children control nodes of controllers
// (4)  Sets reader control nodes of locally read memories
// (5)  Sets writer control nodes of locally written memories
// (6)  Flags accumulators
// (7)  Records list of local memories
// (8)  Records set of metapipes
// (9)  Set parallelization factors of memory readers and writers
// (10) Sets written set of controllers
// (11) Determines the top controller
trait ControlSignalAnalyzer extends Traversal with PipeStageTools {
  val IR: DHDLExp with ControlSignalAnalysisExp
  import IR._

  override val name = "Control Signal Analyzer"
  debugMode = true

  var level = 0
  var indsOwners: Map[Exp[Any], (Exp[Any], Boolean, Int)] = Map.empty // Used to identify the top-most controller for a given address
  var controller: Option[(Exp[Any], Boolean)] = None
  var pendingReads = List[Exp[Any]]()     // Memory reads outside of inner pipes (maintained like a stack to minimize number of "live" reads to check)

  //var indexMap: Map[Exp[Any], (Param[Int], Int)] = Map.empty

  var localMems = List[Exp[Any]]()    // Local memories
  var metapipes = List[Exp[Any]]()    // List of metapipes which can be sequentialized
  var top: Exp[Any] = null

  //val memAccessFactors = HashMap[Exp[Any],List[Param[Int]]]() // Parallelization factors for memory readers/writers

  // HACK: During preprocessing, clear out metadata that we append to in order to get rid of stale symbols
  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    localMems = Nil
    metapipes = Nil
    top = null
    controller = None
    for ((s,p) <- metadata) {
      if (meta[MReaders](s).isDefined) readersOf(s) = Nil
      if (meta[MWritten](s).isDefined) writtenIn(s) = Nil
      if (meta[MWriters](s).isDefined) writersOf(s) = Nil
      if (meta[MChildren](s).isDefined) childrenOf(s) = Nil
    }
    b
  }

  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
    for (mem <- localMems) {
      if (writersOf(mem).isEmpty && !isArgIn(mem)) stageWarn("Memory " + nameOf(mem).getOrElse(s"$mem") + " defined here has no writer")(mpos(mem.pos))
      if (readersOf(mem).isEmpty && !isArgOut(mem)) stageWarn("Memory " + nameOf(mem).getOrElse(s"$mem") + " defined here has no readers")(mpos(mem.pos))
    }
    b
  }

  def traverseWith(owner: Exp[Any], isReduce: Boolean)(b: Block[Any]) {
    level += 1
    val prevCtrl = controller
    val prevReads = pendingReads

    controller = Some((owner,isReduce))
    traverseBlock(b)

    controller = prevCtrl
    pendingReads = prevReads
    level -= 1
  }

  def traverseWith(owner: Exp[Any], isReduce: Boolean, inds: List[Exp[Any]], cc: Rep[CounterChain])(b: Block[Any]) {
    val prevOwners = indsOwners
    //val prevIdxMap = indexMap
    inds.foreach{ind => indsOwners += ind -> (owner, isReduce, level) }

    // HACK: Currently only parallelize the innermost loop index
    //val pars = parParamsOf(cc)
    //if (!inds.isEmpty) { indexMap += inds.last -> (pars.last, level) }
    traverseWith(owner, isReduce)(b)

    indsOwners = prevOwners
    //indexMap = prevIdxMap
  }

  private def dfs(frontier: List[Exp[Any]]): List[Exp[Any]] = {
    frontier.flatMap{
      case Def(d) => dfs(syms(d))
      case bnd => List(bnd)
    }
  }

  def getTopController(ctrl: Exp[Any], isReduce: Boolean, addr: Exp[Any]): (Exp[Any], Boolean) = {
    val bnds = dfs(List(addr))
    val top = bnds.flatMap{d => indsOwners.get(d)}.fold((ctrl,isReduce,level+1)){(a,b) => if (a._3 < b._3) a else b}
    (top._1, top._2)
  }

  // FIXME
  /*def getAddressParFactor(addr: Exp[Any]): Option[Param[Int]] = {
    val bnds = dfs(List(addr))
    // Get all indices in the innermost loop
    val pars = bnds.flatMap{i => indexMap.get(i) }.filter{ _._2 == level }.map(_._1)
    // HACK: Only parallelize the innermost loop index right now
    if (pars.isEmpty) None else Some(pars.last)
  }
  def addAccessFactor(mem: Exp[Any], addr: Exp[Any]) {
    val par = getAddressParFactor(addr)
    par.foreach{ p => addAccessParam(mem, p) }
  }
  def addAccessParam(mem: Exp[Any], p: Param[Int]) {
    if (memAccessFactors.contains(mem)) memAccessFactors(mem) = memAccessFactors(mem) :+ p
    else memAccessFactors(mem) = List(p)
  }*/

  /**
   * In general, we assume that primitive nodes are wrapped within controllers for scheduling purposes
   * (The Unit Pipe Transformer inserts these wrappers for users to simplify programs)
   * Here, however, we relax this assumption for memory reads to two cases:
   *  1. The memory read is in an inner pipe: we use the inner pipe as the "Reader" controller
   *  2. The memory read is in an outer pipe: we use all consumers of this read as "Reader" controllers
   *
   * Note that this is required primarily for register reads, which cannot always be wrapped.
   **/
  def appendReader(ctrl: Exp[Any], isReduce: Boolean, reader: Exp[Any]) = reader match {
    case LocalReader(reads) => reads.foreach{ case (mem,addr) =>
      val top = addr.map{a => getTopController(ctrl, isReduce, a)}.getOrElse( (ctrl,isReduce) )
      readersOf(mem) = readersOf(mem) :+ (top._1, top._2, reader)
    }
  }

  def addReader(ctrl: Exp[Any], isReduce: Boolean, reader: Exp[Any]) {
    if (isInnerPipe((ctrl,isReduce)))
      appendReader(ctrl, isReduce, reader)
    else {
      pendingReads = pendingReads :+ reader
      debug(s"Added pending reader: $reader")
    }
  }

  /**
   * Primitive writers are currently only allowed in inner pipes
   **/
  def checkMultipleWriters(mem: Exp[Any], writer: Exp[Any]) {
    // TODO: Multiple writers should be allowed for memory templates which have buffering support
    if (writersOf(mem).nonEmpty) {
      stageError("Memory " + nameOf(mem).getOrElse("") + " defined here has multiple writers.")(mpos(mem.pos))
    }
  }

  def appendWriter(ctrl: Exp[Any], isReduce: Boolean, writer: Exp[Any]) = writer match {
    case LocalWriter(writes) => writes.foreach{ case (mem,value,addr) =>
      checkMultipleWriters(mem, writer)
      val top = addr.map{a => getTopController(ctrl, isReduce, a)}.getOrElse( (ctrl, isReduce) )
      writersOf(mem) = writersOf(mem) :+ (top._1, top._2, writer)        // (5)
      writtenIn(ctrl) = writtenIn(ctrl) :+ mem                           // (10)

      // This memory is set as an accumulator if it's written value depends on the memory (some read node)
      value.foreach{input => isAccum(mem) = hasDependency(input, mem) }  // (6)
    }
  }

  def addWriter(ctrl: Exp[Any], isReduce: Boolean, writer: Exp[Any]) {
    if (isInnerPipe((ctrl,isReduce)))
      appendWriter(ctrl, isReduce, writer)
    else
      stageError("Memory writers is defined outside inner pipe.")(mpos(writer.pos))
  }

  /**
   * Allocations
   **/
  def addAllocation(ctrl: Exp[Any], alloc: Exp[Any]) {
    parentOf(alloc) = ctrl // (1)

    if (isLocalMemory(alloc)) localMems ::= alloc // (7)
  }

  /**
   * Children/Parent Controllers
   **/
  def addChild(ctrl: Exp[Any], child: Exp[Any]) {
    parentOf(child) = ctrl                        // (2)
    childrenOf(ctrl) = childrenOf(ctrl) :+ child  // (3)
  }

  def addMemoryOps(lhs: Exp[Any], rhs: Def[Any]) {
    if (!isOuterControl(lhs) && controller.isDefined) {
      // Add pending readers
      val ctrl = controller.get
      val deps = readSyms(rhs)
      val parent = if (isControlNode(lhs)) (lhs,false) else ctrl

      if (pendingReads.nonEmpty) {
        debug(s"$lhs = $rhs")
        debug(deps.mkString(", "))
      }

      pendingReads.filter(deps contains _ ).foreach{reader =>
        appendReader(parent._1, parent._2, reader)
        debug(s"Found dep on pending reader $reader")
      }

      if (isAllocation(lhs)) addAllocation(parent._1, lhs)        // (1,7)
      if (isReader(lhs)) addReader(parent._1, parent._2, lhs)     // (4)
      if (isWriter(lhs)) addWriter(parent._1, parent._2, lhs)     // (5,6,10)
    }

    if (isControlNode(lhs)) {
      if (controller.isDefined) addChild(controller.get._1, lhs)  // (2,3)
      else top = lhs                                              // (11)

      if (isMetaPipe(lhs)) metapipes ::= lhs                      // (8)
    }
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    addMemoryOps(lhs, rhs)

    // Hack: usually would use EatReflect here
    rhs match {
      case Reflect(inner,_,_) => analyze(lhs, inner)
      case _ => analyze(lhs, rhs)
    }
  }

  def analyze(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Hwblock(blk)       => traverseWith(lhs, false)(blk)
    case Pipe_parallel(blk) => traverseWith(lhs, false)(blk)
    case Unit_pipe(blk)     => traverseWith(lhs, false)(blk)

    case Pipe_foreach(cc,func,inds) =>
      traverseWith(lhs, false, inds, cc)(func)

    case Pipe_fold(cc,a,zero,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) =>
      val isOuter = !isInnerPipe(lhs)
      traverseWith(lhs, false, inds, cc)(func)
      traverseWith(lhs, isOuter, inds, cc)(rFunc)

      // TODO: Can these be generalized too?
      checkMultipleWriters(a, lhs)
      readersOf(a) = readersOf(a) :+ (lhs, isOuter, lhs)  // (4)
      writersOf(a) = writersOf(a) :+ (lhs, isOuter, lhs)  // (5)
      isAccum(a) = true                                   // (6)
      writtenIn(lhs) = writtenIn(lhs) :+ a                // (10)
      parentOf(a) = lhs  // Reset accumulator with reduction

    case Accum_fold(cc1,cc2,a,zero,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV) =>
      traverseWith(lhs, false, inds1, cc1)(func)
      traverseWith(lhs, true,  inds2, cc2)(rFunc)

      val partial = getBlockResult(func)
      readersOf(partial) = readersOf(partial) :+ (lhs,true,lhs) // (4)

      checkMultipleWriters(a, lhs)
      readersOf(a) = readersOf(a) :+ (lhs, true, lhs)     // (4)
      writersOf(a) = writersOf(a) :+ (lhs, true, lhs)     // (5)
      isAccum(a) = true                                   // (6)
      writtenIn(lhs) = writtenIn(lhs) :+ a                // (10)
      parentOf(a) = lhs  // Reset accumulator with reduction, not allocation

    case _ => blocks(rhs).foreach{blk => traverseBlock(blk)}
  }
}



// --- Control analysis on unrolled IR
// Expanded version of control signal analysis on the unrolled IR
trait UnrolledControlSignalAnalyzer extends ControlSignalAnalyzer {
  val IR: DHDLExp with ControlSignalAnalysisExp
  import IR._

  // All cases of multiple writers are ok now (we banked for unrolled writes already)
  override def checkMultipleWriters(mem: Exp[Any], writer: Exp[Any]) { }

  def traverseUnrolled(owner: Exp[Any], isReduce: Boolean, inds: List[List[Exp[Any]]], cc: Rep[CounterChain])(b: Block[Any]) {
    val prevOwners = indsOwners
    inds.foreach{indSet => indSet.foreach{ind => indsOwners += ind -> (owner, isReduce, level) }}

    traverseWith(owner, isReduce)(b)

    indsOwners = prevOwners
  }

  override def analyze(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case ParPipeForeach(cc,func,inds) =>
      traverseUnrolled(lhs, false, inds, cc)(func)

    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) =>
      traverseUnrolled(lhs, false, inds, cc)(func)
      // rFunc isn't "real" anymore
      readersOf(accum) = readersOf(accum) ++ readersOf(acc) // (4)
      writersOf(accum) = writersOf(accum) ++ writersOf(acc) // (5)
      isAccum(accum) = true                                 // (6)
      writtenIn(lhs) = writtenIn(lhs) :+ accum              // (10)
      parentOf(accum) = lhs  // Reset accumulator with reduction, not allocation

    case _ => super.analyze(lhs, rhs)
  }

}
