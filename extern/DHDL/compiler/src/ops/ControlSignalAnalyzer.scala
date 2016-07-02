package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.analysis.HungryTraversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import scala.collection.mutable.{HashSet,HashMap}

trait ControlSignalAnalysisExp extends PipeStageToolsExp {this: DHDLExp => }
trait UnrolledControlSignalAnalysisExp extends ControlSignalAnalysisExp {this: DHDLExp => }

// (1)  Sets parent control nodes of local memories
// (2)  Sets parent control nodes of controllers
// (3)  Sets children control nodes of controllers
// (4)  Sets reader control nodes of locally read memories
// (5)  Sets writer control nodes of locally written memories
// (6)  Flags accumulators
// (7)  Records set of double buffer candidates (registers and BRAMs)
// (8)  Records set of metapipes
// (9)  Set parallelization factors of memory readers and writers
// (10) Sets written set of controllers
// (11) Determines the top controller
trait ControlSignalAnalyzer extends HungryTraversal with PipeStageTools {
  val IR: DHDLExp with ControlSignalAnalysisExp
  import IR._

  override val name = "Control Signal Analyzer"

  var level = 0
  var indsOwners: Map[Exp[Any], (Exp[Any], Boolean, Int)] = Map.empty

  var indexMap: Map[Exp[Any], (Param[Int], Int)] = Map.empty

  var localMems = List[Exp[Any]]()    // Double buffer candidates
  var metapipes = List[Exp[Any]]()    // List of metapipes which can be sequentialized
  var top: Exp[Any] = null

  val memAccessFactors = HashMap[Exp[Any],List[Param[Int]]]() // Parallelization factors for memory readers/writers

  // HACK: During preprocessing, clear out metadata that we append to in order to get rid of stale symbols
  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    for ((s,p) <- metadata) {
      if (meta[MReaders](s).isDefined && !isDelayReg(s)) readersOf(s) = Nil
      if (meta[MWritten](s).isDefined) writtenIn(s) = Nil
      if (meta[MWriters](s).isDefined) writersOf(s) = Nil
    }
    b
  }

  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
    for (mem <- localMems) {
      if (writersOf(mem).isEmpty && !isArgIn(mem)) stageWarn("Memory " + nameOf(mem).getOrElse("") + " defined here has no writer")(mpos(mem.pos))
      if (readersOf(mem).isEmpty && !isArgOut(mem)) stageWarn("Memory " + nameOf(mem).getOrElse("") + " defined here has no readers")(mpos(mem.pos))
    }
    b
  }

  def traverseWith(owner: Exp[Any], isReduce: Boolean, inds: List[Exp[Any]], cc: Rep[CounterChain])(b: Block[Any]) = {
    level += 1
    val prevOwners = indsOwners
    val prevIdxMap = indexMap
    inds.foreach{ind => indsOwners += ind -> (owner, isReduce, level) }

    // HACK: Currently only parallelize the innermost loop index
    val pars = parParamsOf(cc)
    if (!inds.isEmpty) { indexMap += inds.last -> (pars.last, level) }

    traverseBlock(b)
    indsOwners = prevOwners
    indexMap = prevIdxMap
    level -= 1
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
  def getAddressParFactor(addr: Exp[Any]): Option[Param[Int]] = {
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
  }


  def setReaders(ctrl: Exp[Any], isReduce: Boolean, blks: Block[Any]*) {
    val reads = getLocalReaders(blks:_*)
    reads.foreach{
      case reader@Def(EatReflect(Reg_read(reg))) =>
        if (!isDelayReg(reg)) readersOf(reg) = readersOf(reg) :+ (ctrl,isReduce,reader)

      case reader@Def(EatReflect(Bram_load(ram,addr))) =>
        val top = getTopController(ctrl, isReduce, addr)
        readersOf(ram) = readersOf(ram) :+ (top._1,top._2,reader)
    }
  }

  def checkMultipleWriters(mem: Exp[Any], writer: Exp[Any]) {
    if (writersOf(mem).nonEmpty) {
      // This should be allowed for buffered types, e.g. the Hogwild buffer discussed for SGD
      stageError("Memory " + nameOf(mem).getOrElse("") + " defined here has multiple writers.")(mpos(mem.pos))
    }
  }

  def setWriters(ctrl: Exp[Any], isReduce: Boolean, blks: Block[Any]*) {
    val writes = getLocalWriters(blks:_*)
    writes.foreach{
      case writer@Def(EatReflect(Reg_write(reg,_))) =>
        if (!isDelayReg(reg)) {
          checkMultipleWriters(reg, writer)
          writersOf(reg) = writersOf(reg) :+ (ctrl, isReduce, writer)
          writtenIn(ctrl) = writtenIn(ctrl) :+ reg  // (10)
        }

      case writer@Def(EatReflect(Bram_store(ram,addr,_))) =>
        checkMultipleWriters(ram, writer)
        val top = getTopController(ctrl, isReduce, addr)
        writersOf(ram) = writersOf(ram) :+ (top._1,top._2,writer)
        writtenIn(ctrl) = writtenIn(ctrl) :+ ram    // (10)
    }
  }

  // Checks to see if lhs is dependent on rhs (used for checking for accum. cycles)
  def hasDependency(lhs: Exp[Any], rhs: Exp[Any]): Boolean = {
    def dfs(frontier: List[Exp[Any]]): Boolean = {
      frontier.map {
        case s if s == rhs => true
        case Def(d) => dfs(syms(d))
        case _ => false
      }.fold(false){_||_}
    }

    lhs match {
      case Def(d) => dfs(syms(d))
      case _ => false
    }
  }


  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Offchip_load_vector(mem,ofs,len) =>   // ?
    case Offchip_store_vector(mem,ofs,vec) =>  // ?

    case Hwblock(blk) =>
      val allocs = getAllocations(blk)
      val stages = getControlNodes(blk)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      stages.foreach{s => parentOf(s) = lhs } // (2)
      childrenOf(lhs) = stages                // (3)

      // TODO: Are these necessary?
      setReaders(lhs, false, blk)             // (4)
      setWriters(lhs, false, blk)              // (5)
      traverseBlock(blk)
      top = lhs                               // (11)

    case Pipe_parallel(func) =>
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      stages.foreach{s => parentOf(s) = lhs } // (2)
      childrenOf(lhs) = stages                // (3)

      setReaders(lhs, false, func)            // (4)
      setWriters(lhs, false, func)             // (5)
      traverseBlock(func)
      if (!parentOf(lhs).isDefined) top = lhs // (11)

    case Unit_pipe(func) =>
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      stages.foreach{s => parentOf(s) = lhs } // (2)
      childrenOf(lhs) = stages                // (3)

      setReaders(lhs, false, func)            // (4)
      setWriters(lhs, false, func)             // (5)
      traverseBlock(func)
      if (!parentOf(lhs).isDefined) top = lhs // (11)

    case Pipe_foreach(cc,func,inds) =>
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs }       // (1)
      parentOf(cc) = lhs

      stages.foreach{s => parentOf(s) = lhs }       // (2)
      childrenOf(lhs) = stages                      // (3)

      setReaders(lhs, false, func)                  // (4)
      setWriters(lhs, false, func)                   // (5)
      traverseWith(lhs,false,inds,cc)(func)

      if (styleOf(lhs) == Coarse) metapipes ::= lhs  // (8)
      if (!parentOf(lhs).isDefined) top = lhs        // (11)

    case Pipe_fold(cc,a,_,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) =>
      val isFine = styleOf(lhs) == Fine
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      parentOf(cc) = lhs

      stages.foreach{s => parentOf(s) = lhs } // (2)
      childrenOf(lhs) = stages                // (3)

      setReaders(lhs, false, func)            // (4)
      setWriters(lhs, false, func)             // (5)

      setReaders(lhs, !isFine, rFunc)         // (4)
      setWriters(lhs, !isFine, rFunc)          // (5)

      traverseWith(lhs,false,inds,cc)(func)
      traverseWith(lhs,false,inds,cc)(rFunc)

      checkMultipleWriters(a, lhs)
      readersOf(a) = readersOf(a) :+ (lhs, !isFine, lhs)  // (4)
      writersOf(a) = writersOf(a) :+ (lhs, !isFine, lhs)  // (5)
      isAccum(a) = true                                   // (6)

      if (styleOf(lhs) == Coarse) metapipes ::= lhs  // (8)
      if (!parentOf(lhs).isDefined) top = lhs        // (11)

    case Accum_fold(cc1,cc2,a,_,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV) =>
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      parentOf(cc1) = lhs
      parentOf(cc2) = lhs

      stages.foreach{s => parentOf(s) = lhs } // (2)
      childrenOf(lhs) = stages                // (3)

      setReaders(lhs, false, func)            // (4)
      setWriters(lhs, false, func)             // (5)

      setReaders(lhs, true, rFunc)            // (4)
      setWriters(lhs, true, rFunc)             // (5)

      traverseWith(lhs,false,inds1,cc1)(func)
      traverseWith(lhs,true, inds2,cc2)(rFunc)

      val partial = getBlockResult(func)
      readersOf(partial) = readersOf(partial) :+ (lhs,true,lhs) // (4)

      checkMultipleWriters(a, lhs)
      readersOf(a) = readersOf(a) :+ (lhs, true, lhs)     // (4)
      writersOf(a) = writersOf(a) :+ (lhs, true, lhs)     // (5)
      isAccum(a) = true
      val ipars = parParamsOf(cc2)                   // (6)
      addAccessParam(a, ipars.last)

      if (styleOf(lhs) == Coarse) metapipes ::= lhs  // (8)
      if (!parentOf(lhs).isDefined) top = lhs        // (11)

    case Reg_write(reg,value) =>
      isAccum(reg) = hasDependency(value, reg)      // (6)

    case Bram_load(bram,addr) =>
      addAccessFactor(bram, addr)                   // (9)

    case Bram_store(bram,addr,value) =>
      isAccum(bram) = hasDependency(value, bram)    // (6)
      addAccessFactor(bram, addr)                   // (9)

    case Bram_load_vector(bram,ofs,cchain,inds) =>
      readersOf(bram) = readersOf(bram) :+ (lhs,false,lhs)  // (4)
      addAccessParam(bram, parParamsOf(cchain).last)        // (9)

    case Bram_store_vector(bram,ofs,vec,cchain,inds) =>
      checkMultipleWriters(bram, lhs)
      writersOf(bram) = writersOf(bram) :+ (lhs,false,lhs)  // (4)
      isAccum(bram) = hasDependency(vec, bram)              // (6)
      addAccessParam(bram, parParamsOf(cchain).last)        // (9)
      writtenIn(lhs) = writtenIn(lhs) :+ bram               // (11)

    case _:Reg_new[_] => localMems ::= lhs    // (7)
    case _:Bram_new[_] => localMems ::= lhs   // (7)

    case _ => super.traverse(lhs, rhs)
  }
}



// --- Control analysis on unrolled IR
// Same as above, without access factors
trait UnrolledControlSignalAnalyzer extends ControlSignalAnalyzer {
  val IR: DHDLExp with ControlSignalAnalysisExp
  import IR._

  // All cases of multiple writers are ok now (we banked for parallelized writes already)
  override def checkMultipleWriters(mem: Exp[Any], writer: Exp[Any]) { }


  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Reg_write(reg,value) =>
      isAccum(reg) = hasDependency(value, reg)      // (6)

    case Bram_load(bram,addr) => // No access factors...

    case Bram_store(bram,addr,value) =>
      isAccum(bram) = hasDependency(value, bram)    // (6)

    case ParPipeForeach(cc,func,inds) =>
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      stages.foreach{s => parentOf(s) = lhs } // (2)
      parentOf(cc) = lhs
      childrenOf(lhs) = stages                // (3)

      setReaders(lhs, false, func)            // (4)
      setWriters(lhs, false, func)            // (5)

      traverseBlock(func)

      if (styleOf(lhs) == Coarse) metapipes ::= lhs  // (8)
      if (!parentOf(lhs).isDefined) top = lhs        // (11)

    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) =>
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      stages.foreach{s => parentOf(s) = lhs } // (2)
      parentOf(cc) = lhs
      childrenOf(lhs) = stages                // (3)

      setReaders(lhs, false, func)            // (4)
      setWriters(lhs, false, func)            // (5)

      // Necessary here?
      //setReaders(lhs, true, rFunc)            // (4)
      //setWriter(lhs, true, rFunc)             // (5)

      traverseBlock(func)
      //traverseWith(lhs,true, inds2,cc2)(rFunc)

      readersOf(accum) = readersOf(accum) ++ readersOf(acc) // (4)
      writersOf(accum) = writersOf(accum) ++ writersOf(acc) // (5)
      isAccum(accum) = true

      if (styleOf(lhs) == Coarse) metapipes ::= lhs  // (8)
      if (!parentOf(lhs).isDefined) top = lhs        // (11)

    case ParBramLoadVector(bram,ofs,cchain,inds) =>
      readersOf(bram) = readersOf(bram) :+ (lhs,false,lhs)  // (4)

    case ParBramStoreVector(bram,ofs,vec,cchain,inds) =>
      writersOf(bram) = writersOf(bram) :+ (lhs,false,lhs)  // (4)
      isAccum(bram) = hasDependency(vec, bram)              // (6)
      writtenIn(lhs) = writtenIn(lhs) :+ bram               // (11)

    case _ => super.traverse(lhs, rhs)
  }

}
