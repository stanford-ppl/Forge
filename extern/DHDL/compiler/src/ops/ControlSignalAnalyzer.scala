package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait ControlSignalAnalysisExp extends PipeStageToolsExp { this: DHDLExp => }

// (1) Sets parent control nodes of local memories
// (2) Sets parent control nodes of controllers
// (3) Sets children control nodes of controllers
// (4) Sets reader control nodes of locally read memories
// (5) Sets writer control nodes of locally written memories
// (6) Flags accumulators
trait ControlSignalAnalyzer extends Traversal with PipeStageTools {
  val IR: DHDLExp with ControlSignalAnalysisExp
  import IR._

  var level = 0
  var indsOwners: Map[Exp[Any], (Exp[Any], Boolean, Int)] = Map.empty
  var localMems: List[Exp[Any]] = Nil


  def traverseWith(owner: Exp[Any], isReduce: Boolean, inds: List[Exp[Any]])(b: Block[Any]) = {
    level += 1
    val prevOwners = indsOwners
    inds.foreach{ind => indsOwners += ind -> (owner, isReduce, level) }
    traverseBlock(b)
    indsOwners = prevOwners
    level -= 1
  }

  def getTopController(ctrl: Exp[Any], isReduce: Boolean, addr: Exp[Any]): (Exp[Any], Boolean) = {

    def dfs(frontier: List[Exp[Any]]): List[Exp[Any]] = {
      frontier.flatMap{
        case Def(d) => dfs(syms(d))
        case bnd => List(bnd)
      }
    }
    val bnds = dfs(List(addr))

    val top = bnds.flatMap{d => indsOwners.get(d)}.fold((ctrl,isReduce,level+1)){(a,b) => if (a._3 < b._3) a else b}
    (top._1, top._2)
  }

  def setReaders(ctrl: Exp[Any], isReduce: Boolean, blks: Block[Any]*) {
    val reads = getLocalReaders(blks:_*)
    reads.foreach{
      case Def(EatReflect(Reg_read(reg))) => readersOf(reg) = readersOf(reg) :+ (ctrl,isReduce)
      case Def(EatReflect(Bram_load(ram,addr))) =>
        val top = getTopController(ctrl, isReduce, addr)
        readersOf(ram) = readersOf(ram) :+ top
    }
  }

  def setWriter(ctrl: Exp[Any], isReduce: Boolean, blks: Block[Any]*) {
    val writes = getLocalWriters(blks:_*)
    writes.foreach{
      case Def(EatReflect(Reg_write(reg,_))) => writerOf(reg) = (ctrl, isReduce)
      case Def(EatReflect(Bram_store(ram,addr,_))) =>
        val top = getTopController(ctrl, isReduce, addr)
        writerOf(ram) = top
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


  def traverseNode(lhs: Exp[Any], rhs: Def[Any]): Unit = rhs match {
    case EatReflect(TileTransfer(m,local,s,o,t,c,i,st)) =>
      if (st) readersOf(local) = readersOf(local) :+ (lhs,false)  // (4) Store to offchip
      else    writerOf(local) = lhs                               // (5) Read from offchip

    case EatReflect(Pipe_parallel(func)) =>
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      stages.foreach{s => parentOf(s) = lhs } // (2)
      childrenOf(lhs) = stages                // (3)

      setReaders(lhs, false, func)            // (4)
      setWriter(lhs, false, func)             // (5)
      traverseBlock(func)

    case EatReflect(Unit_pipe(func)) =>
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      stages.foreach{s => parentOf(s) = lhs } // (2)
      childrenOf(lhs) = stages                // (3)

      setReaders(lhs, false, func)            // (4)
      setWriter(lhs, false, func)             // (5)
      traverseBlock(func)

    case EatReflect(Pipe_foreach(cc,func,inds)) =>
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      parentOf(cc) = lhs

      stages.foreach{s => parentOf(s) = lhs } // (2)
      childrenOf(lhs) = stages                // (3)

      setReaders(lhs, false, func)            // (4)
      setWriter(lhs, false, func)             // (5)
      traverseWith(lhs,false,inds)(func)


    case EatReflect(Pipe_reduce(cc,a,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV)) =>
      val isFine = styleOf(lhs) == Fine
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      parentOf(cc) = lhs

      stages.foreach{s => parentOf(s) = lhs } // (2)
      childrenOf(lhs) = stages                // (3)

      setReaders(lhs, false, func)            // (4)
      setWriter(lhs, false, func)             // (5)

      setReaders(lhs, !isFine, rFunc)         // (4)
      setWriter(lhs, !isFine, rFunc)          // (5)

      traverseWith(lhs,false,inds)(func)
      traverseWith(lhs,false,inds)(rFunc)

      readersOf(a) = readersOf(a) :+ (lhs, !isFine)  // (4)
      writerOf(a) = (lhs, !isFine)                   // (5)
      isAccum(a) = true                       // (6)

    case EatReflect(Block_reduce(cc1,cc2,a,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV)) =>
      val allocs = getAllocations(func)
      val stages = getControlNodes(func)
      allocs.foreach{a => parentOf(a) = lhs } // (1)
      parentOf(cc1) = lhs
      parentOf(cc2) = lhs

      stages.foreach{s => parentOf(s) = lhs } // (2)
      childrenOf(lhs) = stages                // (3)

      setReaders(lhs, false, func)            // (4)
      setWriter(lhs, false, func)             // (5)

      setReaders(lhs, true, rFunc)            // (4)
      setWriter(lhs, true, rFunc)             // (5)

      traverseWith(lhs,false,inds1)(func)
      traverseWith(lhs,true, inds2)(rFunc)

      val partial = getBlockResult(func)
      readersOf(partial) = readersOf(partial) :+ (lhs,true) // (4)

      readersOf(a) = readersOf(a) :+ (lhs, true) // (4)
      writerOf(a) = (lhs, true)                  // (5)
      isAccum(a) = true                          // (6)

    case EatReflect(Reg_write(reg,value)) =>
      isAccum(reg) = hasDependency(value, reg)    // (6)

    case EatReflect(Bram_store(ram,addr,value)) =>
      isAccum(ram) = hasDependency(value, ram)    // (6)

    case EatReflect(_:Reg_new[_]) => localMems ::= lhs
    case EatReflect(_:Bram_new[_]) => localMems ::= lhs

    case _ => blocks(rhs) foreach traverseBlock
  }

  override def traverseStm(stm: Stm) = stm match {
    case TP(s, d) => traverseNode(s, d)
  }

}
