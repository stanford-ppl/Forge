package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait AreaAnalysisExp extends AreaModel with LatencyModel with CounterToolsExp {
  this: DHDLExp =>

  // TODO: This shouldn't be hardcoded (or here at all really)
  val BaseDesign = FPGAResources(
    lut7 = 468,
    lut6 = 9200,
    lut5 = 12350,
    lut4 = 11140,
    lut3 = 22600,
    mem64 = 4619,
    mem32 = 519,
    mem16 = 559,
    regs = 75400,
    dsps = 0,
    bram = 338
  )
}

trait AreaAnalyzer extends Traversal {
  val IR: AreaAnalysisExp with DHDLExp
  import IR._

  // Need separate traversal instance to do a different type of traversal
  val latencyModel = new LatencyTools{val IR: this.IR.type = this.IR }

  var totalArea: FPGAResources = NoArea
  var areaScope: List[FPGAResources] = Nil

  def areaOf(e: Exp[Any]) = IR.areaOf(e, inReduce)

  def areaOfBlock(b: Block[Any]) = {
    val outerScope = areaScope
    areaScope = Nil
    traverseBlock(b)
    val area = areaScope.fold(NoArea){_+_}
    areaScope = outerScope
    (area)
  }
  def areaOfReduce(b: Block[Any]) = {
    val outerReduce = inReduce
    inReduce = true
    val out = areaOfBlock(b)
    inReduce = outerReduce
    out
  }

  override def traverseStm(stm: Stm) = stm match {
    case TP(s, d) => traverseNode(s, d)
  }

  /*
    Calculate delay line costs:
    a. Determine time (in cycles) any given input or internal signal needs to be delayed
    b. Distinguish each delay line as a separate entity

    Is there a concise equation that can capture this? Haven't been able to come up with one.
    E.g.
      8 inputs => perfectly balanced binary tree, no delay paths
      9 inputs => 1 path of length 3
      85 inputs => 3 paths with lengths 2, 1, and 1
  */
  def reductionDelays(nLeaves: Int): List[Int] = {
    if (isPow2(nLeaves)) Nil
    // Could also have 2^k + 1 case (delay = 1 path of length k)
    else {
      def reduceLayer(nNodes: Int, completePaths: List[Int], currentPath: Int): List[Int] = {
        if (nNodes <= 1) completePaths  // Stop when 1 node is remaining
        else if (nNodes % 2 == 0) {
          // For an even number of nodes, we don't need any delays - all current delay paths end
          val allPaths = completePaths ++ (if (currentPath > 0) List(currentPath) else Nil)
          reduceLayer(nNodes/2, allPaths, 0)
        }
        // For odd number of nodes, always delay exactly one signal, and keep delaying that signal until it can be used
        else reduceLayer((nNodes-1)/2 + 1, completePaths, currentPath+1)
      }

      reduceLayer(nLeaves, Nil, 0)
    }
  }


  def traverseNode(lhs: Exp[Any], rhs: Def[Any])(implicit ctx: SourceContext) {
    if (inHwScope) {
      val area = rhs match {
        case EatReflect(Counterchain_new(ctrs,nIters)) => areaOfBlock(nIters) + areaOf(lhs)
        case EatReflect(Pipe_parallel(func)) => areaOfBlock(func) + areaOf(lhs)
        case EatReflect(Unit_pipe(func)) => areaOfBlock(func) + areaOf(lhs)

        case EatReflect(Pipe_foreach(cchain, func, _)) =>
          val P = parOf(cchain).reduce(_*_)
          areaOfBlock(func) * P + areaOf(lhs)

        case EatReflect(Pipe_reduce(cchain,_,ld,st,func,rFunc,_,_,_,_)) =>
          val P = parOf(cchain).reduce(_*_)
          val body = areaOfBlock(func) * P // map duplicated P times
          /*
            Some simple math:
            A full binary (reduction) tree is a tree in which every node is either
            a leaf or has exactly two children.
            The number of internal (non-leaf) nodes of a full tree with L leaves is L - 1
            In our case, L is the map's parallelization factor P
            and internal nodes represent duplicates of the reduction function
            The reduction function is therefore duplicated P - 1 times
            Plus the special, tightly cyclic reduction function to update the accumulator
          */
          val internal = areaOfBlock(rFunc) * (P - 1)
          val rFuncLatency = latencyOfBlock(rFunc)
          val internalDelays = reductionDelays(P).map{delay => areaOfDelayLine(nbits(e.mT), delay * rFuncLatency)}.fold(NoArea){_+_}
          val load  = areaOfReduce(ld)    // Load from accumulator (happens once)
          val cycle = areaOfReduce(rFunc) // Special case area of accumulator
          val store = areaOfReduce(st)    // Store to accumulator (happens once)

          body + internal + internalDelays + load + cycle + store + areaOf(lhs)

        case EatReflect(Block_reduce(ccOuter,ccInner,_,func,ld1,ld2,rFunc,st,_,_,_,_,_,_)) =>
          val Pm = parOf(ccOuter).reduce(_*_) // Parallelization factor for map
          val Pr = parOf(ccInner).reduce(_*_) // Parallelization factor for reduce
          val body = areaOfBlock(func) * Pm

          val internal = areaOfBlock(rFunc) * (Pm - 1) * Pr
          val rFuncLatency = latencyOfBlock(rFunc)
          val internalDelays = reductionDelays(Pm).map{delay => areaOfDelayLine(nbits(e.mT), delay * rFuncLatency) * Pr}.fold(NoArea){_+_}
          val load1 = areaOfBlock(ld1) * Pm * Pr
          val load2 = areaOfReduce(ld2) * Pr
          val cycle = areaOfReduce(rFunc) * Pr
          val store = areaOfReduce(st) * Pr

          body + internal + internalDelays + load1 + load2 + cycle + store + areaOf(lhs)

        case _ => areaOf(lhs)
      }
      areaScope ::= area
    }
    else rhs match {
       case Hwblock(blk) =>
        inHwScope = true
        traverseBlock(blk)
        inHwScope = false

      case Reflect(d,_,_) => traverseNode(lhs, d)
      case _ => blocks(rhs) foreach traverseBlock
    }
  }

  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {

    (b)
  }

}
