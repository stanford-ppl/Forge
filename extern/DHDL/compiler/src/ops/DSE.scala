package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal
import ppl.delite.framework.analysis.IRPrinterPlus

import scala.collection.immutable.HashMap

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait DSE extends Traversal {
  val IR: DHDLCompiler
  import IR._

  override val debugMode = true

  override def run[A:Manifest](b: Block[A]): Block[A] = {
    val paramAnalyzer = new ParameterAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
    def tileSizes  = paramAnalyzer.tileSizes
    def parFactors = paramAnalyzer.parFactors
    def metapipes  = paramAnalyzer.metapipes

    val ctrlAnalyzer = new ControlSignalAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
    def localMems = ctrlAnalyzer.localMems

    val areaAnalyzer = new AreaAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
    def totalArea = areaAnalyzer.totalArea

    val cycleAnalyzer = new LatencyAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
    def totalCycles = cycleAnalyzer.totalCycles

    val printer = new IRPrinterPlus{val IR: DSE.this.IR.type = DSE.this.IR}

    // A. Get lists of parameters
    paramAnalyzer.run(b)
    ctrlAnalyzer.run(b)

    printer.run(b)

    // B. Get lists of BRAMs/Regs for each Metapipe which require double buffering
    def leastCommonAncestor(x: (Exp[Any],Boolean), y: (Exp[Any],Boolean)): Option[Exp[Any]] = {
      var pathX: List[(Exp[Any],Boolean)] = List(x)
      var pathY: List[(Exp[Any],Boolean)] = List(y)

      def hasParent(n: (Exp[Any],Boolean)) = n._2 || parentOf(n._1).isDefined
      def getParent(n: (Exp[Any],Boolean)) = if (n._2) (n._1,false) else (parentOf(n._1).get,false)

      var curX = x
      while (hasParent(curX)) { curX = getParent(curX); pathX ::= curX }

      var curY = y
      while (hasParent(curY)) { curY = getParent(curY); pathY ::= curY }

      // Choose last node where paths are the same
      // Note this wouldn't work if paths could diverge and then converge again, but this is never the case
      val intersect =pathX.zip(pathY).filter{case (x,y) => x == y}.map(_._1)
      if (intersect.isEmpty) None
      else Some(intersect.last._1)
    }

    // Heuristic - find memories which have a reader and a writer which are different
    // but whose nearest common parent is a metapipeline.
    val dblBuffList = localMems.flatMap{mem =>
      if (!writerOf(mem).isDefined) None
      else {
        readersOf(mem) find (_ != writerOf(mem).get) match {
          case Some(reader) =>
            debug(s"Found double buffer candidate: $mem")

            val lca = leastCommonAncestor(reader, writerOf(mem).get)
            debug(s"  LCA = $lca")

            lca match {
              case Some(x) if meta[MPipeType](x).isDefined && styleOf(x) == Coarse => Some(x -> mem)
              case _ => None
            }

          case None => None
        }
      }
    }
    dblBuffList foreach {
      case (ctrl,mem) => debug(s"Found double buffer: $ctrl => $mem")
    }

    val doubleBuffers = HashMap[Exp[Any],Exp[Any]]() ++ dblBuffList

    // C. Calculate space

    // D. DSE cycle
    //   1. Get combination of parameters
    //   2. Set direct parameters
    //   3. Set metapipeline factors
    //   4. Set double buffer values
    //   5. Set number of banks for each
    //   6. Run area and cycle analysis
    //   7. Record results

    // (6)
    //areaAnalyzer.run(b)
    //cycleAnalyzer.run(b)

    // E. Calculate pareto curve
    // F. Show user results, pick point on pareto curve

    // G. Set parameters
    // H. Finalize parameters
    tileSizes.foreach{p => p.fix}
    parFactors.foreach{p => p.fix}

    (b)
  }
}
