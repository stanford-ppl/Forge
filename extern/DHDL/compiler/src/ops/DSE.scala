package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal
import scala.virtualization.lms.common.EffectExp
import ppl.delite.framework.analysis.IRPrinterPlus
import ppl.delite.framework.Config

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._
import dhdl.compiler.transform._

import scala.collection.mutable.{HashMap,HashSet,ArrayBuffer}

trait DSE extends Traversal {
  val IR: DHDLCompiler
  import IR._

  override val debugMode = true

  def inferDoubleBuffers(localMems: List[Exp[Any]]) = {

    // TODO: Can probably generalize this and move it elsewhere
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
    localMems.flatMap{mem =>
      if (!writerOf(mem).isDefined) None
      else {
        readersOf(mem) find (_ != writerOf(mem).get) match {
          case Some(reader) =>
            val lca = leastCommonAncestor(reader, writerOf(mem).get)
            lca match {
              case Some(x) if meta[MPipeType](x).isDefined && styleOf(x) == Coarse => Some(mem -> x)
              case _ => None
            }

          case None => None
        }
      }
    }
  }

  lazy val ctrlAnalyzer = new ControlSignalAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
  lazy val paramAnalyzer = new ParameterAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
  lazy val printer = new IRPrinterPlus{val IR: DSE.this.IR.type = DSE.this.IR}
  lazy val bndAnalyzer = new BoundAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}

  lazy val tileSizes  = paramAnalyzer.tileSizes.distinct
  lazy val parFactors = paramAnalyzer.parFactors.distinct
  lazy val accFactors = ctrlAnalyzer.memAccessFactors.toList
  lazy val dblBuffers = inferDoubleBuffers(ctrlAnalyzer.localMems)

  def setDoubleBuffers() {
    dblBuffers.foreach{case (mem,ctrl) => isDblBuf(mem) = styleOf(ctrl) == Coarse }
  }
  def setBanks() {
    accFactors.foreach{case (mem,params) => banks(mem) = params.map(_.x).max } // TODO: LCM, not max
  }

  override def run[A:Manifest](b: Block[A]) = {
    ctrlAnalyzer.run(b)
    paramAnalyzer.run(b)

    if (debugMode) printer.run(b)
    if (debugMode) dblBuffers foreach {case (ctrl,mem) => debug(s"Found double buffer: $mem (in $ctrl)") }

    if (Config.enableDSE) dse(b)

    bndAnalyzer.run(b)
    tileSizes.foreach{p => p.fix}
    parFactors.foreach{p => p.fix}
    setDoubleBuffers()
    setBanks()

    (b)
  }


  def dse[A:Manifest](b: Block[A]) {
    // Specify FPGA target (hardcoded right now)
    val target = FPGATarget

    val areaAnalyzer = new AreaAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
    val cycleAnalyzer = new LatencyAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
    cycleAnalyzer.silenceTraversal()
    areaAnalyzer.silenceTraversal()

    // A. Get lists of parameters
    val metapipes = ctrlAnalyzer.metapipes
    var restrict  = paramAnalyzer.restrict
    val ranges    = paramAnalyzer.range

    // HACK: All par factors for readers and writers of a given BRAM must be equal or one
    for ((mem,factors) <- accFactors) { restrict ::= REqualOrOne(factors) }

    // B. Compress space
    // TODO


    def isLegalSpace() = restrict.forall(_.evaluate)

    def evaluate() = {
      setDoubleBuffers()
      setBanks()
      bndAnalyzer.run(b)
      areaAnalyzer.run(b)
      cycleAnalyzer.run(b)
      (areaAnalyzer.totalArea, cycleAnalyzer.totalCycles)
    }

    // C. Calculate space
    debug("Running DSE")
    debug("Tile Sizes: ")
    tileSizes.foreach{t => debug(s"  $t")}
    debug("Parallelization Factors:")
    parFactors.foreach{t => debug(s"  $t")}
    debug("Metapipelining Toggles:")
    metapipes.foreach{t => debug(s"  $t")}
    debug("")
    debug("Found the following parameter restrictions: ")
    for (r <- restrict)   { debug(s"  $r") }
    for ((p,r) <- ranges) { debug(s"  $p: ${r.start}:${r.step}:${r.end}") }

    val space = tileSizes.map{t => Domain(ranges(t)) } ++
                parFactors.map{t => Domain(ranges(t)) } ++
                metapipes.map{t => Domain(List(true,false)) }

    val TS = tileSizes.length
    val PF = tileSizes.length + parFactors.length

    // D. DSE
    val N = space.length
    val dims = space.map(_.len)
    val spaceSize = dims.map(_.toLong).fold(1L){_*_}
    val prods = List.tabulate(N){i => dims.slice(i+1,N).map(_.toLong).fold(1L){_*_}}

    debug(s"Total space size is $spaceSize")

    val iStep = 1//: () => Int = if (spaceSize < 200000) {() => 1} else {() => util.Random.nextInt(100) } // TODO

    var legalSize = 0
    var validSize = 0

    val indx = ArrayBuffer[Long]()
    val alms = ArrayBuffer[Int]()
    val dsps = ArrayBuffer[Int]()
    val bram = ArrayBuffer[Int]()
    val cycles = ArrayBuffer[Long]()

    case class ParetoPt(idx: Int, alms: Int, cycles: Long)

    val pareto = ArrayBuffer[ParetoPt]()

    debug("And aaaawaaaay we go!")

    val indexedSpace = (space, xrange(0,N,1).toList).zipped

    val startTime = System.currentTimeMillis
    var i = 0L
    var pos = 0
    while (i < spaceSize) {
      // Get and set current parameter combination
      val point = indexedSpace.map{case (domain,d) => domain( ((i / prods(d)) % dims(d)).toInt ) }
      val ts = point.slice(0, TS).map(_.asInstanceOf[Int])
      val pf = point.slice(TS,PF).map(_.asInstanceOf[Int])
      val mp = point.drop(PF).map(_.asInstanceOf[Boolean])

      tileSizes.zip(ts).foreach{case (ts: Param[Int], c: Int) => ts.setValue(c) }
      parFactors.zip(pf).foreach{case (pf: Param[Int], c: Int) => pf.setValue(c) }
      metapipes.zip(mp).foreach{case (mp,c) => styleOf(mp) = (if (c) Coarse else Disabled) }

      if (isLegalSpace()) {
        legalSize += 1
        val (area,runtime) = evaluate()

        if (area.alms < target.alms && area.dsps < target.dsps && area.bram < target.bram && area.streams < target.streams) {
          validSize += 1
          indx += i
          alms += area.alms; dsps += area.dsps; bram += area.bram; cycles += runtime

          // Check if this is a pareto frontier candidate
          var wasAdded = false
          var candidate = true

          var j = 0
          while (j < pareto.size && !wasAdded && candidate) {
            val p = pareto(j)
            if (area.alms < p.alms && runtime < p.cycles) {
              pareto(j) = ParetoPt(pos,area.alms,runtime) // Strictly less than existing frontier point: replace
              wasAdded = true
            }
            candidate = area.alms < p.alms || runtime < p.cycles
            j += 1
          }
          if (!wasAdded && candidate) pareto += ParetoPt(pos,area.alms,runtime)

          pos += 1
        }
      }
      i += iStep
    }
    val time = (System.currentTimeMillis - startTime)/1000.0
    debug(s"Completed space search in $time seconds")
    debug(s"Total legal space size: $legalSize / $spaceSize")
    debug(s"Valid designs generated: $validSize / $spaceSize")
    debug(s"Pareto frontier size: ${pareto.length} / $spaceSize")

    // F. Show user results, pick point on pareto curve
    // TODO

    // G. Set parameters
    // TODO

  }
}
