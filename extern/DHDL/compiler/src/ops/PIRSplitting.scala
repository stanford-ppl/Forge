package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet,Queue,ArrayBuffer}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait PIRSplitter extends Traversal with PIRCommon {
  val IR: DHDLExp with PIRScheduleAnalysisExp
  import IR._

  val MAX_STAGES = 10   // 10 compute stages total
  val MAX_IN     = 4    // Up to 4 scalar inputs and 4 vector inputs
  // Assumed there is only up to one vector local for now

  override val name = "PIR Splitting"
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val cuMapping = HashMap[Exp[Any], ComputeUnit]()
  def allocateCU(pipe: Exp[Any]): ComputeUnit = cuMapping(pipe)

  val cus = HashMap[Exp[Any], List[ComputeUnit]]()

  override def run[A:Manifest](b: Block[A]) = {
    debug(s"\n\n\n")
    for ((pipe,cu) <- cuMapping) {
      val splitCUs = splitCU(cu)
      cus += pipe -> splitCUs
    }
    (b)
  }

  def splitCU(cu: ComputeUnit): List[ComputeUnit] = cu match {
    case tu: TileTransferUnit => List(tu)
    case cu: BasicComputeUnit =>
      // TODO: DEBUGGING ONLY. Remove this check later
      val nStages = cu.stages.length + cu.writeStages.values.map(_.length).fold(0){_+_}
      if (nStages > MAX_STAGES) splitComputeCU(cu) else List(cu)
  }

  def splitComputeCU(cu: BasicComputeUnit): List[BasicComputeUnit] = List(cu)

  /*{
    val MAX_OUT = if (cu.isUnitCompute) 4 else  1    // Maximum scalar output and vector outputs

    debug(s"Splitting CU: $cu")
    def inputsOf(stage: Stage) = stage match {
      case stage: MapStage => stage.inputMems.filterNot(stage.outputMems contains _ ) // Ignore cycles
      case stage: ReduceStage =>
        val idx = cu.stages.indexOf(stage)
        cu.stages(idx - 1).outputMems.filter(reg => reg.isInstanceOf[ReduceReg] || reg.isInstanceOf[AccumReg])
    }
    def outputsOf(stage: Stage) = stage.outputMems

    val cuOutputs = allMapStages(cu).flatMap{stage => outputsOf(stage).filter{reg => reg.isInstanceOf[ScalarOut] || reg.isInstanceOf[VectorOut] }}
    debug(s"CU Outputs: $cuOutputs")

    val outStages = cu.stages.filter{stage => outputsOf(stage).exists(cuOutputs contains _)}
    val liveOuts = outStages.map{stage => outputsOf(stage).filter(cuOutputs contains _)}

    def getScheduleFor(stages: Set[Stage])(result: Stage*) = {
      val frontier = Queue[Stage](result:_*)
      var visited  = HashSet[Stage](result:_*)
      var schedule = ArrayBuffer[Stage]()

      while (frontier.nonEmpty) {
        val stage = frontier.dequeue()
        schedule += stage
        val deps = inputsOf(stage).flatMap{in => stages.find{stage => outputsOf(stage) contains in}}.filterNot(visited contains _)
        frontier.enqueue(deps:_*)
        visited ++= deps
      }
      schedule.toList
    }

    val stagesSet = cu.stages.toSet

    val schedules = Map(outStages.map{stage => stage -> getScheduleFor(stagesSet)(stage).toSet }:_*)

    val unusedOutputs = HashSet[Stage]() ++ outStages
    val groups = ArrayBuffer[HashSet[Stage]]()
    def curGroup = groups.last
    var groupSchedule: Set[Stage] = Set.empty

    while (unusedOutputs.nonEmpty) {
      if (groups.isEmpty || groupIsFull(curGroup)) {
        val stage = unusedOutputs.head
        groups += HashSet(stage)
        unusedOutputs -= stage
        groupSchedule = schedules(stage)
      }
      else {
        val stage = unusedOutputs.map{stage => (stage, (groupSchedule union schedules(stage)).size)}.reduce{(a,b) =>
            if (a._2 > b._2) a._1 else b._1
        }
        curGroup += stage
        unusedOutputs -= stage
        groupSchedule ++= schedules(stage)
      }
    }



    /*val computedOutputs = HashMap[Stage, List[LocalMem]]()

    def bfs(frontier: List[Stage], live: List[List[LocalMem]]): Unit = if (frontier.nonEmpty) {
      debug(s"Frontier:")
      frontier.zip(live).foreach{case (stage, liveIn) =>
        debug(s"  $stage")
        debug(s"  $liveIn")
        debug("")
        liveIns(stage) = liveIn
      }

      val inputLivePairs = frontier.zip(live).flatMap{case (stage,liveIn) =>
        inputsOf(stage).map{input => input -> (liveIn :+ input).filterNot(outputsOf(stage) contains _) }
      }
      val inputMap = inputLivePairs.groupBy(_._1).mapValues{lists => lists.map(_._2).flatten.distinct }
      debug(s"Input Mapping: ")
      for ((input,lives) <- inputMap) {
        debug(s"$input : (${lives.length}) - $lives ")
      }
      val inputList: List[(LocalMem, List[LocalMem])] = inputMap.toList
      val inputLiveList: List[(Stage, List[LocalMem])] = inputList.flatMap{case (input,liveIn) => cu.stages.find{stage => outputsOf(stage).contains(input) }.map(stage => stage -> liveIn) }
      val newFrontier = inputLiveList.map(_._1)
      val newLiveIns  = inputLiveList.map(_._2)
      bfs(newFrontier, newLiveIns)
    }

    bfs(outStages, liveOuts)*/
  }*/

}
