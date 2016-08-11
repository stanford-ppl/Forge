package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait OffChipAnalyzer extends Traversal {
  val IR: DHDLExp
  import IR._

  override val name = "OffChip Analyzer"
  override val eatReflect = true   // Ignore reflect wrappers
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  var softValue: Map[Exp[Reg[Any]], Exp[Any]] = Map.empty
  var offchips: Set[Exp[OffChipMem[Any]]] = Set.empty

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Set_arg(reg, value) => softValue += reg -> value
    case Offchip_new(_) => offchips += lhs.asInstanceOf[Exp[OffChipMem[Any]]]
    case _ => super.traverse(lhs, rhs)
  }

  override def postprocess[A:Manifest](b: Block[A]) = {
    debug("Set input arguments: ")
    for ((reg,value) <- softValue) {
      debug(s"  $reg: $value")
    }
    debug("OffChipMems: ")
    for (offchip <- offchips) {
      debug(s"  $offchip")
      val dims = dimsOf(offchip)
      dims.foreach{
        case dim@Def(rhs) => debug(s"    $dim = $rhs")
        case dim => debug(s"    $dim")
      }
    }

    offchips.foreach{offchip =>
      val softDims = dimsOf(offchip).zipWithIndex.map{case (dim,i) => dim match {
        case Deff(Reg_read(reg)) if isArgIn(reg) && softValue.contains(reg) => softValue(reg)
        case dim@Exact(c) => dim
        case _ =>
          val name = nameOf(offchip).getOrElse("")
          stageError(s"Unable to resolve OffChipMem $name dimension $i. Dimensions must be constants or input arguments.")(mpos(offchip.pos))
      }}
      softDimsOf(offchip) = softDims.asInstanceOf[List[Exp[Index]]]
    }
    super.postprocess(b)
  }

}
