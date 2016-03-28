package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import ppl.delite.framework.DeliteApplication


trait AreaAnalysisExp extends DHDLExp with AreaModel with LatencyModel {
  this: DHDLCompiler with DHDLApplication with DeliteApplication =>

  // TODO: This shouldn't be hardcoded
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
  val IR: AreaAnalysisExp
  import IR._

  private var inHwScope = false
  var area = NoArea
  var areaScope: List[FPGAResources] = Nil

  def inAreaScope(x: => Unit) = {
    val outerScope = areaScope
    areaScope = Nil
    x
    val area = areaScope.fold(NoArea){_+_}
    areaScope = outerScope
    (area)
  }

  override def traverseStm(stm: Stm) = stm match {
    case TP(s, d) => traverseNode(s, d)
  }


  def traverseNode(lhs: Exp[Any], rhs: Def[Any])(implicit ctx: SourceContext) {
    if (inHwScope) rhs match {
      case _ => // TODO


    } else rhs match {
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
