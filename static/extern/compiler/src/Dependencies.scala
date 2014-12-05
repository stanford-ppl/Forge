package LOWERCASE_DSL_NAME.compiler

import java.util.IdentityHashMap

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

trait DependenciesOpsExp extends BaseFatExp with EffectExp {
  // Have to use reference equality, or we stack overflow due to DeliteOps.scala equals override
  // However, this probably will not work post-mirroring, since the map will be invalid.
  // This requires further investigation. Punting for now.
  val dependenceMap: IdentityHashMap[Any, List[Exp[Any]]] = new IdentityHashMap()

  def infix_getOrElse[A,B](map: IdentityHashMap[A,B], key: A, default: B) = {
    if (map.containsKey(key)) map.get(key)
    else default
  }

  // One thing to watch out for: if a higher-priority syms method gets called
  // before us and decomposes 'syms' so that it is no longer called on our
  // original symbol, our syms method will never get called and dependencies
  // won't be honored. We currently mix-in externs nearly last, and save
  // syms on Defs when possible (which get called first), to try to avoid this.

  def forge_after[A,B](a: => Exp[A], b: => Exp[B]): Exp[A] = {
    val first: Exp[B] = b
    val second: Exp[A] = a
    second match {
      case Def(d) => dependenceMap.put(d, first :: dependenceMap.getOrElse(d,Nil))
      case _ => dependenceMap.put(second, first :: dependenceMap.getOrElse(a,Nil))
    }
    second
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case x if dependenceMap.containsKey(x) => syms(dependenceMap.get(x)) ::: super.syms(e)
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case x if dependenceMap.containsKey(x) => freqCold(dependenceMap.get(x)) ::: super.symsFreq(e)
    case _ => super.symsFreq(e)
  }
}
