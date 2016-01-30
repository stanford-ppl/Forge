package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

trait ProfilingOpsExp extends BaseFatExp with EffectExp {
  case class ForgeProfileStart(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]
  case class ForgeProfileStop(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]
  case class ForgeProfileTime(deps: List[Exp[Any]]) extends Def[Long]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(ForgeProfileStart(c,deps), u, es) => reflectMirrored(Reflect(ForgeProfileStart(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ForgeProfileStop(c,deps), u, es) => reflectMirrored(Reflect(ForgeProfileStop(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ForgeProfileTime(deps), u, es) => reflectMirrored(Reflect(ForgeProfileTime(f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  def forge_profile_start(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(ForgeProfileStart(component, deps.toList))
  def forge_profile_stop(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(ForgeProfileStop(component, deps.toList))
  def forge_profile_time(deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(ForgeProfileTime(deps.toList))
}

trait ScalaGenProfilingOps extends ScalaGenEffect {
  val IR: ProfilingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ForgeProfileStart(c,deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.start(" + quote(c) + ")")
    case ForgeProfileStop(c,deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.stop(" + quote(c) + ")")
    case ForgeProfileTime(deps) => emitValDef(sym, "System.currentTimeMillis()")
    case _ => super.emitNode(sym,rhs)
  }
}

trait CudaGenProfilingOps
trait OpenCLGenProfilingOps

trait CGenProfilingOps extends CGenEffect {
  val IR: ProfilingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ForgeProfileStart(c,deps) =>
      stream.println("DeliteCppTimerTic(" + quote(c) + ");")
    case ForgeProfileStop(c,deps) =>
      stream.println("DeliteCppTimerToc(" + quote(c) + ");")
    case ForgeProfileTime(deps) =>
      stream.println("struct timeval _" + quote(sym) + ";")
      stream.println("gettimeofday(&_" + quote(sym) + ", NULL);")
      emitValDef(sym, "(_" + quote(sym) + ".tv_sec * 1000000L + _" + quote(sym) + ".tv_usec)/1000")
    case _ => super.emitNode(sym,rhs)
  }
}
