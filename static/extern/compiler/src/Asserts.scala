package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

trait AssertsOpsExp extends BaseFatExp with EffectExp {
  case class ForgeAssert(cond: Exp[Boolean], err: Exp[String]) extends Def[Unit]
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(ForgeAssert(c,err), u, es) => reflectMirrored(Reflect(ForgeAssert(f(c),f(err)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)    
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  def forge_assert(cond: Rep[Boolean], err: Rep[String])(implicit ctx: SourceContext): Rep[Unit] = {
    // only assert when debug is true, since assert effects can interfere with optimizations
    if (Config.debug) {
      reflectEffect(ForgeAssert(cond, err))
    }
    else {
      ()
    }
  }
}

trait ScalaGenAssertsOps extends ScalaGenEffect {
  val IR: AssertsOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ForgeAssert(cond,err) => emitValDef(sym, "assert("+quote(cond)+","+quote(err)+")")    
    case _ => super.emitNode(sym,rhs)
  }
}
