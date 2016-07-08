package dhdl.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.virtualization.lms.internal.{Traversal}
import scala.reflect.{Manifest,SourceContext}
import scala.collection.mutable.Set
import java.io.{File, FileWriter, PrintWriter}
import ppl.delite.framework.transform.{DeliteTransform}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait DHDLCounter
trait DHDLCounterChain

trait ExternCounterTypesExp extends ExternCounterTypes {
  type Counter = DHDLCounter
  type CounterChain = DHDLCounterChain

  def counterManifest: Manifest[Counter] = manifest[DHDLCounter]
  def counterChainManifest: Manifest[CounterChain] = manifest[DHDLCounterChain]
}

trait ExternCounterOpsExp extends ExternCounterTypesExp with CounterOpsExp with CounterChainOpsExp {
  this: DHDLExp =>

  // --- Nodes
  case class Counter_new(start: Rep[Idx], end: Rep[Idx], step: Rep[Idx], par: Param[Int])(implicit val ctx: SourceContext) extends Def[Counter]
  case class Counterchain_new(counters: List[Rep[Counter]], nIter: Rep[Idx])(implicit val ctx: SourceContext) extends Def[CounterChain]

  // --- Internal API
  def counter_new(start: Rep[Idx],end: Rep[Idx],step: Rep[Idx], par: Rep[Int])(implicit ctx: SourceContext) = {
    val truePar: Param[Int] = par match {
      case Const(c) =>
        val p = param(c)
        domainOf(p) = (c,c,1)
        p
      case p: Param[_] => p.asInstanceOf[Param[Int]]

      case _ => stageError("Counter parallelization factor must be a parameter or a constant")
    }
    reflectEffect[Counter](Counter_new(start,end,step,truePar)(ctx))
  }

  private def counterSplit(x: Rep[Counter]): (Rep[Idx],Rep[Idx],Rep[Idx],Param[Int]) = x match {
    case Def(EatReflect(Counter_new(start,end,step,par))) => (start,end,step,par)
    case _ => throw new Exception("Could not find def for counter")
  }

  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain] = {
    val ctrSplit = counters.map{ctr => counterSplit(ctr)}
    val starts: List[Rep[Idx]] = ctrSplit.map(_._1)
    val ends:   List[Rep[Idx]] = ctrSplit.map(_._2)
    val steps:  List[Rep[Idx]] = ctrSplit.map(_._3)
    val pars:   List[Rep[Idx]] = ctrSplit.map(_._4).map(int_to_fix[Signed,B32](_)) // HACK: Convert Int param to fixed point

    val nIter = {
      // TODO: These should be more general rewrite rules
      val lens: List[Rep[Idx]] = starts.zip(ends).map{
        case (ConstFix(x: Int),ConstFix(y: Int)) => (y - x).as[Idx]
        case (ConstFix(0), end) => end
        case (start, end) => sub(end, start)
      }
      val total: List[Rep[Idx]] = (lens,steps,pars).zipped.map {
        case (len, ConstFix(1), par) => div(len, par) // Should round correctly here
        case (len, step, par) => div(len, mul(step, par))
      }
      productTree(total)
    }

    // HACK: Not actually mutable, but isn't being scheduled properly otherwise
    reflectMutable(Counterchain_new(counters, nIter)(ctx))
  }

  // --- Analysis tools
  def parsOf(cc: Rep[CounterChain]): List[Int] = parParamsOf(cc).map(_.x)

  def parParamsOf(cc: Rep[CounterChain]): List[Param[Int]] = cc match {
    case Deff(Counterchain_new(ctrs,nIter)) => ctrs.map{
      case Deff(Counter_new(_,_,_,par)) => par
    }
  }

  def offsets(cc: Rep[CounterChain]) = cc match {
    case Deff(Counterchain_new(ctrs,nIter)) => ctrs.map{
      case Deff(Counter_new(start,_,_,par)) => start
    }
  }

  def ccMaxes(cc: Rep[CounterChain]) = cc match {
    case Deff(Counterchain_new(ctrs, nIter)) => ctrs.map{
      case Deff(Counter_new(start,end,_,_)) => end
    }
  }

  def isUnitCounterChain(e: Exp[Any]): Boolean = e match {
    case Deff(Counterchain_new(ctrs,_)) if ctrs.length == 1 => isUnitCounter(ctrs(0))
    case _ => false
  }

  def isUnitCounter(e: Exp[Any]): Boolean = e match {
    case Deff(Counter_new(ConstFix(0),ConstFix(1),ConstFix(1),_)) => true
    case _ => false
  }

  // TODO: Default number of iterations if bound can't be computed?
  def nIters(x: Rep[CounterChain]): Long = x match {
    case Deff(Counterchain_new(_,nIters)) => Math.ceil( bound(nIters).getOrElse(1.0) ).toLong
    case _ => 1L
  }


  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@Counter_new(s,end,t,p) => reflectPure( Counter_new(f(s),f(end),f(t),p)(pos) )(mtype(manifest[A]), pos)
    case Reflect(e@Counter_new(s,end,t,p), u, es) => reflectMirrored(Reflect(Counter_new(f(s),f(end),f(t),p)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Counterchain_new(counters,nIter) => reflectPure(Counterchain_new(f(counters),f(nIter))(e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@Counterchain_new(counters,nIter), u, es) => reflectMirrored(Reflect(Counterchain_new(f(counters),f(nIter))(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e,f)
  }
}

trait ScalaGenExternCounterOps extends ScalaGenEffect {
  val IR: ExternCounterOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DHDLCounter" => "FixedPointRange[Signed,B32,B0]"
    case "DHDLCounterChain" => "Array[FixedPointRange[Signed,B32,B0]]"
    case _ => super.remap(m)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counter_new(start,end,step,par) =>
      stream.println("val "+quote(sym)+" = "+quote(start)+" until "+quote(end)+" by "+quote(step)+" par "+quote(par))

    case e@Counterchain_new(counters, nIter) =>
      emitValDef(sym, "Array(" + counters.map(quote).mkString(", ") + ")")

    case _ => super.emitNode(sym,rhs)
  }
}

trait MaxJGenExternCounterOps extends MaxJGenEffect {
  val IR: ExternCounterOpsExp with DHDLMetadataOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DHDLCounter" => "DHDLCounter"
    case "DHDLCounterChain" => "CounterChain"
    case _ => super.remap(m)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counter_new(start,end,step,par) =>


    case e@Counterchain_new(counters, nIter) =>
//      val parent = parentOf(sym)
//      stream.println(s"""DFEVar ${quote(sym)}_en = ${quote(parent)}_en);""")
      stream.println(s"""CounterChain ${quote(sym)}_chain = control.count.makeCounterChain(${quote(sym)}_en);""")
      val pars = parsOf(sym.asInstanceOf[Sym[CounterChain]])
      counters.zipWithIndex.map { t =>
        val c = t._1
        val i = t._2
        val Def(EatReflect(Counter_new(start, end, step, p))) = c
        val par = pars(i)
        val pre = if (par == 1) "DFEVar" else "DFEVector<DFEVar>"  // <-- TODO: Better way?
        if (par == 1) {
          stream.println(s"""$pre ${quote(c)} = ${quote(sym)}_chain.addCounter(${quote(end)}, ${quote(step)});""")
        } else {
          stream.println(s"""$pre ${quote(c)} = ${quote(sym)}_chain.addCounterVect(${quote(par)}, ${quote(end)}, ${quote(step)});""")
        }
      }


    case _ => super.emitNode(sym,rhs)
  }
}
