package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
//import ppl.delite.framework.transform.TunnelingTransformer
import scala.virtualization.lms.common.ForwardTransformer

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait MetaPipeRegInsertion extends ForwardTransformer with PipeStageTools {
  val IR: DHDLExp with PipeStageToolsExp
  import IR._

  override val debugMode = true

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(sym, rhs) if apply(sym) == sym =>
      val replace = transform(sym, rhs)(mtype(sym.tp),mpos(sym.pos)).getOrElse(self_mirror(sym,rhs))
      assert(!subst.contains(sym) || subst(sym) == replace)
      if (sym != replace) subst += (sym -> replace) // record substitution only if result is different

    // Someone else has already mirrored/transformed us!
    // Assumed case: Some higher scope has a block which includes us, and they've already gone through and
    // mirrored some or all of the nodes in that block before traversing the block
    // The correct thing to do here is mirror the previously transformed node, then scrub the intermediate node from
    // the IR def and context lists so it doesn't appear in any effects lists.
    case TP(sym, rhs) =>
      val sym2 = apply(sym)
      val replace = sym2 match {
        case Def(rhs2) =>
          transform(sym2.asInstanceOf[Sym[Any]], rhs2)(mtype(sym2.tp),mpos(sym2.pos)).getOrElse(mirrorExp(sym2))
        case _ => sym2
      }
      if (replace != sym2 && sym != sym2) scrubSym(sym2.asInstanceOf[Sym[Any]])
      if (sym != replace) subst += (sym -> replace) // Change substitution from sym -> sym2 to sym -> replace
  }


  private def mirrorExp[A](e: Exp[A]) = e match {
    case Def(d) => self_mirror(e.asInstanceOf[Sym[A]], d.asInstanceOf[Def[A]])
    case _ => e
  }

  // Transform stages to use increasingly delayed versions of inds
  def insertRegisters[A:Manifest](owner: Sym[Any], func: Block[A], inds: List[Sym[Idx]])(implicit ctx: SourceContext) = {
    val stages = getControlNodes(func)

    reifyBlock {
      var prevDly: List[Sym[Idx]] = inds
      var delayChain = List[List[Sym[Idx]]](inds)
      val newStages = stages.zipWithIndex.drop(1).map{ case (stage,i) =>
        val dly = prevDly.map{ctr =>
          val reg = Reg[Idx]
          reg := ctr
          // Set metadata for this register
          isDblBuf(reg) = true
          readersOf(reg) = List((stage,false))
          writerOf(reg) = stages(i - 1)
          writtenIn(stages(i-1)) = writtenIn(stages(i - 1)) :+ reg
          parentOf(reg) = owner
          childrenOf(owner) = childrenOf(owner) :+ reg
          reg.value.asInstanceOf[Sym[Idx]]
        }
        delayChain ::= dly
        prevDly = dly

        withSubstScope(inds.zip(dly):_*){ mirrorExp(stage) }
      }

      withSubstScope(stages.drop(1).zip(newStages):_*){
        traverseBlock(func)
        apply(getBlockResult(func))
      }
    }
  }

  def f = this.asInstanceOf[Transformer]

  def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = rhs match {
    case Reflect(Pipe_foreach(cchain, func, inds), u, es) if styleOf(lhs) == Coarse =>
      val newFunc = insertRegisters(lhs, func, inds)(mtype(getBlockResult(func).tp),ctx)
      val newPipe = reflectMirrored(Reflect(Pipe_foreach(f(cchain),newFunc,inds)(ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
      setProps(newPipe, getProps(lhs))
      Some(newPipe)

    case Reflect(e@Pipe_reduce(cchain,accum,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV), u, es) if styleOf(lhs) == Coarse =>
      val newFunc = insertRegisters(lhs, func, inds)(mtype(getBlockResult(func).tp),ctx)
      val newPipe = reflectMirrored(Reflect(Pipe_reduce(f(cchain),f(accum),f(iFunc),f(ld),f(st),newFunc,f(rFunc),inds,idx,acc,res,rV)(ctx, e.memC, e.mT, e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
      setProps(newPipe, getProps(lhs))
      Some(newPipe)

    case Reflect(e@Block_reduce(c1,c2,a,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV), u, es) if styleOf(lhs) == Coarse =>
      val newFunc = insertRegisters(lhs, func, inds1)(mtype(getBlockResult(func).tp),ctx)
      val newPipe = reflectMirrored(Reflect(Block_reduce(f(c1),f(c2),f(a),f(iFunc),f(func),f(ld1),f(ld2),f(rFunc),f(st),inds1,inds2,idx,part,acc,res,rV)(e.mT,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
      setProps(newPipe, getProps(lhs))
      Some(newPipe)
    case _ => None
  }


}
