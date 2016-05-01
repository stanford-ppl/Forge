package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.MultiPassTransformer

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait MetaPipeRegInsertion extends MultiPassTransformer with PipeStageTools {
  val IR: DHDLExp with PipeStageToolsExp
  import IR.{assert => _, _}

  override val debugMode = true

  // Transform stages to use increasingly delayed versions of inds
  def insertRegisters[A:Manifest](owner: Sym[Any], func: Block[A], inds: List[Sym[Idx]])(implicit ctx: SourceContext) = {
    val stages = getControlNodes(func)

    // Set up transformed versions of each child
    reifyBlock {
      var prevDly: List[Sym[Idx]] = inds
      //var delayChain = List[List[Sym[Idx]]](inds) [Unused]
      val newStages = stages.zipWithIndex.drop(1).map{ case (stage,i) =>
        val dly = prevDly.map{ctr =>
          val reg = Reg[Idx]
          reg := ctr
          // Set metadata for this register
          isDblBuf(reg) = true
          isDelayReg(reg) = true
          readersOf(reg) = List((stage,false))
          writerOf(reg) = stages(i - 1)
          writtenIn(stages(i-1)) = writtenIn(stages(i - 1)) :+ reg
          parentOf(reg) = owner
          childrenOf(owner) = childrenOf(owner) :+ reg
          reg.value.asInstanceOf[Sym[Idx]]
        }
        //delayChain ::= dly
        prevDly = dly

        withSubstScope(inds.zip(dly):_*){ mirrorExp(stage) }
      }

      // Traverse the block to add the transformed versions to the IR
      withSubstScope(stages.drop(1).zip(newStages):_*){
        traverseBlock(func)
        apply(getBlockResult(func))
      }
    }
  }


  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = rhs match {
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
