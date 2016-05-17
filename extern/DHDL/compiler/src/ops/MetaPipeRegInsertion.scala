package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.SinglePassTransformer

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait MetaPipeRegInsertion extends SinglePassTransformer with PipeStageTools {
  val IR: DHDLExp with PipeStageToolsExp
  import IR.{assert => _, _}

  debugMode = true

  // Transform stages to use increasingly delayed versions of inds
  def insertRegisters[A:Manifest](owner: Sym[Any], func: Block[A], inds: List[Sym[Idx]])(implicit ctx: SourceContext) = {
    debug(s"Found MetaPipe $owner:")

    reifyBlock {
      focusBlock(func) {
        focusExactScope(func){ stms =>
          var firstStage = true // Ignore stage zero (mirror normally)
          var prevDly: List[Sym[Idx]] = inds
          var prevStage: Option[Sym[Any]] = None

          // Change substitution for the same symbol as we traverse through the block
          stms foreach {
            case stm@TP(s,d) if !isControlNode(s) || firstStage =>
              traverseStm(stm)
              if (isControlNode(s)) {
                firstStage = false
                prevStage = Some(s)
              }

            case TP(stage, stageDef) =>
              val dly = prevDly.map{ctr =>
                val reg = Reg[Idx]
                val regWrite = reg := ctr
                val regValue = reg.value
                // Set metadata for this register
                isDblBuf(reg) = true
                isDelayReg(reg) = true
                readersOf(reg) = List((stage,false,regValue))
                writerOf(reg) = (prevStage.get, false, regWrite)
                writtenIn(prevStage.get) = writtenIn(prevStage.get) :+ reg
                parentOf(reg) = owner
                childrenOf(owner) = childrenOf(owner) :+ reg
                regValue.asInstanceOf[Sym[Idx]]
              }
              prevDly = dly
              prevStage = Some(stage)

              subst ++= inds.zip(dly)
              debug(s"Mirroring stage $owner : $stage")
              val mirroredStage = self_mirror(stage, stageDef)

              val Def(origDef) = stage
              val Def(newDef) = mirroredStage
              debug(s"Finishing mirroring $owner : $stage")
              debug(s"  $stage = $origDef")
              debug(s"  $mirroredStage = $newDef")
          }
        }
      }
      f(getBlockResult(func))
    }
  }

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = rhs match {
    case Reflect(Pipe_foreach(cchain, func, inds), u, es) if styleOf(lhs) == Coarse =>
      val newFunc = insertRegisters(lhs, func, inds)(mtype(getBlockResult(func).tp),ctx)
      val newPipe = reflectMirrored(Reflect(Pipe_foreach(f(cchain),newFunc,inds)(ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
      setProps(newPipe, getProps(lhs))
      Some(newPipe)

    case Reflect(e@Pipe_fold(cchain,accum,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV), u, es) if styleOf(lhs) == Coarse =>
      val newFunc = insertRegisters(lhs, func, inds)(mtype(getBlockResult(func).tp),ctx)
      val newPipe = reflectMirrored(Reflect(Pipe_fold(f(cchain),f(accum),fA,f(iFunc),f(ld),f(st),newFunc,f(rFunc),inds,idx,acc,res,rV)(ctx, e.memC, e.mT, e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
      setProps(newPipe, getProps(lhs))
      Some(newPipe)

    case Reflect(e@Accum_fold(c1,c2,a,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV), u, es) if styleOf(lhs) == Coarse =>
      val newFunc = insertRegisters(lhs, func, inds1)(mtype(getBlockResult(func).tp),ctx)
      val newPipe = reflectMirrored(Reflect(Accum_fold(f(c1),f(c2),f(a),fA,f(iFunc),f(func),f(ld1),f(ld2),f(rFunc),f(st),inds1,inds2,idx,part,acc,res,rV)(ctx, e.memC, e.mT, e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
      setProps(newPipe, getProps(lhs))
      Some(newPipe)
    case _ => None
  }

//  override def postprocess[A:Manifest](b: Block[A]) =


}
