package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.transform.MultiPassTransformer
import scala.collection.mutable.HashMap

import dhdl.compiler._
import dhdl.compiler.ops._

trait UnitPipeTransformExp extends PipeStageToolsExp with LoweredPipeOpsExp { this: DHDLExp => }

/**
 * Inserts UnitPipe wrappers for primitive nodes in outer control nodes, along with registers for communication
 **/
trait UnitPipeTransformer extends MultiPassTransformer with PipeStageTools {
  val IR: UnitPipeTransformExp with DHDLExp
  import IR.{infix_until => _, _}

  override val name = "Unit Pipe Insertion Transformer"
  debugMode = true

  var scope = 0
  def debugs(x: => Any) { debug(".."*scope + x) }

  override def traverseBlock[A](block: Block[A]): Unit = {
    scope += 1
    super.traverseBlock(block)
    scope -= 1
  }

  // HACK: Have to get a reset value for inserted registers for writing out of unit pipes
  // TODO: Tuples? Custom records?
  private def zeroHack[T](mT: Manifest[T])(implicit ctx: SourceContext): Exp[T] = (mT match {
    case mT if isFixPtType(mT) => canFixPtNum(mT.typeArguments(0),mT.typeArguments(1),mT.typeArguments(2))
    case mT if isFltPtType(mT) => canFltPtNum(mT.typeArguments(0),mT.typeArguments(1))
    case mT if isBitType(mT) => canBitNum
    case _ => stageError("Disallowed primitive expression with non-numeric type")(ctx)
  }).zero.asInstanceOf[T]

  def wrapPrimitives[T:Manifest](blk: Block[T])(implicit ctx: SourceContext): Block[T] = {
    scope += 1

    val blk2 = reifyBlock {
      focusBlock(blk){
        focusExactScope(blk){ stms =>

          // Imperative version (functional version caused ugly scalac crash :( )
          var stages = List[List[Stm]]()
          var prevStageIsControl = true

          stms foreach { case stm@TP(s,d) =>
            if (isPrimitiveNode(s)) {
              if (prevStageIsControl) {
                stages = List(stm) +: stages
                prevStageIsControl = false
              }
              else {
                stages = (stm +: stages.head) +: stages.tail
                prevStageIsControl = false
              }
            }
            else {
              stages = List(stm) +: stages
              prevStageIsControl = true
            }
          }

          stages = stages.map(_.reverse).reverse

          val deps = stages.map{stage => stage.flatMap{case TP(s,d) => (syms(d) ++ readSyms(d)).distinct }}

          if (debugMode) {
            stages.zipWithIndex.foreach{case (stage,i) => stage.head match {
              case TP(s,d) if isPrimitiveNode(s) =>
                debugs(s"$i. Primitive nodes: ")
                stage.foreach{case TP(s,d) => debugs(s"..$s = $d") }
              case TP(s,d) => debugs(s"$i. $s = $d")
            }}
            debug("")
          }

          stages.zipWithIndex.foreach{ (stage,i) => stage.head match {
            case TP(s,d) if isPrimitiveNode(s) =>
              val calculatedSyms = stage.map{case TP(s,d) => s}
              val neededSyms = deps.drop(i+1).flatten
              // Determine which symbols escape (can be empty)
              val escapeSyms = calculatedSyms filter (neededSyms contains _)
              var regs: List[Exp[Reg[Any]]] = Nil

              Pipe {
                stage.foreach{stm => traverseStm(stm) } // Mirror each statement

                // Write all escaping symbols to newly created registers
                val mirroredEscapeSyms = escapeSyms.map{sym => f(sym)}
                regs = mirroredEscapeSyms.foreach{sym =>
                  val reg = reg_create(zeroHack(sym.tp)(mpos(sym.pos)), Regular)
                  reg := sym
                  reg
                }
              }

              // Replace substitutions of original symbol with register reads
              escapeSyms.zip(regs){case (sym,reg) => subst(sym) = reg_read(reg) }

            case TP(s,d) => self_mirror(s, d)
          }}

        }
      }
    }
    scope -= 1

    blk2
  }

  def wrapPrimitives_Hwblock(lhs: Sym[Any], rhs: Hwblock)(implicit ctx: SourceContext) = {
    debugs(s"$lhs = $rhs")
    val Hwblock(blk) = rhs
    val wrappedBlk = wrapPrimitives(f(blk))
    val pipe2 = reflectEffect(Hwblock(wrappedBlk)(ctx), summarizeEffects(blk) andAlso Simple())
    setProps(pipe2, getProps(lhs))
    pipe2
  }
  def wrapPrimitives_UnitPipe(lhs: Sym[Any], rhs: Unit_pipe)(implicit ctx: SourceContext) = {
    debugs(s"$lhs = $rhs")
    val Unit_pipe(func) = rhs
    val wrappedBlk = wrapPrimitives(f(func))
    val pipe2 = reflectEffect(Unit_pipe(wrappedBlk)(ctx), summarizeEffects(wrappedBlk) andAlso Simple())
    setProps(pipe2, getProps(lhs))
    pipe2
  }

  def wrapPrimitives_PipeForeach(lhs: Sym[Any], rhs: Pipe_foreach)(implicit ctx: SourceContext) = {
    debugs(s"$lhs = $rhs")
    val Pipe_foreach(cchain, func, inds) = rhs
    val wrappedBlk = wrapPrimitives(f(func))
    val pipe2 = reflectEffect(Pipe_foreach(f(cchain), wrappedBlk, inds)(ctx), summarizeEffects(wrappedBlk).star andAlso Simple())
    setProps(pipe2, getProps(lhs))
    pipe2
  }

  def wrapPrimitives_PipeFold[T,C[T]](lhs: Sym[Any], rhs: Pipe_fold[T,C])(implicit ctx: SourceContext, memC: Mem[T,C], numT: Num[T], mT: Manifest[T], mC: Manifest[C[T]]) = {
    debugs(s"$lhs = $rhs")
    val Pipe_fold(cchain,accum,zero,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) = rhs
    val accum2 = f(accum)
    val mBlk = wrapPrimitives(f(func))
    val iBlk = f(iFunc)
    val ldBlk = f(ld)
    val stBlk = f(st)
    val rBlk = f(rFunc)

    val effects = summarizeEffects(iBlk) andAlso summarizeEffects(mBlk) andAlso summarizeEffects(ldBlk) andAlso
                  summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum2.asInstanceOf[Sym[C[T]]]))

    val pipe2 = reflectEffect(Pipe_fold(f(cchain),accum2,f(zero),fA,iBlk,ldBlk,stBlk,mBlk,rBlk,inds,idx,acc,res,rV)(ctx,memC,numT,mT,mC), effects.star)
    setProps(pipe2, getProps(lhs))
    pipe2
  }

  def wrapPrimitives_AccumFold[T,C[T]](lhs: Sym[Any], rhs: Accum_fold[T,C])(implicit ctx: SourceContext, memC: Mem[T,C], numT: Num[T], mT: Manifest[T], mC: Manifest[C[T]]) = {
    debugs(s"$lhs = $rhs")
    val Accum_fold(ccOuter,ccInner,accum,zero,fA,iFunc,func,ld1,ld2,rFunc,st,indsOuter,indsInner,idx,part,acc,res,rV) = rhs
    val accum2 = f(accum)
    val iBlk = f(iFunc)
    val mBlk = wrapPrimitives(f(func))
    val ldPartBlk = f(ld1)
    val ldBlk = f(ld2)
    val rBlk = f(rFunc)
    val stBlk = f(st)

    val effects = summarizeEffects(iBlk) andAlso summarizeEffects(mBlk) andAlso summarizeEffects(ldPartBlk) andAlso
                  summarizeEffects(ldBlk) andAlso summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum2.asInstanceOf[Sym[C[T]]]))

    val pipe2 = reflectEffect(Accum_fold(f(ccOuter),f(ccInner),accum2,f(zero),fA,iBlk,mBlk,ldPartBlk,ldBlk,rBlk,stBlk,indsOuter,indsInner,idx,part,acc,res,rV)(ctx,memC,numT,mT,mC), effects.star)
    setProps(pipe2, getProps(lhs))
    pipe2
  }

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = rhs match {
    case EatReflect(Pipe_parallel(func)) => None

    case EatReflect(e: Hwblock) if isOuterControl(lhs) => Some(wrapPrimitives_Hwblock(lhs, e)(e.__pos))
    case EatReflect(e: Unit_pipe) if isOuterControl(lhs) => Some(wrapPrimitives_UnitPipe(lhs, e)(e.__pos))
    case EatReflect(e: Pipe_foreach) if isOuterControl(lhs) => Some(wrapPrimitives_PipeForeach(lhs, e)(e.ctx))
    case EatReflect(e: Pipe_fold[_,_]) if isOuterControl(lhs) => Some(wrapPrimitives_PipeFold(lhs, e)(e.ctx, e.memC, e.numT, e.mT, e.mC))
    case EatReflect(e: Accum_fold[_,_]) if isOuterControl(lhs) => Some(wrapPrimitives_AccumFold(lhs, e)(e.ctx, e.memC, e.numT, e.mT, e.mC))
    case _ => None
  }
}
