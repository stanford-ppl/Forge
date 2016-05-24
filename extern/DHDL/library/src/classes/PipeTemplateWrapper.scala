package dhdl.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.library._
import dhdl.library.classes._

trait ControllerTemplateWrapper {
  this: DHDLBase with DHDLClasses =>

  // TODO: Better way to do this besides recursion?
  def loop(cchain: Rep[CounterChain], idx: Int, indices: List[FixPt[Signed,B32,B0]], func: Rep[Indices] => Rep[Unit]): Rep[Unit] = {
    val ctr = cchain(idx)
    if (idx >= cchain.length - 1) {
      for (i <- ctr) { func(indices_create(indices :+ i)) }
    }
    else {
      for (i <- ctr) { loop(cchain, idx+1, indices :+ i, func) }
    }
  }

  def pipe_foreach(cchain: Rep[CounterChain], func: Rep[Indices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Pipeline] = {
    loop(cchain, 0, Nil, func)
  }

  def pipe_reduce[T,C[T]](cchain: Rep[CounterChain], accum: Rep[C[T]], func: Rep[Indices] => Rep[T], rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext, __mem: Mem[T,C], __mT: Manifest[T], __cb_hk_0: Manifest[C[T]]): Rep[Pipeline] = {
    def iFunc(c: Rep[C[T]], i: Rep[Indices]): Rep[FixPt[Signed,B32,B0]] = __mem.flatIdx(c, i)
    def ldFunc(c: Rep[C[T]], i: Rep[FixPt[Signed,B32,B0]]): Rep[T] = __mem.ld(c, i)
    def stFunc(c: Rep[C[T]], i: Rep[FixPt[Signed,B32,B0]], x: Rep[T]): Rep[Unit] = __mem.st(c, i, x)

    loop(cchain, 0, Nil, {i: Rep[Indices] =>
      val idx = iFunc(accum, i)
      stFunc(accum, idx, rFunc(ldFunc(accum, idx), func(i)))
    })
  }

  def block_reduce[T:Manifest](chain: Rep[CounterChain], cchainRed: Rep[CounterChain], accum: Rep[BRAM[T]], func: Rep[Indices] => Rep[BRAM[T]], rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[Pipeline] = {
    def iFunc(c: Rep[BRAM[T]], i: Rep[Indices]): Rep[FixPt[Signed,B32,B0]] = canBramMem[T].flatIdx(c, i)
    def ldFunc(c: Rep[BRAM[T]], i: Rep[FixPt[Signed,B32,B0]]): Rep[T] = canBramMem[T].ld(c, i)
    def stFunc(c: Rep[BRAM[T]], i: Rep[FixPt[Signed,B32,B0]], x: Rep[T]) = canBramMem[T].st(c, i, x)

    //var first = true
    loop(chain, 0, Nil, {i: Rep[Indices] =>
      val part = func(i)

      loop(cchainRed, 0, Nil, {j: Rep[Indices] =>
        val idx  = iFunc(part, j)
        //if (first) stFunc(accum, j, ldFunc(part, j))
        stFunc(accum, idx, rFunc(ldFunc(part, idx), ldFunc(accum, idx)))
      })
      //first = false
    })
  }

  def counter_new(start: Rep[FixPt[Signed,B32,B0]],end: Rep[FixPt[Signed,B32,B0]],step: Rep[FixPt[Signed,B32,B0]], par: Rep[Int])(implicit ctx: SourceContext) = {
    start until end by step
  }
  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain] = {
    counters.toArray.asInstanceOf[Rep[CounterChain]]
  }
}
