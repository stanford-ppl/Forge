package dhdl.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.library._
import dhdl.library.classes._

trait PipeTemplateWrapper {
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
    def ldFunc(c: Rep[C[T]], i: Rep[Indices]): Rep[T] = __mem.ld(c, i)
    def stFunc(c: Rep[C[T]], i: Rep[Indices], x: Rep[T]): Rep[Unit] = __mem.st(c, i, x)

    loop(cchain, 0, Nil, {i: Rep[Indices] => stFunc(accum, i, rFunc(ldFunc(accum, i), func(i))) })
  }

  def block_reduce[T:Manifest](chain: Rep[CounterChain], accum: Rep[BRAM[T]], func: Rep[Indices] => Rep[BRAM[T]], rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[Pipeline] = {
    def ldFunc(c: Rep[BRAM[T]], i: Rep[Indices]): Rep[T] = canBramMem[T].ld(c, i)
    def stFunc(c: Rep[BRAM[T]], i: Rep[Indices], x: Rep[T]) = canBramMem[T].st(c, i, x)

    val ctrsRed = dimsOf(accum).map{dim => counter_counter_create(None, tpes_lift_to[Int,FixPt[Signed,B32,B0]](0), tpes_lift_to[Int,FixPt[Signed,B32,B0]](dim), tpes_lift_to[Int,FixPt[Signed,B32,B0]](1), 1) }
    val cchainRed = counterchain_object_apply(ctrsRed)

    //var first = true
    loop(chain, 0, Nil, {i: Rep[Indices] =>
      val part = func(i)

      loop(cchainRed, 0, Nil, {j: Rep[Indices] =>
        //if (first) stFunc(accum, j, ldFunc(part, j))
        stFunc(accum, j, rFunc(ldFunc(part, j), ldFunc(accum, j)))
      })
      //first = false
    })
  }

  def counter_new(start: Rep[FixPt[Signed,B32,B0]],end: Rep[FixPt[Signed,B32,B0]],step: Rep[FixPt[Signed,B32,B0]], par: Int)(implicit ctx: SourceContext) = {
    start until end by step
  }
  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain] = {
    counters.toArray.asInstanceOf[Rep[CounterChain]]
  }
}
