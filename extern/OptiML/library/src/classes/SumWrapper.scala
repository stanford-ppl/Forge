package optiml.library.classes

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import optiml.shared._
import optiml.shared.ops._
import optiml.shared.typeclass._
import optiml.library._
import optiml.library.classes._

trait SumWrapper {
  this: OptiMLBase with OptiMLClasses with CanSumOps =>

  def optiml_sum[A:Manifest:Arith](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A])(implicit cs: CanSum[A,A], ctx: SourceContext): Rep[A] = {
    var acc = implicitly[CanSum[A,A]].mutableA(block(start))
    var i = start+1
    while (i < end) {
      acc = implicitly[CanSum[A,A]].accA(acc, block(i))
      i += 1
    }
    acc
  }

  def optiml_sumif[R:Manifest:Arith,A:Manifest](start: Rep[Int], end: Rep[Int], cond: Rep[Int] => Rep[Boolean], block: Rep[Int] => Rep[A])(implicit cs: CanSum[R,A], ctx: SourceContext): Rep[R] = {
    var acc = null.asInstanceOf[Rep[R]]
    var init = false

    var i = start
    while (i < end) {
      if (cond(i) && !init) {
        acc = implicitly[CanSum[R,A]].mutableA(block(start))
        init = true
      }
      else if (cond(i)) {
        acc = implicitly[CanSum[R,A]].accA(acc, block(i))
      }
      i += 1
    }
    acc
  }
}
