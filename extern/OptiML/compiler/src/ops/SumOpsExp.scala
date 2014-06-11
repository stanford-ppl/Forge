package optiml.compiler.ops

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import ppl.delite.framework.ops.DeliteCollection
import ppl.delite.framework.datastructures._
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.Util._
import optiml.shared._
import optiml.shared.ops._
import optiml.shared.typeclass._
import optiml.compiler._
import optiml.compiler.ops._

trait SumOpsExp extends SumOps with BaseFatExp with EffectExp {
  this: OptiMLExp =>

   case class Sum[A:Manifest:Arith](start: Exp[Int], end: Exp[Int], map: Exp[Int] => Exp[A], init: Exp[A])(implicit canSum: CanSum[A,A], ctx: SourceContext)
     extends DeliteOpMapReduce[Int,A] {

     override val mutable = true // can we do this automatically?

     val in = copyTransformedOrElse(_.in)(IndexVector(start,end))
     val size = copyTransformedOrElse(_.size)(end - start)
     def zero = a.zero(init)
     override def accInit = canSum.mutableA(a.zero(init))
     def reduce = (a,b) => canSum.accA(a,b)

     def m = manifest[A]
     def a = implicitly[Arith[A]]
     def cs = implicitly[CanSum[A,A]]
     def sc = implicitly[SourceContext]
   }

  case class SumIf[R:Manifest:Arith,A:Manifest](start: Exp[Int], end: Exp[Int], co: Exp[Int] => Exp[Boolean], fu: Exp[Int] => Exp[A])(implicit canSum: CanSum[R,A], ctx: SourceContext) // TODO aks: CS into Arith
    extends DeliteOpFilterReduceFold[R] {

    override val mutable = true // can we do this automatically?

    val in = copyTransformedOrElse(_.in)(IndexVector(start,end))
    val size = copyTransformedOrElse(_.size)(end - start)
    val zero = (copyTransformedBlockOrElse(_.zero._1)(reifyEffects(a.empty)),copyTransformedBlockOrElse(_.zero._2)(reifyEffects(unit(-1))))

    def func = (v) => (zero._1, reifyEffects(v)) // will copy block that is zero._1, not just reference result!

    // FOR REDUCE SEQ
    // a._1 = accumulator
    // a._2 = zero_2 = initialization check: -1 if uninitialized, >= 0 otherwise
    // b._1 = unused
    // b._2 = loop index

    // FOR REDUCE PAR
    // a._1 = accumulator
    // a._2 = act zero_2 = initialization check: -1 if uninitialized, >= 0 otherwise
    // b._1 = next value to reduce
    // b._2 = rhs zero_2 = initialization check: -1 if uninitialized, >= 0 otherwise


    // this is the reduce used inside each chunk (R,A) => R
    def reduceSeq = (a,b) => (reifyEffects(if (co(b._2)) { if (a._2 >= unit(0)) canSum.accA(a._1, fu(b._2)) else canSum.mutableA(fu(b._2)) } else a._1),
                              reifyEffects(if (co(b._2)) b._2 else a._2 )) // would not work in parallel...  // returns the current index (v) if the condition is true, or a._2, which is defaulted to -1 (uninitialized)

    // this is the reduce used in the tree (R,R) => R
    def reducePar = (a,b) => (reifyEffects(if (b._2 >= unit(0)) { if (a._2 >= unit(0)) canSum.accR(a._1, b._1) else canSum.mutableR(b._1) } else a._1),
                              reifyEffects(if (b._2 >= unit(0)) b._2 else a._2))

    def mR = manifest[R]
    def a = implicitly[Arith[R]]
    def mA = manifest[A]
    def cs = implicitly[CanSum[R,A]]
    def sc = implicitly[SourceContext]
  }

  def optiml_sum[A:Manifest:Arith](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A])(implicit cs: CanSum[A,A], ctx: SourceContext) = {
    // don't add it back in, just re-compute it to avoid the peeled iteration problems
    val zero = block(start)
    reflectPure(Sum(start,end,block,zero))
  }

  def optiml_sumif[R:Manifest:Arith,A:Manifest](start: Exp[Int], end: Exp[Int], cond: Exp[Int] => Exp[Boolean], block: Exp[Int] => Exp[A])(implicit cs: CanSum[R,A], ctx: SourceContext) = {
    reflectPure(SumIf[R,A](start, end, cond, block))
  }


  /**
   * Mirroring
   */
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    e match {
      case e@Sum(st,en,b,init) => reflectPure(new { override val original = Some(f,e) } with Sum(f(st),f(en),f(b),f(init))(mtype(e.m),atype(e.a),cstype(e.cs),e.sc))(mtype(manifest[A]), implicitly[SourceContext])
      case e@SumIf(st,en,c,b) => reflectPure(new { override val original = Some(f,e) } with SumIf(f(st),f(en),f(c),f(b))(mtype(e.mR),atype(e.a),mtype(e.mA),cstype(e.cs),e.sc))(mtype(manifest[A]), implicitly[SourceContext])
      case Reflect(e@Sum(st,en,b,init), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Sum(f(st),f(en),f(b),f(init))(mtype(e.m), atype(e.a), cstype(e.cs), e.sc), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
      case Reflect(e@SumIf(st,en,c,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SumIf(f(st),f(en),f(c),f(b))(mtype(e.mR),atype(e.a),mtype(e.mA),cstype(e.cs),e.sc), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
      case _ => super.mirror(e, f)
  }}.asInstanceOf[Exp[A]] // why??
}

// these need to exist for externs, even though we don't need them for sum
trait ScalaGenSumOps
trait CudaGenSumOps
trait OpenCLGenSumOps
trait CGenSumOps

