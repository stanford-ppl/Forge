package optiql.compiler.ops

import scala.virtualization.lms.common.StructOps
import scala.reflect.{RefinedManifest,SourceContext}
import optiql.compiler._
import optiql.shared.ops._

trait RewriteOpsExp extends RewriteOps with TableOpsExp {
  this: OptiQLExp =>

  def upgradeInt[R:Manifest](value: Rep[Int]): Rep[R] = value.asInstanceOf[Rep[R]]

  def groupByHackImpl[K:Manifest,V:Manifest](self: Rep[Table[V]], keySelector: Rep[V] => Rep[K])(implicit pos: SourceContext): Rep[Table[Tup2[K,Table[V]]]] = {
    throw new RuntimeException("groupBy not implemented")
  }

  override def table_groupby[A:Manifest,K:Manifest](self: Rep[Table[A]],keySelector: (Rep[A]) => Rep[K])(implicit __pos: SourceContext): Rep[Table[Tup2[K,Table[A]]]] = self match {
    case Def(Table_Where(origS, predicate)) =>
      reflectPure(Table_GroupByWhere(origS, keySelector, predicate))
    case _ => super.table_groupby(self,keySelector)
  }

  case class Table_GroupByWhere[T:Manifest, K:Manifest](in: Exp[Table[T]], keyFunc: Exp[T] => Exp[K], cond: Exp[T] => Exp[Boolean]) extends Def[Table[Tup2[K,Table[T]]]] {
    val mT = manifest[T]
    val mK = manifest[K]
  }

  private def hashReduce[A:Manifest,K:Manifest,T:Manifest,R:Manifest](resultSelector: Exp[T] => Exp[R], keySelector: Exp[A] => Exp[K]): Option[(Exp[A]=>Exp[R], (Exp[R],Exp[R])=>Exp[R], (Exp[R],Exp[Int])=>Exp[R])] = {
    var failed = false
    val ctx = implicitly[SourceContext]
    def rewriteMap(value: Exp[Any]) = (value match {
      case Def(Field(Def(Field(s,"_1")),index)) => (a:Exp[A]) => field(keySelector(a),index)(value.tp,ctx)
      case Def(Table_Sum(s, sumSelector)) => sumSelector
      case Def(Table_Average(s, avgSelector)) => avgSelector
      case Def(Table_Count(s)) => (a:Exp[A]) => unit(1)
      case Def(a) => printlog("found unknown: " + a.toString); failed = true; null
      case _ => printlog("found unknown: " + value.toString); failed = true; null
    }).asInstanceOf[Exp[A]=>Exp[R]]

    def rewriteReduce[N](value: Exp[Any]) = (value match {
      case Def(Field(s,"_1")) => (a:Exp[N],b:Exp[N]) => a
      case Def(Field(Def(Field(s,"_1")),index)) => (a:Exp[N],b:Exp[N]) => a
      case Def(d@Table_Sum(_,_)) => (a:Exp[N],b:Exp[N]) => numeric_pl(a,b)(ntype(d._numR),mtype(d._mR),ctx)
      case Def(d@Table_Average(_,_)) => (a:Exp[N],b:Exp[N]) => numeric_pl(a,b)(ntype(d._numR),mtype(d._mR),ctx)
      case Def(d@Table_Count(s)) => (a:Exp[N],b:Exp[N]) => numeric_pl(a,b)(ntype(implicitly[Numeric[Int]]),mtype(manifest[Int]),ctx)
      case _ => failed = true; null
    }).asInstanceOf[(Exp[N],Exp[N])=>Exp[N]]

    def rewriteAverage[N](value: Exp[Any]) = (value match {
      case Def(d@Table_Average(_,_)) => (a:Exp[N],count:Exp[Int]) => fractional_div(a, count.asInstanceOf[Exp[N]])(mtype(d._mR),frtype(d._fracR),mtype(d._mR),ctx,implicitly[Rep[N]=>Rep[N]])
      case _ => (a:Exp[N],count:Exp[N]) => a
    }).asInstanceOf[(Exp[N],Exp[Int])=>Exp[N]]


    val funcs = resultSelector(fresh[T]) match {
      case Def(Struct(tag: StructTag[R], elems)) =>
        val valueFunc = (a:Exp[A]) => struct[R](tag, elems map { case (key, value) => (key, rewriteMap(value)(a)) })
        val reduceFunc = (a:Exp[R],b:Exp[R]) => struct[R](tag, elems map { case (key, value) => (key, rewriteReduce(value)(field(a,key)(value.tp,ctx), field(b,key)(value.tp,ctx))) })
        val averageFunc = (a:Exp[R],count:Exp[Int]) => struct[R](tag, elems map { case (key, value) => (key, rewriteAverage(value)(field(a,key)(value.tp,ctx), count)) })
        (valueFunc, reduceFunc, averageFunc)

      case a => (rewriteMap(a), rewriteReduce[R](a), rewriteAverage[R](a))
    }

    if (failed) None else Some(funcs)
  }

  override def table_select[A:Manifest,R:Manifest](self: Rep[Table[A]], resultSelector: (Rep[A]) => Rep[R])(implicit __pos: SourceContext): Exp[Table[R]] = self match {
    case Def(g@Table_GroupByWhere(origS: Exp[Table[a]], keySelector, cond)) => hashReduce(resultSelector, keySelector)(g.mT,g.mK,manifest[A],manifest[R]) match {
      case Some((valueFunc, reduceFunc, averageFunc)) =>
        val hr = groupByReduce(origS, keySelector, valueFunc, reduceFunc, cond)(g.mT,g.mK,manifest[R],implicitly[SourceContext])
        val count = groupByReduce(origS, keySelector, (e:Exp[a]) => unit(1), (a:Exp[Int],b:Exp[Int])=>primitive2_forge_int_plus(a,b), cond)(g.mT,g.mK,manifest[Int],implicitly[SourceContext])
        bulkDivide(hr, count, averageFunc)(manifest[R],implicitly[SourceContext])
      case None =>
        Console.println("ERROR: unable to fuse GroupBy-Select")
        return super.table_select(self, resultSelector)
    }
    case _ => super.table_select(self, resultSelector)
  }

  def zeroType[T:Manifest]: Exp[T] = manifest[T] match { //need a more robust solution, e.g. type class
    case r: RefinedManifest[T] => struct[T](AnonTag(r), r.fields.map(e => (e._1, zeroType(e._2))))
    case s if s == manifest[String] => unit("").asInstanceOf[Rep[T]]
    case v if v <:< manifest[AnyVal] => cast_asinstanceof[Int,T](unit(0))
    case o => throw new IllegalArgumentException("Unknown Numeric type: " + o)
  }

}

// these need to exist for externs, even though we don't need them
trait ScalaGenRewriteOps
trait CudaGenRewriteOps
trait OpenCLGenRewriteOps
trait CGenRewriteOps
