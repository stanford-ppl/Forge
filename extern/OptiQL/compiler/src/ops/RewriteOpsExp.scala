package optiql.compiler.ops

import scala.virtualization.lms.common.{StructOps,ScalaGenFat,CGenFat}
import scala.reflect.{RefinedManifest,SourceContext}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform.MultiloopSoATransformWithReduceExp
import optiql.compiler._
import optiql.shared.ops._

//extra traits from Delite that we want to include that aren't pulled in by default with Forge
trait DeliteOptiQLExtraExp //extends MultiloopSoATransformWithReduceExp //TODO: Forge's KeysDistinct doesn't transform

trait RewriteOpsExp extends RewriteOps with TableOpsExp with DeliteOptiQLExtraExp {
  this: OptiQLExp =>

  def groupByHackImpl[K:Manifest,V:Manifest](self: Rep[Table[V]], keySelector: Rep[V] => Rep[K])(implicit pos: SourceContext): Rep[Table[Tup2[K,Table[V]]]] = {
    val map = DeliteArrayBuffer(table_raw_data(self), table_size(self)).groupBy(keySelector)
    val arr = dmap_keys(map).zip(dmap_values(map)){ (k,v) => pack(k, table_object_apply(darray_buffer_unsafe_result(v), darray_buffer_length(v))) }
    table_object_apply(arr, darray_length(arr))
  }

  def sortHackImpl[A:Manifest](self: Rep[Table[A]], comparator: (Rep[A],Rep[A]) => Rep[Int])(implicit pos: SourceContext): Rep[Table[A]] = {
    val indices = DeliteArray.sortIndices(self.size)((i:Rep[Int],j:Rep[Int]) => comparator(self(i), self(j)))
    val sorted = DeliteArray.fromFunction(self.size)(i => table_apply(self, darray_apply(indices, i)))
    Table(sorted, self.size)
  }

  case class CompareHack[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends Def[Int] {
    def mev = manifest[T]
    def aev = implicitly[Ordering[T]]
  }

  def compareHackImpl[A:Manifest:Ordering](lhs: Rep[A], rhs: Rep[A]): Rep[Int] = {
    CompareHack(lhs, rhs)
  }


  //TODO: this special-cases filter fusion (only for groupBy); LMS fusion should take care of it generically for us
  override def table_groupby[A:Manifest,K:Manifest](self: Rep[Table[A]],keySelector: (Rep[A]) => Rep[K])(implicit __pos: SourceContext): Rep[Table[Tup2[K,Table[A]]]] = self match {
    case Def(Table_Where(origS, predicate)) =>
      reflectPure(Table_GroupByWhere(origS, keySelector, predicate))
    case _ => super.table_groupby(self,keySelector)
  }

  case class Table_GroupByWhere[A:Manifest, K:Manifest](in: Exp[Table[A]], keyFunc: Exp[A] => Exp[K], cond: Exp[A] => Exp[Boolean]) extends Def[Table[Tup2[K,Table[A]]]] {
    val _mA = manifest[A]
    val _mK = manifest[K]
  }

  private def hashReduce[A:Manifest,K:Manifest,T:Manifest,R:Manifest](resultSelector: Exp[T] => Exp[R], keySelector: Exp[A] => Exp[K]): Option[(Exp[A]=>Exp[R], (Exp[R],Exp[R])=>Exp[R], (Exp[R],Exp[Int])=>Exp[R])] = {
    var failed = false
    val ctx = implicitly[SourceContext]
    def rewriteMap(value: Exp[Any]) = (value match {
      case Def(Field(Def(Field(s,"_1")),index)) => (a:Exp[A]) => field(keySelector(a),index)(value.tp,ctx)
      case Def(Field(s,"_1")) => keySelector
      case Def(Table_Sum(s, sumSelector)) => sumSelector
      case Def(Table_Average(s, avgSelector)) => avgSelector
      case Def(Table2_Count(s)) => (a:Exp[A]) => unit(1)
      case Def(Table_Max(s, maxSelector)) => maxSelector
      case Def(Table_Min(s, minSelector)) => minSelector
      case Def(a) => printlog("found unknown: " + a.toString); failed = true; null
      case _ => printlog("found unknown: " + value.toString); failed = true; null
    }).asInstanceOf[Exp[A]=>Exp[R]]

    def rewriteReduce[N](value: Exp[Any]) = (value match {
      case Def(Field(Def(Field(s,"_1")),index)) => (a:Exp[N],b:Exp[N]) => a
      case Def(Field(s,"_1")) => (a:Exp[N],b:Exp[N]) => a
      case Def(d@Table_Sum(_,_)) => (a:Exp[N],b:Exp[N]) => numeric_pl(a,b)(ntype(d._numR),mtype(d._mR),ctx)
      case Def(d@Table_Average(_,_)) => (a:Exp[N],b:Exp[N]) => numeric_pl(a,b)(ntype(d._numR),mtype(d._mR),ctx)
      case Def(d@Table2_Count(s)) => (a:Exp[N],b:Exp[N]) => numeric_pl(a,b)(ntype(implicitly[Numeric[Int]]),mtype(manifest[Int]),ctx)
      case Def(d@Table_Max(_,_)) => (a:Exp[N],b:Exp[N]) => ordering_max(a,b)(otype(d._ordR),mtype(d._mR),ctx)
      case Def(d@Table_Min(_,_)) => (a:Exp[N],b:Exp[N]) => ordering_min(a,b)(otype(d._ordR),mtype(d._mR),ctx)
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
    //case Def(QueryableWhere(origS, predicate)) => //Where-Select fusion
    //  QueryableSelectWhere(origS, resultSelector, predicate)
    case Def(g@Table_GroupBy(origS: Exp[Table[a]], keySelector)) => hashReduce(resultSelector, keySelector)(g._mA,g._mK,manifest[A],manifest[R]) match {
      case Some((valueFunc, reduceFunc, averageFunc)) =>
        //Console.err.println("fused GroupBy-Select")
        val hr = groupByReduce(origS, keySelector, valueFunc, reduceFunc, (e:Exp[a]) => unit(true))(g._mA,g._mK,manifest[R],implicitly[SourceContext])
        val count = groupByReduce(origS, keySelector, (e:Exp[a]) => unit(1), (a:Exp[Int],b:Exp[Int])=>forge_int_plus(a,b), (e:Exp[a])=>unit(true))(g._mA,g._mK,manifest[Int],implicitly[SourceContext])
        bulkDivide(hr, count, averageFunc)(manifest[R],implicitly[SourceContext])
      case None =>
        Console.err.println("WARNING: unable to fuse GroupBy-Select")
        return super.table_select(self, resultSelector)
    }
    case Def(g@Table_GroupByWhere(origS: Exp[Table[a]], keySelector, cond)) => hashReduce(resultSelector, keySelector)(g._mA,g._mK,manifest[A],manifest[R]) match {
      case Some((valueFunc, reduceFunc, averageFunc)) =>
        //Console.err.println("fused GroupBy-Select")
        val hr = groupByReduce(origS, keySelector, valueFunc, reduceFunc, cond)(g._mA,g._mK,manifest[R],implicitly[SourceContext])
        val count = groupByReduce(origS, keySelector, (e:Exp[a]) => unit(1), (a:Exp[Int],b:Exp[Int])=>forge_int_plus(a,b), cond)(g._mA,g._mK,manifest[Int],implicitly[SourceContext])
        bulkDivide(hr, count, averageFunc)(manifest[R],implicitly[SourceContext])
      case None =>
        Console.err.println("WARNING: unable to fuse GroupBy-Select")
        return super.table_select(self, resultSelector)
    }
    case _ => super.table_select(self, resultSelector)
  }

  ///////

  def table_printastable[A:Manifest](self: Rep[Table[A]],maxRows: Rep[Int] = unit(100))(implicit __pos: SourceContext) = {
    reflectEffect(Table_PrintAsTable[A](self,maxRows)(implicitly[Manifest[A]],__pos))
  }
  def table_writeasjson[A:Manifest](self: Rep[Table[A]],path: Rep[String])(implicit __pos: SourceContext) = {
    reflectEffect(Table_WriteAsJSON[A](self,path)(implicitly[Manifest[A]],__pos))
  }

  case class Table_PrintAsTable[A:Manifest](self: Rep[Table[A]],maxRows: Rep[Int] = unit(100))(implicit val __pos: SourceContext) extends Def[Unit] {
    val _mA = implicitly[Manifest[A]]
  }

  case class Table_WriteAsJSON[A:Manifest](self: Rep[Table[A]],path: Rep[String])(implicit val __pos: SourceContext) extends Def[Unit] {
    val _mA = implicitly[Manifest[A]]
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case mn@Table_PrintAsTable(__arg0,__arg1) => table_printastable(f(__arg0),f(__arg1))(mtype(mn._mA),mn.__pos)
    case Reflect(mn@Table_PrintAsTable(__arg0,__arg1), u, es) => reflectMirrored(Reflect(Table_PrintAsTable(f(__arg0),f(__arg1))(mtype(mn._mA),mn.__pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case mn@Table_WriteAsJSON(__arg0,__arg1) => table_writeasjson(f(__arg0),f(__arg1))(mtype(mn._mA),mn.__pos)
    case Reflect(mn@Table_WriteAsJSON(__arg0,__arg1), u, es) => reflectMirrored(Reflect(Table_WriteAsJSON(f(__arg0),f(__arg1))(mtype(mn._mA),mn.__pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case e@CompareHack(a,b) => reflectPure(CompareHack(f(a),f(b))(e.aev,e.mev))(mtype(manifest[A]), pos)
    case Reflect(e@CompareHack(a,b), u, es) => reflectMirrored(Reflect(CompareHack(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]

}

// these need to exist for externs, even if we don't need them
trait ScalaGenRewriteOps extends ScalaGenFat {
  val IR: RewriteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case mn@Table_PrintAsTable(self,maxRows) =>
      stream.println("val "+quote(sym)+" = {")
      stream.print("TablePrinter.printAsTable("+quote(self)+", "+quote(maxRows)+")")
      stream.println("}")

    case mn@Table_WriteAsJSON(self,path) =>
      stream.println("val "+quote(sym)+" = {")
      stream.print("TablePrinter.writeAsJSON("+quote(self)+", "+quote(path)+")")
      stream.println("}")

    // Unfortunately duplicated from LMS OrderingOps now, since we no longer include OrderingOps in Forge DSLs.
    // Need to come up with a good way of code-generating this type of implementation from a Forge spec
    // (it should be implemented in Forge's Scala.scala).
    case c@CompareHack(a,b) => c.mev match {
      case m if m == Manifest.Int => emitValDef(sym, "java.lang.Integer.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Long => emitValDef(sym, "java.lang.Long.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Double => emitValDef(sym, "java.lang.Double.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Float => emitValDef(sym, "java.lang.Float.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Boolean => emitValDef(sym, "java.lang.Boolean.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Byte => emitValDef(sym, "java.lang.Byte.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Char => emitValDef(sym, "java.lang.Character.compare("+quote(a)+","+quote(b)+")")
      case m if m == Manifest.Short => emitValDef(sym, "java.lang.Short.compare("+quote(a)+","+quote(b)+")")
      case _ => emitValDef(sym, quote(a) + " compare " + quote(b))
    }

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenRewriteOps extends CGenFat {
  val IR: RewriteOpsExp
  import IR._

  // TODO: Add robust comparators for float and double
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case c@CompareHack(a,b) => c.mev match {
      case m if m == Manifest.Int => emitValDef(sym, quote(a)+"-"+quote(b))
      case m if m == Manifest.Long => emitValDef(sym, quote(a)+"-"+quote(b))
      case m if m == Manifest.Double => emitValDef(sym, quote(a)+"-"+quote(b))
      case m if m == Manifest.Float => emitValDef(sym, quote(a)+"-"+quote(b))
      case m if m == Manifest.Boolean => emitValDef(sym, quote(a)+"-"+quote(b))
      case m if m == Manifest.Byte => emitValDef(sym, quote(a)+"-"+quote(b))
      case m if m == Manifest.Char => emitValDef(sym, quote(a)+"-"+quote(b))
      case m if m == Manifest.Short => emitValDef(sym, quote(a)+"-"+quote(b))
      case _ => super.emitNode(sym, rhs)
    }
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenRewriteOps
trait OpenCLGenRewriteOps
