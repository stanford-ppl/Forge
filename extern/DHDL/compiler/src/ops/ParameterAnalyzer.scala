package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{Expressions,Traversal}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import scala.collection.mutable.{HashMap,ArrayBuffer}

trait ParamRestrictions extends Expressions {
  this: DHDLMetadataOpsExp with GenOverloadHack =>

  private def qt(x: Param[_]) = {
    val name = nameOf(x)
    if (name == "") s"$x" else name
  }

  type CRange = scala.collection.immutable.Range

  trait Restrict {this: Product =>
    def evaluate: Boolean
    def deps = this.productIterator.flatMap{
      case p: Param[Int] => List(p)
      case x: List[_] => x.flatMap{case p: Param[Int] => Some(p); case _ => None}
      case _ => Nil
    }
    def dependsOnlyOn(x: Param[Int]*) = {
      val d = deps.toList.distinct
      val c = x.toList.distinct
      d.length == c.length && c.forall(d contains _)
    }
  }

  case class RLess(a: Param[Int], b: Param[Int]) extends Restrict {
    def evaluate = a.x < b.x
    override def toString = s"${qt(a)} < ${qt(b)}"
  }
  case class RLessEqual(a: Param[Int], b: Param[Int]) extends Restrict {
    def evaluate = a.x <= b.x
    override def toString = s"${qt(a)} <= ${qt(b)}"
  }
  case class RDivides(a: Param[Int], b: Param[Int]) extends Restrict {
    def evaluate = b.x % a.x == 0
    override def toString = s"${qt(a)} divides ${qt(b)}"
  }
  case class RDividesConst(a: Param[Int], b: Int) extends Restrict {
    def evaluate = b % a.x == 0
    override def toString = s"${qt(a)} divides $b"
  }
  case class RDividesQuotient(a: Param[Int], n: Int, d: Param[Int]) extends Restrict {
    def evaluate = {
      val q = Math.ceil(n.toDouble / d.x).toInt
      a.x < q && (q % a.x == 0)
    }
    override def toString = s"${qt(a)} divides $n/${qt(d)}"
  }
  case class RProductLessThan(ps: List[Param[Int]], y: Int) extends Restrict {
    def evaluate = ps.map(_.x).fold(1){_*_} < y
    override def toString = "product(" + ps.map(qt(_)).mkString(",") + s") < $y"
  }
  case class REqualOrOne(ps: List[Param[Int]]) extends Restrict {
    def evaluate = {
      val p = ps.map(_.x).distinct
      p.length == 1 || (p.length == 2 && p.contains(1))
    }
    override def toString = "(" + ps.map(qt(_)).mkString(",") + ") equal or one"
  }
  case class Domain[T](options: List[T], setter: T => Unit) {
    def apply(i: Int) = options(i)
    def set(i: Int) = setter(options(i))
    def setValue(v: T) = setter(v)
    def len: Int = options.length
    override def toString = if (len < 10) "Domain(" + options.mkString(",") + ")" else "Domain( " + len + " x)"

    def filter(cond: () => Boolean) = {
      new Domain(options.filter{t => setValue(t); cond()}, setter)
    }
  }
  object Domain {
    def apply(r: CRange, setter: Int => Unit) = {
      if (r.start % r.step != 0) {
        val start = r.step*(r.start/r.step + 1)
        new Domain[Int]((start until r.end by r.step).toList :+ r.start, setter)
      }
      else new Domain[Int](r.toList, setter)
    }
    def restricted(r: CRange, setter: Int => Unit, cond: () => Boolean) = {
      val values = ArrayBuffer[Int]()
      var start = r.start
      if (r.start % r.step != 0) {
        start = r.step*((r.start/r.step) + 1)
        setter(r.start);
        if (cond()) values += r.start
      }
      for (i <- start until r.end by r.step) {
        setter(i)
        if (cond()) values += i
      }
      new Domain[Int](values.toList, setter)
    }
  }

  def prune(params: List[Param[Int]], ranges: HashMap[Param[Int],CRange], restrict: List[Restrict]) = {
    val pruneSingle = params.map{t =>
      val restricts = restrict.filter(_.dependsOnlyOn(t))
      t -> Domain.restricted(ranges(t), {c: Int => t.setValue(c)}, () => restricts.forall(_.evaluate))
    }
    // TODO: prune pairs
    pruneSingle.map(_._2)
  }


  def xrange(start: Int, end: Int, step: Int) = new scala.collection.immutable.Range(start,end,step)
}

trait ParameterAnalysisExp extends ParamRestrictions with PipeStageToolsExp { this: DHDLExp => }


trait ParameterAnalyzer extends Traversal {
  val IR: DHDLExp with ParameterAnalysisExp
  import IR._

  override val debugMode = false

  val MIN_TILE_SIZE  = 96   // words
  val MAX_TILE_SIZE  = 9600 // words
  val MAX_TILE       = 51340

  val MAX_PAR_FACTOR = 192  // duplications
  val MAX_OUTER_PAR  = 15

  var tileSizes  = List[Param[Int]]()  // Params used to calculate BRAM size
  var parFactors = List[Param[Int]]()  // Params used as parallelization factors for counters
  val range      = HashMap[Param[Int],CRange]()

  var restrict   = List[Restrict]()
  var innerLoop  = false

  override def preprocess[A:Manifest](b: Block[A]) = {
    for ((s,m) <- metadata) {
      if (domainOf(s).isDefined && s.isInstanceOf[Param[_]] && s.tp == manifest[Int]) {
        val d = domainOf(s).get
        range(s.asInstanceOf[Param[Int]]) = xrange(d._1, d._2, d._3)
      }
    }
    (b)
  }

  def setRange(p: Param[Int], mn: Int, mx: Int, step: Int) = {
    if (!range.contains(p)) {
      range(p) = xrange(mn,mx,step)
    }
    else {
      val old = range(p)
      range(p) = xrange(Math.max(mn,old.start),Math.min(mx,old.end),Math.max(step,old.step))
    }
  }
  def setMax(p: Param[Int], mx: Int) = {
    if (!range.contains(p))
      range(p) = xrange(1,mx,1)
    else
      range(p) = xrange(range(p).start,Math.min(mx,range(p).end),range(p).step)
  }

  def canParallelize(e: Exp[Any]) = true //styleOf(e) != Disabled

  override def traverseStm(stm: Stm) = stm match {
    case TP(s, d) =>
      traverseNode(s, d)
      super.traverseStm(stm)
  }
  def traverseNode(lhs: Exp[Any], rhs: Def[Any]) = rhs match {
    case EatReflect(Bram_new(_,_)) =>
      val dims = dimsOf(lhs)
      val (consts,params) = dims.partition{ case Const(_) => true; case _ => false }
      val cSize = consts.map{case Const(c) => c.asInstanceOf[Int] }.fold(1){_*_}

      val tiles = params.flatMap{case ParamFix(p) => Some(p); case _ => None}

      tiles.zipWithIndex.foreach{
        case (p, idx) =>
          tileSizes ::= p
          if (idx < params.length - 1) { setRange(p, 1, MAX_TILE_SIZE, 1) }
          else                         { setRange(p, 1, MAX_TILE_SIZE, MIN_TILE_SIZE) }
      }

      //if (tiles.length > 1) restrict ::= RProductLessThan(tiles, )


    case EatReflect(Counter_new(start,end,step,par)) =>
      var max = MAX_PAR_FACTOR
      debug(s"Found counter with start=$start, end=$end, step=$step, par=$par")

      // Set constraints on par factor
      (start,end,step) match {
        case (Exact(0),ParamFix(p),Exact(1)) =>
          restrict ::= RLessEqual(par, p)
          restrict ::= RDivides(par, p)

        case (_,ParamFix(e),ParamFix(p)) => // ???

        case (Exact(0),Bound(e),ParamFix(p)) =>
          restrict ::= RDividesQuotient(par, e.toInt, p)

        case (Bound(s),Bound(e),ParamFix(p)) =>
          restrict ::= RDividesQuotient(par, (e-s).toInt, p)

        case (Bound(s),Bound(e),Bound(t)) =>
          val nIters = (e - s)/t
          if (nIters < max) max = nIters.toInt
          restrict ::= RDividesConst(par, nIters.toInt)  // HACK: par factor divides bounded loop size (avoid edge case)

        case _ => // No restrictions
      }
      setRange(par, 1, max, 1)

      // Set constraints on step size
      (start,end,step) match {
        case (ParamFix(s),ParamFix(p),ParamFix(_)) => // ???

        case (Exact(0),ParamFix(p),ParamFix(s)) =>
          restrict ::= RLessEqual(s, p)
          restrict ::= RDivides(s, p)  // HACK: avoid edge case

        case (Bound(s),Bound(b),ParamFix(p)) =>
          val l = b - s
          setRange(p, 1, l.toInt, MIN_TILE_SIZE)

          restrict ::= RDividesConst(p, l.toInt) // HACK: avoid edge case

        case _ => // No restrictions
      }

    // HACK: Parallelize innermost loop only
    case EatReflect(e:Pipe_foreach) if canParallelize(lhs) =>
      val pars = List( parParamsOf(e.cchain).last )
      parFactors :::= pars
      if (styleOf(lhs) != Fine) pars.foreach{p => setMax(p, MAX_OUTER_PAR) }

    case EatReflect(e:Pipe_reduce[_,_]) if canParallelize(lhs) =>
      val pars = List( parParamsOf(e.cchain).last )
      parFactors :::= pars
      if (styleOf(lhs) != Fine) pars.foreach{p => setMax(p, MAX_OUTER_PAR) }

    case EatReflect(e:Block_reduce[_]) if canParallelize(lhs) =>
      val opars = List( parParamsOf(e.ccOuter).last )
      val ipars = List( parParamsOf(e.ccInner).last )
      parFactors :::= opars
      parFactors :::= ipars
      opars.foreach{p => setMax(p, MAX_OUTER_PAR) }

    case EatReflect(e:TileTransfer[_]) =>
      val pars = List( parParamsOf(e.cchain).last )
      parFactors :::= pars

    case _ => //
  }
}

