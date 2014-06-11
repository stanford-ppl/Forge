package ppl.dsl.forge
package templates
package library

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.util.matching._
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._

import core._
import shared.{BaseGenOps,BaseGenDataStructures}
import Utilities._

trait LibGenOps extends BaseGenOps with BaseGenDataStructures {
  this: ForgeCodeGenInterpreter =>

  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  // inline 'unquotes' calls as well in the library version
  // is this always the right thing to do? probably not! how do we know?
  // override def inline(o: Rep[DSLOp], str: Exp[String], quoter: Exp[Any] => String = quote) = {
  //   var x = super.inline(o, str, quoter)
  //   val quotePattern = new Regex("""\"\+(.*?)\+\"""", "body")
  //   x = quotePattern replaceAllIn (x, m => m.group("body"))
  //   x
  // }

  override def quote(x: Exp[Any]): String = x match {
    // case Def(QuoteSeq(i)) => "("+i+": _*)"  // not exactly a quoted sequence..
    case Def(QuoteSeq(argName)) => argName
    case Const(s: String) => replaceWildcards(s) // don't add quotes
    case _ => super.quote(x)
  }

  def requiresImpl(o: Rep[DSLOp]) = Impls(o) match {
    case _:CodeGen | _:Redirect => false
    case Getter(structArgIndex,field) => false
    case Setter(structArgIndex,field,value) => false
    case Allocates(tpe,init) => false
    case _ => true
  }

  def emitImpls(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("SingleTask and Composite Impls", stream)
    stream.println()
    stream.println("trait " + opsGrp.grp.name + "WrapperImpl {")
    stream.println("  this: " + dsl + "Application with " + dsl + "CompilerOps => ")
    stream.println()
    val indent = 2
    for (o <- unique(opsGrp.ops) if requiresImpl(o)) {
      emitWithIndent(makeOpImplMethodSignature(o) + " = {", stream, indent)
      Impls(o) match {
        case single:SingleTask =>
          inline(o, single.func, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent+2 )}
        case composite:Composite =>
          inline(o, composite.func, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent+2 )}
        case map:Map =>
          val outDc = ForgeCollections(getHkTpe(o.retTpe))
          val in = o.args.apply(map.argIndex)
          val inDc = ForgeCollections(getHkTpe(in.tpe))
          emitWithIndent("def func: " + repify(map.tpePars._1) + " => " + repify(map.tpePars._2) + " = " + inline(o, map.func), stream, indent+2)
          emitWithIndent("val in = " + in.name, stream, indent+2)
          // TODO: why not just makeTpePars(o.retTpe.tpePars)? and sanity check with map.tpePars._2:
          //  need to call the dcAlloc function, which may have different tpe pars than the output..
          emitWithIndent("val out = " + makeOpMethodName(outDc.alloc) + makeTpePars(instAllocReturnTpe(outDc.alloc,in.tpe,map.tpePars._2)) + "(in, " + makeOpMethodName(inDc.size) + "(in)" + ")", stream, indent+2) // TODO - makeArg
          emitWithIndent("var i = 0", stream, indent+2)
          emitWithIndent("while (i < " + makeOpMethodName(inDc.size) + "(in)" + ") {", stream, indent+2)
          emitWithIndent(makeOpMethodName(outDc.update) + "(out, i, func(" + makeOpMethodName(inDc.apply) + "(in, i)))", stream, indent+4)
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)
          emitWithIndent("out", stream, indent+2)
        case zip:Zip =>
          val outDc = ForgeCollections(getHkTpe(o.retTpe))
          val inA = o.args.apply(zip.argIndices._1)
          val inB = o.args.apply(zip.argIndices._2)
          val inADc = ForgeCollections(getHkTpe(inA.tpe))
          val inBDc = ForgeCollections(getHkTpe(inB.tpe))
          emitWithIndent("def func: (" + repify(zip.tpePars._1) + "," + repify(zip.tpePars._2) + ") => " + repify(zip.tpePars._3) + " = " + inline(o, zip.func), stream, indent+2)
          emitWithIndent("val inA = " + inA.name, stream, indent+2)
          emitWithIndent("val inB = " + inB.name, stream, indent+2)
          emitWithIndent("val out = " + makeOpMethodName(outDc.alloc) + makeTpePars(instAllocReturnTpe(outDc.alloc,inA.tpe,zip.tpePars._3)) + "(inA, " + makeOpMethodName(inADc.size) + "(inA)" + ")", stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)
          emitWithIndent("while (i < " + makeOpMethodName(inADc.size) + "(inA)" + ") {", stream, indent+2)
          emitWithIndent(makeOpMethodName(outDc.update) + "(out, i, func(" + makeOpMethodName(inADc.apply) + "(inA, i)," + makeOpMethodName(inBDc.apply) + "(inB, i)))", stream, indent+4)
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)
          emitWithIndent("out", stream, indent+2)
        case reduce:Reduce =>
          val c = o.args.apply(reduce.argIndex)
          val dc = ForgeCollections(getHkTpe(c.tpe))
          emitWithIndent("def func: (" + repify(reduce.tpePar) + "," + repify(reduce.tpePar) + ") => " + repify(reduce.tpePar) + " = " + inline(o, reduce.func), stream, indent+2)
          emitWithIndent("def zero: " + repify(reduce.tpePar) + " = " + inline(o, reduce.zero), stream, indent+2)
          emitWithIndent("val in = " + c.name, stream, indent+2)
          emitWithIndent("var acc = if (" + makeOpMethodName(dc.size) + "(in) == 0) zero else " + makeOpMethodName(dc.apply) + "(in, 0)", stream, indent+2)
          emitWithIndent("var i = 1", stream, indent+2)
          emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(in)" + ") {", stream, indent+2)
          emitWithIndent("acc = " + " func(acc, " + makeOpMethodName(dc.apply) + "(in, i))", stream, indent+4)
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)
          emitWithIndent("acc", stream, indent+2)
        case mapreduce:MapReduce =>
          val c = o.args.apply(mapreduce.argIndex)
          val dc = ForgeCollections(getHkTpe(c.tpe))
          emitWithIndent("def map: " + repify(mapreduce.tpePars._1) + " => " + repify(mapreduce.tpePars._2) + " = " + inline(o, mapreduce.map), stream, indent+2)
          emitWithIndent("def reduce: (" + repify(mapreduce.tpePars._2) + "," + repify(mapreduce.tpePars._2) + ") => " + repify(mapreduce.tpePars._2) + " = " + inline(o, mapreduce.reduce), stream, indent+2)
          emitWithIndent("def zero: " + repify(mapreduce.tpePars._2) + " = " + inline(o, mapreduce.zero), stream, indent+2)
          emitWithIndent("val in = " + c.name, stream, indent+2)
          if (mapreduce.cond.isDefined) {
            emitWithIndent("def cond: " + repify(mapreduce.tpePars._1) + " => " + repify(MBoolean) + " = " + inline(o, mapreduce.cond.get), stream, indent+2)
            emitWithIndent("var acc = null.asInstanceOf["+quote(mapreduce.tpePars._2)+"]", stream, indent+2)
            emitWithIndent("var i = 0", stream, indent+2)
          }
          else {
            emitWithIndent("var acc = if (" + makeOpMethodName(dc.size) + "(in) == 0) zero else map(" + makeOpMethodName(dc.apply) + "(in, 0))", stream, indent+2)
            emitWithIndent("var i = 1", stream, indent+2)
          }
          emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(in)" + ") {", stream, indent+2)
          emitWithIndent("val e = " + makeOpMethodName(dc.apply) + "(in, i)", stream, indent+4)
          if (mapreduce.cond.isDefined) {
            emitWithIndent("if (cond(e)) {", stream, indent+4)
            emitWithIndent("if (acc == null) acc = map(e)", stream, indent+6)
            emitWithIndent("else acc = reduce(acc, map(e))", stream, indent+6)
            emitWithIndent("}", stream, indent+4)
          }
          else {
            emitWithIndent("acc = " + " reduce(acc, map(e))", stream, indent+4)
          }
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)
          emitWithIndent("acc", stream, indent+2)
        case filter:Filter =>
          val outDc = ForgeCollections(getHkTpe(o.retTpe)).asInstanceOf[ParallelCollectionBuffer]
          val in = o.args.apply(filter.argIndex)
          val inDc = ForgeCollections(getHkTpe(in.tpe))
          emitWithIndent("def func: " + repify(filter.tpePars._1) + " => " + repify(filter.tpePars._2) + " = " + inline(o, filter.func), stream, indent+2)
          emitWithIndent("def cond: " + repify(filter.tpePars._1) + " => " + repify(MBoolean) + " = " + inline(o, filter.cond), stream, indent+2)
          emitWithIndent("val in = " + in.name, stream, indent+2)
          emitWithIndent("val out = " + makeOpMethodName(outDc.alloc) + makeTpePars(instAllocReturnTpe(outDc.alloc,in.tpe,filter.tpePars._2)) + "(in,0)", stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)
          emitWithIndent("while (i < " + makeOpMethodName(inDc.size) + "(in)"  + ") {", stream, indent+2)
          emitWithIndent("val e = " + makeOpMethodName(inDc.apply) + "(in, i)", stream, indent+4)
          emitWithIndent("if (cond(e)) {", stream, indent+4)
          emitWithIndent(makeOpMethodName(outDc.append) + "(out, i, func(e))", stream, indent+6)
          emitWithIndent("}", stream, indent+4)
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)
          emitWithIndent("out", stream, indent+2)
        case flatmap:FlatMap =>
          val outCol = getHkTpe(o.retTpe)
          val outDc = ForgeCollections(outCol).asInstanceOf[ParallelCollectionBuffer]
          val in = o.args.apply(flatmap.argIndex)
          val inDc = ForgeCollections(getHkTpe(in.tpe))
          emitWithIndent("def func: " + repify(flatmap.tpePars._1) + " => " + repify(tpeInst(outCol, flatmap.tpePars._2)) + " = " + inline(o, flatmap.func), stream, indent+2)
          emitWithIndent("val in = " + in.name, stream, indent+2)
          emitWithIndent("val out = " + makeOpMethodName(outDc.alloc) + makeTpePars(instAllocReturnTpe(outDc.alloc,in.tpe,flatmap.tpePars._2)) + "(in,0)", stream, indent+2)
          emitWithIndent("var sz = 0", stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)
          emitWithIndent("while (i < " + makeOpMethodName(inDc.size) + "(in)) {", stream, indent+2)
          emitWithIndent("val e = " + makeOpMethodName(inDc.apply) + "(in, i)", stream, indent+4)
          emitWithIndent("val buf = func(e)", stream, indent+4)
          emitWithIndent("var j = 0", stream, indent+4)
          emitWithIndent("while (j < " + makeOpMethodName(outDc.size) + "(buf)) {", stream, indent+4)
          emitWithIndent(makeOpMethodName(outDc.append) + "(out, sz, buf(j))", stream, indent+6)
          emitWithIndent("sz += 1", stream, indent+6)
          emitWithIndent("j += 1", stream, indent+6)
          emitWithIndent("}", stream, indent+4)
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)
          emitWithIndent("out", stream, indent+2)
        case gb:GroupBy =>
          // val outerColTpe = getHkTpe(o.retTpe)
          val outerColTpe = MArrayBuffer
          val outDc = ForgeCollections(outerColTpe).asInstanceOf[ParallelCollectionBuffer]
          val innerColTpe = getHkTpe(gb.tpePars._4)
          val innerDc = ForgeCollections(innerColTpe).asInstanceOf[ParallelCollectionBuffer]
          val in = o.args.apply(gb.argIndex)
          val inDc = ForgeCollections(getHkTpe(in.tpe))
          if (gb.cond.isDefined) {
            emitWithIndent("def cond: " + repify(gb.tpePars._1) + " => " + repify(MBoolean) + " = " + inline(o, gb.cond.get), stream, indent+2)
          }
          emitWithIndent("def key: " + repify(gb.tpePars._1) + " => " + repify(gb.tpePars._2) + " = " + inline(o, gb.key), stream, indent+2)
          emitWithIndent("def map: " + repify(gb.tpePars._1) + " => " + repify(gb.tpePars._3) + " = " + inline(o, gb.map), stream, indent+2)
          emitWithIndent("val in = " + in.name, stream, indent+2)
          emitWithIndent("val out = SHashMap["+quote(gb.tpePars._2)+","+quote(tpeInst(innerColTpe, gb.tpePars._3))+"]()", stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)
          emitWithIndent("while (i < " + makeOpMethodName(inDc.size) + "(in)"  + ") {", stream, indent+2)
          emitWithIndent("val e = " + makeOpMethodName(inDc.apply) + "(in, i)", stream, indent+4)
          if (gb.cond.isDefined) {
            emitWithIndent("if (cond(e)) {", stream, indent+4)
          }
          emitWithIndent("val k = key(e)", stream, indent+6)
          emitWithIndent("if (!out.contains(k)) {", stream, indent+6)

          // FIXME: passing 'in' to dc_alloc is a bit of a hack - what do we do when 'in' is not the same type as 'out'? currently (and only for groupby) we pass null.
          // this is only safe when the output collection type is known to not use the input argument, such as the current case, ArrayBuffer.
          // to really fix this, either the dc_alloc design needs to be revisited, or we need to use a different allocation mechanism than dc_alloc.
          val innerDcArg = if (quote(in.tpe) == quote(innerColTpe)) "in" else "null.asInstanceOf["+repify(tpeInst(innerColTpe, gb.tpePars._3))+"]"

          emitWithIndent("val bucket = " + makeOpMethodName(innerDc.alloc) + makeTpePars(instAllocReturnTpe(innerDc.alloc, in.tpe, gb.tpePars._3)) + "("+innerDcArg+",0)", stream, indent+8)
          emitWithIndent(makeOpMethodName(innerDc.append) + "(bucket, 0, map(e))", stream, indent+8)
          emitWithIndent("out(k) = bucket", stream, indent+8)
          emitWithIndent("}", stream, indent+6)
          emitWithIndent("else {", stream, indent+6)
          emitWithIndent("val bucket = out(k)", stream, indent+8)
          emitWithIndent(makeOpMethodName(innerDc.append) + "(bucket, " + makeOpMethodName(innerDc.size) + "(bucket), map(e))", stream, indent+8)
          emitWithIndent("}", stream, indent+6)
          if (gb.cond.isDefined) {
            emitWithIndent("}", stream, indent+4)
          }
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)
          emitWithIndent("fhashmap_from_shashmap(out)", stream, indent+2) // convert to ForgeHashMap
        case gbr:GroupByReduce =>
          val in = o.args.apply(gbr.argIndex)
          val inDc = ForgeCollections(getHkTpe(in.tpe))
          if (gbr.cond.isDefined) {
            emitWithIndent("def cond: " + repify(gbr.tpePars._1) + " => " + repify(MBoolean) + " = " + inline(o, gbr.cond.get), stream, indent+2)
          }
          emitWithIndent("def key: " + repify(gbr.tpePars._1) + " => " + repify(gbr.tpePars._2) + " = " + inline(o, gbr.key), stream, indent+2)
          emitWithIndent("def map: " + repify(gbr.tpePars._1) + " => " + repify(gbr.tpePars._3) + " = " + inline(o, gbr.map), stream, indent+2)
          emitWithIndent("def reduce: (" + repify(gbr.tpePars._3) + "," + repify(gbr.tpePars._3) + ") => " + repify(gbr.tpePars._3) + " = " + inline(o, gbr.reduce), stream, indent+2)
          emitWithIndent("val in = " + in.name, stream, indent+2)
          emitWithIndent("val out = SHashMap["+quote(gbr.tpePars._2)+","+quote(gbr.tpePars._3)+"]()", stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)
          emitWithIndent("while (i < " + makeOpMethodName(inDc.size) + "(in)"  + ") {", stream, indent+2)
          emitWithIndent("val e = " + makeOpMethodName(inDc.apply) + "(in, i)", stream, indent+4)
          if (gbr.cond.isDefined) {
            emitWithIndent("if (cond(e)) {", stream, indent+4)
          }
          emitWithIndent("val k = key(e)", stream, indent+6)
          emitWithIndent("if (!out.contains(k)) {", stream, indent+6)
          emitWithIndent("out(k) = map(e)", stream, indent+8)
          emitWithIndent("}", stream, indent+6)
          emitWithIndent("else {", stream, indent+6)
          emitWithIndent("out(k) = reduce(out(k), map(e))", stream, indent+8)
          emitWithIndent("}", stream, indent+6)
          if (gbr.cond.isDefined) {
            emitWithIndent("}", stream, indent+4)
          }
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)
          emitWithIndent("fhashmap_from_shashmap(out)", stream, indent+2)
        case foreach:Foreach =>
          val c = o.args.apply(foreach.argIndex)
          val dc = ForgeCollections(getHkTpe(c.tpe))
          emitWithIndent("def func: " + repify(foreach.tpePar) + " => " + repify(MUnit) + " = " + inline(o, foreach.func), stream, indent+2)
          emitWithIndent("val in = " + c.name, stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)
          emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(in)" + ") {", stream, indent+2)
          emitWithIndent("func(" + makeOpMethodName(dc.apply) + "(in, i))", stream, indent+4)
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)
      }
      emitWithIndent("}", stream, indent)
      stream.println()
    }
    stream.println("}")
  }

  def emitOverloadShadows(o: Rep[DSLOp], stream: PrintWriter, indent: Int = 0) {
    val i = nameClashId(o)
    if (i != "") emitWithIndent("def " + implicitOpArgPrefix + o.implicitArgs.length + " = ()", stream, indent)
  }

  def emitOp(o: Rep[DSLOp], stream: PrintWriter, indent: Int = 0) {
    Impls(o) match {
      case codegen:CodeGen =>
        val rule = codegen.decls.getOrElse($cala, err("could not find Scala codegen rule for op: " + o.name))
        inline(o, rule.decl, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent) }
      case Getter(structArgIndex,field) =>
        emitOverloadShadows(o, stream, indent)
        emitWithIndent(inline(o, quotedArg(o.args.apply(structArgIndex).name)) + "." + field, stream, indent)
      case Setter(structArgIndex,field,value) =>
        emitOverloadShadows(o, stream, indent)
        emitWithIndent(inline(o, quotedArg(o.args.apply(structArgIndex).name)) + "." + field + " = " + inline(o,value), stream, indent)
      case Allocates(tpe,init) =>
        emitOverloadShadows(o, stream, indent)
        val initialVals = init.map(i => inline(o,i)).mkString(",")
        emitWithIndent("new " + quote(tpe) + "(" + initialVals + ")", stream, indent)
      case _ => emitWithIndent(makeOpImplMethodNameWithArgs(o), stream, indent)
    }
  }

  def emitClass(opsGrp: DSLOps, stream: PrintWriter) {
    val classes = opsGrpTpes(opsGrp)
    for (tpe <- classes) {
      val d = DataStructs.get(tpe)
      d.foreach { data =>
        val classType = if (data.tpe.name.startsWith("Tup")) "case class " else "class " //sketchy, but we want our special tuples to retain value equality (like Scala tuples)
        stream.println(classType + data.tpe.name + makeTpeParsWithBounds(data.tpe.tpePars) + "(" + makeFieldArgs(data) + ") {")
        stream.println(makeFieldsWithInitArgs(data))

        // Actually emitting the infix methods as instance methods, while a little more readable, makes the interpreter methods
        // ambiguous with the op conversions unless they already exist on every instance and must be overridden (e.g. toString)
        for (o <- unique(opsGrp.ops) if overrideList.contains(o.name) && !Impls(o).isInstanceOf[Redirect] && o.style == infixMethod && o.args.length > 0 && quote(o.args.apply(0).tpe) == quote(tpe)) {
          val otherTpePars = o.tpePars.filterNot(p => data.tpe.tpePars.map(_.name).contains(p.name))
          stream.print("  "+makeDefWithOverride(o)+" " + o.name + makeTpeParsWithBounds(otherTpePars))
          //stream.print("(" + o.args/*.drop(1)*/.map(t => t.name + ": " + repify(t.tpe) + " = " + unit(t.default)).mkString(",") + ")") TODO
          stream.print("(" + o.args.drop(1).map(t => argify(t, repify)).mkString(",") + ")")
          stream.print(makeImplicitArgsWithCtxBoundsWithType(o.implicitArgs, o.tpePars diff otherTpePars, without = data.tpe.tpePars))
          stream.println(" = {")
          emitWithIndent("val " + o.args.apply(0).name + " = this", stream, 4)
          emitOp(o, stream, indent=4)
          stream.println("  }")
        }

        stream.println("}")
        stream.println()
      }

      if (d.isEmpty && !isForgePrimitiveType(tpe)) {
        // do nothing -- abstract class will have been generated in the front-end
        // warn("(library) no data structure found for tpe " + tpe.name + ". emitting empty class declaration")
        // stream.println("  class " + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + "() {}")
        // stream.println()
      }
    }

    for (o <- unique(opsGrp.ops) if !Impls(o).isInstanceOf[Redirect]) {
      // no return tpe because not all of the tpes are in scope in the lib wrapper
      stream.println("  " + makeOpMethodSignature(o, withReturnTpe = Some(false)) + " = {")
      o.style match {
        case `infixMethod` if overrideList.contains(o.name) && grpIsTpe(opsGrp.grp) && DataStructs.contains(grpAsTpe(opsGrp.grp)) && o.args.length > 1 && quote(o.args.apply(0).tpe) == quote(opsGrp.grp) =>
          //val args = o.args/*.drop(1)*/.map(t => t.name)).mkString(",")
          val args = o.args.drop(1).map(t => t.name).mkString(",")
          val argsWithParen = if (args == "") args else "(" + args + ")"
          emitWithIndent(o.args.apply(0).name + "." + o.name + argsWithParen, stream, 4)
        case _ => emitOp(o, stream, indent=4)
      }
      stream.println("  }")
    }
    stream.println()

  }
}
