package ppl.dsl.forge
package templates
package ident

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.util.matching._
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._

import core._
import shared.{BaseGenOps,BaseGenDataStructures}
import Utilities._

trait LibGenOps extends BaseGenOps with BaseGenDataStructures {  
  this: ForgeCodeGenIdent =>
  
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
    case _:CodeGen => false
    case Getter(structArgIndex,field) => false
    case Setter(structArgIndex,field,value) => false
    case Allocates(tpe,init) => false
    case _ => true
  }
  
  def emitImpls(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("SingleTask and Composite Impls", stream)   
    return //XX
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
          emitWithIndent("val in = " + c.name, stream, indent+2)
          emitWithIndent("var acc = " + inline(o, reduce.zero), stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)  
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
          emitWithIndent("val in = " + c.name, stream, indent+2)
          emitWithIndent("var acc = " + inline(o, mapreduce.zero), stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)  
          emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(in)" + ") {", stream, indent+2)            
          emitWithIndent("acc = " + " reduce(acc, map(" + makeOpMethodName(dc.apply) + "(in, i)))", stream, indent+4)
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
        case hfr:HashFilterReduce =>
          val outDc = ForgeCollections(getHkTpe(o.retTpe)).asInstanceOf[ParallelCollectionBuffer]        
          val in = o.args.apply(hfr.argIndex)
          val inDc = ForgeCollections(getHkTpe(in.tpe))          
          emitWithIndent("def cond: " + repify(hfr.tpePars._1) + " => " + repify(MBoolean) + " = " + inline(o, hfr.cond), stream, indent+2)            
          emitWithIndent("def key: " + repify(hfr.tpePars._1) + " => " + repify(hfr.tpePars._2) + " = " + inline(o, hfr.key), stream, indent+2)            
          emitWithIndent("def map: " + repify(hfr.tpePars._1) + " => " + repify(hfr.tpePars._3) + " = " + inline(o, hfr.map), stream, indent+2) 
          emitWithIndent("def reduce: (" + repify(hfr.tpePars._3) + "," + repify(hfr.tpePars._3) + ") => " + repify(hfr.tpePars._3) + " = " + inline(o, hfr.reduce), stream, indent+2)                               
          emitWithIndent("val in = " + in.name, stream, indent+2)          
          emitWithIndent("val out = " + makeOpMethodName(outDc.alloc) + makeTpePars(instAllocReturnTpe(outDc.alloc,in.tpe,hfr.tpePars._3)) + "(in,0)", stream, indent+2)
          // TODO: replace with staged HashMap when it's available OR refactor things so that the op implementation is in the class wrapper and the staged functions are in the impls
          emitWithIndent("val indexMap = array_buffer_empty["+quote(hfr.tpePars._2)+"](0)", stream, indent+2)
          // emitWithIndent("val indexMap = scala.collection.mutable.HashMap["+repify(hfr.tpePars._2)+",Rep[Int]]()", stream, indent+2)
          emitWithIndent("var index = 0", stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)  
          emitWithIndent("while (i < " + makeOpMethodName(inDc.size) + "(in)"  + ") {", stream, indent+2)
          emitWithIndent("val e = " + makeOpMethodName(inDc.apply) + "(in, i)", stream, indent+4)
          emitWithIndent("if (cond(e)) {", stream, indent+4)
          emitWithIndent("val k = key(e)", stream, indent+6)
          // emitWithIndent("if (!indexMap.contains(k)) {", stream, indent+6)
          emitWithIndent("val ki = array_buffer_indexof(indexMap,k)", stream, indent+6)
          emitWithIndent("if (ki < 0) {", stream, indent+6)
          // emitWithIndent("indexMap(k) = index", stream, indent+8)              
          emitWithIndent("array_buffer_append(indexMap, k)", stream, indent+8)          
          emitWithIndent(makeOpMethodName(outDc.append) + "(out, index, map(e))", stream, indent+8)
          emitWithIndent("index += 1", stream, indent+8)
          emitWithIndent("}", stream, indent+6)
          emitWithIndent("else {", stream, indent+6)
          emitWithIndent("val cur = " + makeOpMethodName(outDc.apply) + "(out, ki)", stream, indent+8)
          emitWithIndent(makeOpMethodName(outDc.update) + "(out, ki, reduce(cur, map(e)))", stream, indent+8)
          emitWithIndent("}", stream, indent+6)            
          emitWithIndent("}", stream, indent+4)            
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)            
          emitWithIndent("out", stream, indent+2)                              
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
        emitWithIndent("compiler (\"" + o.name + "\")", stream, indent)
        val rule = codegen.decls.getOrElse($cala, err("could not find Scala codegen rule for op: " + o.name))
        inline(o, rule.decl, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent) }      
      case Getter(structArgIndex,field) =>
        emitWithIndent("getter (\"" + o.name + "\")", stream, indent)
        emitOverloadShadows(o, stream, indent)
        emitWithIndent(inline(o, quotedArg(o.args.apply(structArgIndex).name)) + "." + field, stream, indent)
      case Setter(structArgIndex,field,value) =>
        emitWithIndent("setter (\"" + o.name + "\")", stream, indent)
        emitOverloadShadows(o, stream, indent)
        emitWithIndent(inline(o, quotedArg(o.args.apply(structArgIndex).name)) + "." + field + " = " + inline(o,value), stream, indent)
      case Allocates(tpe,init) =>        
        // static (Vector) ("apply", T, MInt :: Vector(T), effect = mutable) implements allocates(Vector, ${$0}, ${ array_empty[T]($0) })
        emitWithIndent("static (\"" + o.name + "\")", stream, indent)
        emitOverloadShadows(o, stream, indent)
        val initialVals = init.map(i => inline(o,i)).mkString(",")
        emitWithIndent("new " + quote(tpe) + "(" + initialVals + ")", stream, indent)
      case _ => emitWithIndent(makeOpImplMethodNameWithArgs(o), stream, indent)
    }
  }
  
  def xx[T](x:Rep[T]): Def[T] = Def.unapply(x).get
  def xxx[T](x:List[Rep[T]]): List[Def[T]] = x.map(xx)


  def zzz(x:Any): String = x match {
    case x: String => "\"" + x + "\""
    case x: Rep[Any] => zzz(Def.unapply(x).get)
    case xs: List[Any] => "List(" + xs.map(zzz).mkString(",") + ")"
    case xs: Seq[Any] => "Seq(" + xs.map(zzz).mkString(",") + ")"
    case xs: Product if xs.productArity == 0 => xs.productPrefix
    case xs: Product => xs.productPrefix + "(" + xs.productIterator.map(zzz).mkString(",") + ")"
    case _ => ""+x
  }

  def emitClass(opsGrp: DSLOps, stream: PrintWriter) {
    if (grpIsTpe(opsGrp.grp)) {
      val tpe = grpAsTpe(opsGrp.grp)
      val d = DataStructs.get(tpe)
      d.foreach { data => 

        stream.println("val " + data.tpe.name + " = " + zzz(data.tpe))
        stream.println()
        stream.println("data(" + data.tpe.name + ", " + zzz(data.fields) + ")")
        stream.println()

        //stream.println("class " + data.tpe.name + makeTpeParsWithBounds(data.tpe.tpePars) + "(" + makeFieldArgs(data) + ") {")
        //stream.println(makeFieldsWithInitArgs(data))
        // Actually emitting the infix methods as instance methods, while a little more readable, makes the interpreter methods
        // ambiguous with the op conversions unless they already exist on every instance and must be overridden (e.g. toString)
        /*for (o <- unique(opsGrp.ops) if overrideList.contains(o.name) && o.style == infixMethod && o.args.length > 0 && quote(o.args.apply(0).tpe) == quote(tpe)) {       
          stream.println("// xxx " + o.name)

          stream.print("  "+makeDefWithOverride(o)+" " + o.name + makeTpeParsWithBounds(o.tpePars.drop(1)))
          //stream.print("(" + o.args/*.drop(1)*/.map(t => t.name + ": " + repify(t.tpe) + " = " + unit(t.default)).mkString(",") + ")") TODO 
          stream.print("(" + o.args.drop(1).map(t => argify(t, repify)).mkString(",") + ")")  
          stream.print(makeImplicitArgsWithCtxBoundsWithType(o.implicitArgs, o.tpePars, without = data.tpe.tpePars))
          stream.println(" = {")
          emitWithIndent("val " + o.args.apply(0).name + " = this", stream, 4)
          emitOp(o, stream, indent=4)
          stream.println("  }")
        }
        stream.println("}")
        stream.println()*/
      }
      if (d.isEmpty && !isForgePrimitiveType(tpe)) {
        // do nothing -- abstract class will have been generated in the front-end
        // warn("(ident) no data structure found for tpe " + tpe.name + ". emitting empty class declaration") 
        // stream.println("  class " + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + "() {}")
        // stream.println()
      }
      stream.println("val " + opsGrp.name + " = withTpe (" + opsGrp.name.dropRight(3)/*hack*/ + ")")
      stream.println(opsGrp.grp.name + " {")
    } else {
      val Def(Grp(name)) = opsGrp.grp
      stream.println("val " + name + " = grp (\"" + name + "\")")
      stream.println(";{")
    }



    stream.println()
    //val VectorOps = withTpe (Vector)
    
    for (o <- unique(opsGrp.ops)) {       
      stream.println("// " + o.name)
      stream.println("// " + zzz(o))

/*

  case class Op(grp: Rep[DSLGroup], name: String, style: MethodType, tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], curriedArgs: List[List[Rep[DSLArg]]], implicitArgs: List[Rep[DSLArg]], retTpe: Rep[DSLType], effect: EffectType, aliasHint: AliasHint) extends Def[DSLOp]
  
  def forge_op(_grp: Rep[DSLGroup], name: String, style: MethodType, tpePars: List[Rep[TypePar]], 
  args: List[Rep[DSLArg]], curriedArgs: List[List[Rep[DSLArg]]], implicitArgs: List[Rep[DSLArg]], retTpe: Rep[DSLType], 
  effect: EffectType, aliasHint: AliasHint) = {
*/

      def forge_op0(style: String)(grp: Rep[DSLGroup])(name: String, tpePars: List[Rep[TypePar]], 
        args: List[Rep[DSLArg]], curriedArgs: List[List[Rep[DSLArg]]], implicitArgs: List[Rep[DSLArg]], retTpe: Rep[DSLType], 
        effect: EffectType = pure, aliasHint: AliasHint = nohint) = {
        // need to reverse:
        //           signature.args,signature.curriedArgs,implicitArgs.zipWithIndex.map(anyToImplicitArg).asInstanceOf[List[Rep[DSLArg]]],signature.retTpe

        val signature = if (curriedArgs.isEmpty) MethodSignature(args,retTpe)
                        else CurriedMethodSignature(curriedArgs,retTpe)

        forge_op1(style)(grp.name)(zzz(name),zzz(tpePars),zzz(signature),zzz(implicitArgs),zzz(effect),zzz(aliasHint))
      }
      def forge_op1(style: String)(grp: String)(name: String, tpePars: String, signature: String, implicitArgs: String, effect: String, aliasHint: String) = 
          stream.println(s"$style ($grp)($name, $tpePars, $signature, $implicitArgs, $effect, $aliasHint)")

      val Def(op: Op) = o

      val key = op.style match {
        case `staticMethod`   => "static"
        case `infixMethod`    => "infix"
        case `directMethod`   => "direct"
        case `compilerMethod` => "compiler"
        case `implicitMethod` => "fimplicit"
      }

      forge_op0(key)(op.grp)(op.name, op.tpePars, op.args, op.curriedArgs, op.implicitArgs, op.retTpe, op.effect, op.aliasHint)


      stream.println()


      /*stream.println("  " + makeOpMethodSignature(o) + " = {")
      o.style match {
        /*case `infixMethod` if overrideList.contains(o.name) && grpIsTpe(opsGrp.grp) && DataStructs.contains(grpAsTpe(opsGrp.grp)) && o.args.length > 1 && quote(o.args.apply(0).tpe) == quote(opsGrp.grp) => 
          //val args = o.args/*.drop(1)*/.map(t => t.name)).mkString(",")
          val args = o.args.map(t => t.name).mkString(",")
          val argsWithParen = if (args == "") args else "(" + args + ")"
          emitWithIndent("infix (\"" + o.name + "\")" + o.args.apply(0).name + "." + o.name + argsWithParen, stream, 4)
          emitWithIndent(o.args.apply(0).name + "." + o.name + argsWithParen, stream, 4)*/
        case _ => emitOp(o, stream, indent=4)
      }
      stream.println("  }")   */     
    }
    stream.println("}")
        
  }
}
