package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

trait ScalaOps extends ForgeApplication {
  
  def addScalaOps() = {
    proxies()
    misc()
    variables()
    numerics()
    ordering()
    strings()
  }
  
  /**
   * TODO: Use reflection to auto-generate the ops and code generators
   * Question: how do we handle effects?
   *   withWrites("update", "insert", ...)?
   */
  def proxies() = {
    // proxy(scala.collection.immutable.Array)      
  }
  
  def misc() = {
    val Misc = grp("Misc")
    
    val println = op (Misc) ("println", direct, List(), List(MString), MUnit, codegenerated, effect = simple)
    codegen (println) ($cala, quote("println(" + println.quotedArg(0) + ")"))
    
    extern(grp("While"))
    /*
    val whileDo = op (Misc) ("__whileDo", direct, List(), List(MThunk(MBoolean),MThunk(MUnit)), MUnit, codegenerated, effect = simple)    
    
    // TODO: need some kind of templating engine 
    // TODO: need syms for functions    
    // TODO: how can we express this in a way that is reversible?
    //  - need abstractions for stream.print, sym, emitBlock, getBlockResult...
    codegen (whileDo) ($cala, (nl +
      "      stream.print(\"val " + unquote("quote(sym)") + " = while ({\")" + nl +
      "      emitBlock(" + opArgPrefix + 0 + ")" + nl + 
      "      stream.print(quote(getBlockResult(" + opArgPrefix + 0 + ")))" + nl +
      "      stream.print(\"}) {\")" + nl +
      "      emitBlock(" + opArgPrefix + 1 + ")" + nl + 
      "      stream.print(quote(getBlockResult(" + opArgPrefix + 1 + ")))" + nl +
      "      stream.println(\"}\")"
    ), isSimple = false)               
    */
  }
  
  def variables() = {
    // lots of quirks in the LMS file, just plug in external implementation for now
    extern(grp("Var"), withLift = true)
    
    /* 
    val Var = grp("Variables")
    
    val T = tpeArg("T")
    val newVar1 = op (Var) ("__newVar", direct, List(T), List(T), MVar(T), codegenerated, effect = simple)
    val pluseq = op (Var) ("+=", infix, List(T), List(MVar(T), T), MUnit, codegenerated, effect = write(0))
    
    codegen (newVar1) ($cala, "emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], quote(" + opArgPrefix + 0 + "))", isSimple = false)
    codegen (pluseq) ($cala, quote(pluseq.quotedArg(0) + " += " + pluseq.quotedArg(1)))
    */
  }
  
  def numerics() = {
    val Num = grp("Numeric")    
    val T = tpeArg("T")    
    
    lift (Num) (T withBound TNumeric)
        
    val plus = op (Num) ("+", infix, List(T withBound TNumeric), List(T,T), T, codegenerated)    
    val times = op (Num) ("*", infix, List(T withBound TNumeric), List(T,T), T, codegenerated)    
    codegen (plus) ($cala, quote(plus.quotedArg(0) + " + " + plus.quotedArg(1)))
    codegen (times) ($cala, quote(times.quotedArg(0) + " * " + times.quotedArg(1)))
  }
  
  def ordering() = {
    val Ord = grp("Ordering")
    val T = tpeArg("T")
    
    val lt = op (Ord) ("<", infix, List(T withBound TOrdering), List(T,T), MBoolean, codegenerated)    
    val gt = op (Ord) (">", infix, List(T withBound TOrdering), List(T,T), MBoolean, codegenerated)    
    
    codegen (lt) ($cala, quote(lt.quotedArg(0) + " < " + lt.quotedArg(1)))    
    codegen (gt) ($cala, quote(gt.quotedArg(0) + " > " + gt.quotedArg(1)))    
  }
  
  def strings() = {
    val Str = grp("String")
    lift (Str) (MString)
    
    // overloaded variants of string concat
    val CString = tpe("String", stage = now) 
    val T = tpeArg("T") 

    // most of these variants collapse to the a common back-end implementation:
    
    // maps to Rep[String], Rep[Any]
    val concat = op (Str) ("+", infix, List(T), List(CString, T), MString, codegenerated)
    val concat2 = op (Str) ("+", infix, List(T), List(MString, T), MString, codegenerated)
    val concat3 = op (Str) ("+", infix, List(T), List(CString, MVar(T)), MString, codegenerated)
    val concat4 = op (Str) ("+", infix, List(T), List(MString, MVar(T)), MString, codegenerated)
    
    // Rep[Any], Rep[String]
    val concat5 = op (Str) ("+", infix, List(T), List(T, CString), MString, codegenerated)
    val concat6 = op (Str) ("+", infix, List(T), List(T, MString), MString, codegenerated)
    val concat7 = op (Str) ("+", infix, List(T), List(MVar(T), CString), MString, codegenerated)
    val concat8 = op (Str) ("+", infix, List(T), List(MVar(T), MString), MString, codegenerated)
    
    // Rep[String], Rep[String]
    val concat9 = op (Str) ("+", infix, List(), List(MString, CString), MString, codegenerated)
    val concat10 = op (Str) ("+", infix, List(), List(CString, MString), MString, codegenerated)
    
    // TODO: we would like overloaded variants to possibly use the same codegen impl instead of being redundant here    
    // most of the concat codegens are not used, but it is not easy to tell which ones will "make it"
    // should overloading be more explicit in the spec to avoid this problem? (an 'overloaded' parameter?)
    // at the very least, we should check for inconsistent codegen rules (or impls) between overloaded variants that collapse to the same thing
    def scalaStrConcat(o: Rep[DSLOp]) = quote(o.quotedArg(0)+".toString + " + o.quotedArg(1)+".toString")
    codegen (concat) ($cala, scalaStrConcat(concat))
    codegen (concat2) ($cala, scalaStrConcat(concat2))
    codegen (concat3) ($cala, scalaStrConcat(concat3))
    codegen (concat4) ($cala, scalaStrConcat(concat4))
    codegen (concat5) ($cala, scalaStrConcat(concat5))
    codegen (concat6) ($cala, scalaStrConcat(concat6))
    codegen (concat7) ($cala, scalaStrConcat(concat7))
    codegen (concat8) ($cala, scalaStrConcat(concat8))
    codegen (concat9) ($cala, scalaStrConcat(concat9))
    codegen (concat10) ($cala, scalaStrConcat(concat10))
  }
}
