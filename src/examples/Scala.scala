package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

trait ScalaOps extends ForgeApplication {
  
  def addScalaOps() = {
    proxies()
    misc()
    numerics()
    ordering()
    strings()
    math()
    booleans()
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
    
    val println = op (Misc) ("println", direct, List(), List(MAny), MUnit, effect = simple)
    val println2 = op (Misc) ("println", direct, List(), List(), MUnit, effect = simple)
    codegen (println) ($cala, "println(" + println.quotedArg(0) + ")")
    codegen (println2) ($cala, "println()")
    
    val whileDo = op (Misc) ("__whileDo", direct, List(), List(MThunk(MBoolean),MThunk(MUnit)), MUnit, effect = simple)        
    val stream = ForgePrinter()    
    codegen (whileDo) ($cala, stream.printLines(
      "while ({",
        blockResult(whileDo, 0),
      "}) {",
        blockResult(whileDo, 1),
      "}"
    ))
    
    // TODO: something is broken with IfThenElse here; bound symbols (effects) are getting hoisted if the frequencies are not set to cold.
    val T = tpePar("T")
    val ifThenElse = op (Misc) ("__ifThenElse", direct, List(T), List(MThunk(MBoolean),MThunk(T,cold),MThunk(T,cold)), T) 
    codegen (ifThenElse) ($cala, stream.printLines(
      "if ({",
        blockResult(ifThenElse, 0),
      "}){",
        blockResult(ifThenElse, 1),
      "} else {",
        blockResult(ifThenElse, 2),
      "}"
    ))    
    
    val immutable = op (Misc) ("unsafeImmutable", infix, List(T), List(T), T, aliasHint = copies(0))
    codegen (immutable) ($cala, quotedArg(0))
  }
  
  def numerics() = {
    val Num = grp("Numeric")    
    val T = tpePar("T")    
    
    lift (Num) (T withBound TNumeric)
        
    val zero = op (Num) ("zero", infix, List(T withBound TNumeric), List(), T)
    val plus = op (Num) ("+", infix, List(T withBound TNumeric), List(T,T), T)    
    val minus = op (Num) ("-", infix, List(T withBound TNumeric), List(T,T), T)     
    val times = op (Num) ("*", infix, List(T withBound TNumeric), List(T,T), T)    
    codegen (zero) ($cala, "implicitly[Numeric["+zero.tpeInstance(0)+"]].zero")
    codegen (plus) ($cala, plus.quotedArg(0) + " + " + plus.quotedArg(1))
    codegen (minus) ($cala, minus.quotedArg(0) + " - " + minus.quotedArg(1))
    codegen (times) ($cala, times.quotedArg(0) + " * " + times.quotedArg(1))
  }
  
  def ordering() = {
    val Ord = grp("Ordering2") // Ordering gets pulled in by DeliteArrayOps, need to disambiguate
    val T = tpePar("T")
    
    val lt = op (Ord) ("<", infix, List(T withBound TOrdering), List(T,T), MBoolean)    
    val gt = op (Ord) (">", infix, List(T withBound TOrdering), List(T,T), MBoolean)    
    
    codegen (lt) ($cala, lt.quotedArg(0) + " < " + lt.quotedArg(1))    
    codegen (gt) ($cala, gt.quotedArg(0) + " > " + gt.quotedArg(1))
  }
  
  def strings() = {
    val Str = grp("String")
    lift (Str) (MString)
    
    // overloaded variants of string concat
    val T = tpePar("T") 

    // most of these variants collapse to a common back-end implementation:
    
    // maps to Rep[String], Rep[Any]
    val concat = op (Str) ("+", infix, List(T), List(CString, T), MString)
    val concat2 = op (Str) ("+", infix, List(T), List(MString, T), MString)
    val concat3 = op (Str) ("+", infix, List(T), List(CString, MVar(T)), MString)
    val concat4 = op (Str) ("+", infix, List(T), List(MString, MVar(T)), MString)
    
    // Rep[Any], Rep[String]
    val concat5 = op (Str) ("+", infix, List(T), List(T, CString), MString)
    val concat6 = op (Str) ("+", infix, List(T), List(T, MString), MString)
    val concat7 = op (Str) ("+", infix, List(T), List(MVar(T), CString), MString)
    val concat8 = op (Str) ("+", infix, List(T), List(MVar(T), MString), MString)
    
    // Rep[String], Rep[String]
    val concat9 = op (Str) ("+", infix, List(), List(MString, CString), MString)
    val concat10 = op (Str) ("+", infix, List(), List(CString, MString), MString)
    val concat11 = op (Str) ("+", infix, List(), List(MString, MString), MString)
    
    // TODO: we would like overloaded variants to possibly use the same codegen impl instead of being redundant here    
    // most of the concat codegens are not used, but it is not easy to tell which ones will "make it"
    // should overloading be more explicit in the spec to avoid this problem? (an 'overloaded' parameter?)
    // at the very least, we should check for inconsistent codegen rules (or impls) between overloaded variants that collapse to the same thing
    def scalaStrConcat(o: Rep[DSLOp]) = o.quotedArg(0)+".toString + " + o.quotedArg(1)+".toString"
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
    codegen (concat11) ($cala, scalaStrConcat(concat11))
  }
  
  def math() = {
    val Math = grp("Math")
    
    val maxInt = op (Math) ("max", static, List(), List(MInt,MInt), MInt)
    val maxDbl = op (Math) ("max", static, List(), List(MDouble,MDouble), MDouble)
    
    codegen (maxInt) ($cala, "scala.math.max(" + quotedArg(0) + ", " + quotedArg(1) + ")")
    codegen (maxDbl) ($cala, "scala.math.max(" + quotedArg(0) + ", " + quotedArg(1) + ")")
  }
  
  def booleans() = {
    val Bool = grp("Boolean")
    lift (Bool) (MBoolean)    
  }
}
