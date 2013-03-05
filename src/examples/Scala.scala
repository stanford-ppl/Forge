package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

trait ScalaOps extends ForgeApplication {
  
  def addScalaOps() = {
    proxies()
    arrays()
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

  def arrays() = {
    val stream = ForgePrinter()    
    val T = tpePar("T")
    val R = tpePar("R")
    val Arr = GArray(T)
    
    // can't overload 'apply', because varArgs(T) is ambiguous with MInt
    val anew = unnamed_op (Arr) ("empty", static, List(T), List(MInt), Arr, codegenerated, effect = mutable)
    val anew2 = unnamed_op (Arr) ("apply", static, List(T), List(varArgs(T)), Arr, codegenerated)
    
    val alength = unnamed_op (Arr) ("length", infix, List(T), List(Arr), MInt, codegenerated)    
    val aplength = unnamed_op (Arr) ("plength", infix, List(T), List(Arr,MFunction(List(MInt),MInt)), MInt, single(MInt, { stream.printLines(quotedArg(1)+"(3)")}))    
    val aapply = unnamed_op (Arr) ("apply", infix, List(T), List(Arr,MInt), T, codegenerated)
    val aupdate = unnamed_op (Arr) ("update", infix, List(T), List(Arr,MInt,T), MUnit, codegenerated, effect = write(0))    
    //val amap = unnamed_op (Arr) ("a_map", infix, List(T,R), List(Arr,MFunction(List(T),R)), GArray(R), map((T,R,Arr), 0, "e => "+quotedArg(1)+"(e)"))
    val amap = unnamed_op (Arr) ("map", infix, List(T,R), List(Arr,MFunction(List(T),R)), GArray(R), single(GArray(R), {stream.printLines(
      "val length = " + quotedArg(0) + ".length()",
      "val arx = Array.empty[R](length)",
      "var i = 0 ",
      "while(i < length) {",
        "arx(i) = "+quotedArg(1)+"("+quotedArg(0)+"(i))",
        "i += 1",
      "}",
      "arx"
    )}))

    // the alias hint tells Delite that this operation copies its inputs, avoiding conservative mutable sharing errors     
    val aclone = unnamed_op (Arr) ("Clone", infix, List(T), List(Arr), Arr, codegenerated, aliasHint = copies(0))            

    // Arrays cannot be DeliteCollection right now because Delite expects us to use native
    // Scala arrays for some things (e.g. args); therefore we cannot shadow the native type with our own.
    // We could fix this in Delite by making DeliteCollection a type class instead of a subtype.
    
    // Arr is DeliteCollection(T, anew, alength, aapply, aupdate)
    
    codegen (anew) ($cala, "new "+anew.tpeName+"["+anew.tpeInstance(0)+"]("+quotedArg(0)+")")
    codegen (anew2) ($cala, quotedSeq(0)+".toArray")  // arg0: Seq[Rep[T]], so we use the quote method quoteSeq
    codegen (alength) ($cala, quotedArg(0) + ".length")
    codegen (aapply) ($cala, quotedArg(0) + ".apply(" + quotedArg(1) + ")")
    codegen (aupdate) ($cala, quotedArg(0) + ".update(" + quotedArg(1) + ", " + quotedArg(2) + ")")                
    codegen (aclone) ($cala, quotedArg(0) + ".clone")
  }  
  
  def misc() = {
    val Misc = grp("Misc")
    
    val println = unnamed_op (Misc) ("println", direct, List(), List(MAny), MUnit, codegenerated, effect = simple)
    val println2 = unnamed_op (Misc) ("println", direct, List(), List(), MUnit, codegenerated, effect = simple)
    codegen (println) ($cala, "println(" + println.quotedArg(0) + ")")
    codegen (println2) ($cala, "println()")
    
    val whileDo = unnamed_op (Misc) ("__whileDo", direct, List(), List(MThunk(MBoolean),MThunk(MUnit)), MUnit, codegenerated, effect = simple)        
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
    val ifThenElse = unnamed_op (Misc) ("__ifThenElse", direct, List(T), List(MThunk(MBoolean),MThunk(T,cold),MThunk(T,cold)), T, codegenerated) 
    codegen (ifThenElse) ($cala, stream.printLines(
      "if (",
        blockResult(ifThenElse, 0),
      "){",
        blockResult(ifThenElse, 1),
      "} else {",
        blockResult(ifThenElse, 2),
      "}"
    ))    
  }
  
  def variables() = {
    // lots of quirks in the LMS file, just plug in external implementation for now
    extern(grp("Var"), withLift = true)    
  }
  
  def numerics() = {
    val Num = grp("Numeric")    
    val T = tpePar("T")    
    
    lift (Num) (T withBound TNumeric)
        
    val zero = unnamed_op (Num) ("zero", infix, List(T withBound TNumeric), List(), T, codegenerated)
    val plus = unnamed_op (Num) ("+", infix, List(T withBound TNumeric), List(T,T), T, codegenerated)    
    val minus = unnamed_op (Num) ("-", infix, List(T withBound TNumeric), List(T,T), T, codegenerated)     
    val times = unnamed_op (Num) ("*", infix, List(T withBound TNumeric), List(T,T), T, codegenerated)    
    codegen (zero) ($cala, "implicitly[Numeric["+zero.tpeInstance(0)+"]].zero")
    codegen (plus) ($cala, plus.quotedArg(0) + " + " + plus.quotedArg(1))
    codegen (minus) ($cala, minus.quotedArg(0) + " - " + minus.quotedArg(1))
    codegen (times) ($cala, times.quotedArg(0) + " * " + times.quotedArg(1))
  }
  
  def ordering() = {
    val Ord = grp("Ordering")
    val T = tpePar("T")
    
    val lt = unnamed_op (Ord) ("<", infix, List(T withBound TOrdering), List(T,T), MBoolean, codegenerated)    
    val gt = unnamed_op (Ord) (">", infix, List(T withBound TOrdering), List(T,T), MBoolean, codegenerated)    
    
    codegen (lt) ($cala, lt.quotedArg(0) + " < " + lt.quotedArg(1))    
    codegen (gt) ($cala, gt.quotedArg(0) + " > " + gt.quotedArg(1))
  }
  
  def strings() = {
    val Str = grp("String")
    lift (Str) (MString)
    
    // overloaded variants of string concat
    val CString = tpe("String", stage = now) 
    val T = tpePar("T") 
/*
    val replaceAll = unnamed_op (Str) ("replaceAll", infix, List(), List(MString, MString, MString), MString, codegenerated)
    val replaceAll2 = unnamed_op (Str) ("replaceAll", infix, List(), List(CString, MString, MString), MString, codegenerated)
    codegen (replaceAll) ($cala, quotedArg(0)+".replaceAll("+quotedArg(1)+", "+quotedArg(2)+")")
    codegen (replaceAll2) ($cala, quotedArg(0)+".replaceAll("+quotedArg(1)+", "+quotedArg(2)+")")
*/
    // most of these variants collapse to a common back-end implementation:
    
    // maps to Rep[String], Rep[Any]
    val concat = unnamed_op (Str) ("+", infix, List(T), List(CString, T), MString, codegenerated)
    val concat2 = unnamed_op (Str) ("+", infix, List(T), List(MString, T), MString, codegenerated)
    val concat3 = unnamed_op (Str) ("+", infix, List(T), List(CString, GVar(T)), MString, codegenerated)
    val concat4 = unnamed_op (Str) ("+", infix, List(T), List(MString, GVar(T)), MString, codegenerated)
    
    // Rep[Any], Rep[String]
    val concat5 = unnamed_op (Str) ("+", infix, List(T), List(T, CString), MString, codegenerated)
    val concat6 = unnamed_op (Str) ("+", infix, List(T), List(T, MString), MString, codegenerated)
    val concat7 = unnamed_op (Str) ("+", infix, List(T), List(GVar(T), CString), MString, codegenerated)
    val concat8 = unnamed_op (Str) ("+", infix, List(T), List(GVar(T), MString), MString, codegenerated)
    
    // Rep[String], Rep[String]
    val concat9 = unnamed_op (Str) ("+", infix, List(), List(MString, CString), MString, codegenerated)
    val concat10 = unnamed_op (Str) ("+", infix, List(), List(CString, MString), MString, codegenerated)
    val concat11 = unnamed_op (Str) ("+", infix, List(), List(MString, MString), MString, codegenerated)
    
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
}
