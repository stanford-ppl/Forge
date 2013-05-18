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
    
    val println = direct (Misc) ("println", List(), List(MAny) :: MUnit, effect = simple)
    val println2 = direct (Misc) ("println", List(), List() :: MUnit, effect = simple)
    impl (println) (codegen($cala, "println(" + quotedArg(0) + ")"))
    impl (println2) (codegen($cala, "println()"))
    
    val whileDo = direct (Misc) ("__whileDo", List(), List(MThunk(MBoolean),MThunk(MUnit)) :: MUnit, effect = simple)        
    
    // function (block) arguments should be referenced using $b[<arg name>]
    impl (whileDo) (codegen($cala, ${
      while ($b[0]) {
        $b[1]
      }
    }))
    
    // TODO: something is broken with IfThenElse here; bound symbols (effects) are getting hoisted if the frequencies are not set to cold.    
    val T = tpePar("T")
    val ifThenElse = direct (Misc) ("__ifThenElse", List(T), List(MThunk(MBoolean),MThunk(T,cold),MThunk(T,cold)) :: T) 
    impl (ifThenElse) (codegen($cala, ${
      if ($b[0]) {
        $b[1]
      } 
      else {
        $b[2]
      }
    }))
    
    val immutable = infix (Misc) ("unsafeImmutable", List(T), List(T) :: T, aliasHint = copies(0))
    impl (immutable) (codegen($cala, quotedArg(0)))
  }
  
  def numerics() = {
    val Num = grp("Numeric")    
    val T = tpePar("T")    
    
    lift (Num) (T withBound TNumeric)
        
    val zero = infix (Num) ("zero", List(T withBound TNumeric), List() :: T)
    val plus = infix (Num) ("+", List(T withBound TNumeric), List(T,T) :: T)    
    val minus = infix (Num) ("-", List(T withBound TNumeric), List(T,T) :: T)     
    val times = infix (Num) ("*", List(T withBound TNumeric), List(T,T) :: T)    
    impl (zero) (codegen($cala, "implicitly[Numeric["+quotedTpe(0,zero)+"]].zero"))
    impl (plus) (codegen($cala, quotedArg(0) + " + " + quotedArg(1)))
    impl (minus) (codegen($cala, quotedArg(0) + " - " + quotedArg(1)))
    impl (times) (codegen($cala, quotedArg(0) + " * " + quotedArg(1)))
  }
  
  def ordering() = {
    val Ord = grp("Ordering2") // Ordering gets pulled in by DeliteArrayOps, need to disambiguate
    val T = tpePar("T")
    
    val lt = infix (Ord) ("<", List(T withBound TOrdering), List(T,T) :: MBoolean)    
    val gt = infix (Ord) (">", List(T withBound TOrdering), List(T,T) :: MBoolean)    
    
    impl (lt) (codegen($cala, quotedArg(0) + " < " + quotedArg(1)))    
    impl (gt) (codegen($cala, quotedArg(0) + " > " + quotedArg(1)))
  }
  
  def strings() = {
    val Str = grp("String")
    lift (Str) (MString)
    
    // overloaded variants of string concat
    val T = tpePar("T") 

    // most of these variants collapse to a common back-end implementation:
    
    // maps to Rep[String], Rep[Any]
    val concat = infix (Str) ("+", List(T), List(CString, T) :: MString)
    val concat2 = infix (Str) ("+", List(T), List(MString, T) :: MString)
    val concat3 = infix (Str) ("+", List(T), List(CString, MVar(T)) :: MString)
    val concat4 = infix (Str) ("+", List(T), List(MString, MVar(T)) :: MString)
    
    // Rep[Any], Rep[String]
    val concat5 = infix (Str) ("+", List(T), List(T, CString) :: MString)
    val concat6 = infix (Str) ("+", List(T), List(T, MString) :: MString)
    val concat7 = infix (Str) ("+", List(T), List(MVar(T), CString) :: MString)
    val concat8 = infix (Str) ("+", List(T), List(MVar(T), MString) :: MString)
    
    // Rep[String], Rep[String]
    val concat9 = infix (Str) ("+", List(), List(MString, CString) :: MString)
    val concat10 = infix (Str) ("+", List(), List(CString, MString) :: MString)
    val concat11 = infix (Str) ("+", List(), List(MString, MString) :: MString)
    
    // TODO: we would like overloaded variants to possibly use the same codegen impl instead of being redundant here    
    // most of the concat codegens are not used, but it is not easy to tell which ones will "make it"
    // should overloading be more explicit in the spec to avoid this problem? (an 'overloaded' parameter?)
    // at the very least, we should check for inconsistent codegen rules (or impls) between overloaded variants that collapse to the same thing
    def scalaStrConcat(o: Rep[DSLOp]) = quotedArg(0)+".toString + " + quotedArg(1)+".toString"
    impl (concat) (codegen($cala, scalaStrConcat(concat)))
    impl (concat2) (codegen($cala, scalaStrConcat(concat)))
    impl (concat3) (codegen($cala, scalaStrConcat(concat)))
    impl (concat4) (codegen($cala, scalaStrConcat(concat)))
    impl (concat5) (codegen($cala, scalaStrConcat(concat)))
    impl (concat6) (codegen($cala, scalaStrConcat(concat)))
    impl (concat7) (codegen($cala, scalaStrConcat(concat)))
    impl (concat8) (codegen($cala, scalaStrConcat(concat)))
    impl (concat9) (codegen($cala, scalaStrConcat(concat)))
    impl (concat10) (codegen($cala, scalaStrConcat(concat)))
    impl (concat11) (codegen($cala, scalaStrConcat(concat)))    
  }
  
  def math() = {
    val Math = grp("Math")
    
    val maxInt = static (Math) ("max", List(), List(MInt,MInt) :: MInt)
    val maxDbl = static (Math) ("max", List(), List(MDouble,MDouble) :: MDouble)
    
    impl (maxInt) (codegen($cala, "scala.math.max(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
    impl (maxDbl) (codegen($cala, "scala.math.max(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
  }
  
  def booleans() = {
    val Bool = grp("Boolean")
    lift (Bool) (MBoolean)    
  }
}
