package ppl.dsl.forge
package library

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

/**
 * This file re-implements LMS common ops in Forge.
 * 
 * Note that Delite pulls in some LMS common ops directly, requiring name disambiguation here.
 * However, so far it does not look like there are any major problems (e.g. ambiguous implicits) from including both versions in a DSL.
 */
trait ScalaOps {
  this: ForgeApplication =>
  
  def importScalaOps() = {
    importProxies()
    importPrimitives()
    importMisc()
    importCasts()
    importNumerics()
    importOrdering()
    importStrings()
    importMath()
    importTuples()
  }
    
  /**
   * TODO: Use reflection to auto-generate the ops and code generators
   * Question: how do we handle effects?
   *   withWrites("update", "insert", ...)?
   */
  def importProxies() = {
    // proxy(scala.collection.immutable.Array)      
  }
  
  def importPrimitives() = {
    val Prim = grp("Primitive2") // conflicts with PrimitiveOps from LMS brought in by Delite
    val T = tpePar("T")
    
    lift (Prim) (MBoolean)    
    
    // why do these conflict with Delite Boolean ops and not the others in Prim?
    // the reason seems to be because of the combination of overloaded parameters that already exist in the LMS versions of the other ops. (i.e., we got lucky)
    infix (Prim) ("unary_!", Nil, MBoolean :: MBoolean) implements codegen($cala, "!" + quotedArg(0))
    infix (Prim) ("||", Nil, (MBoolean, MBoolean) :: MBoolean) implements codegen($cala, quotedArg(0) + " || " + quotedArg(1))
    infix (Prim) ("&&", Nil, (MBoolean, MBoolean) :: MBoolean) implements codegen($cala, quotedArg(0) + " && " + quotedArg(1))
    
    lift (Prim) (MInt)
    lift (Prim) (MFloat)
    lift (Prim) (MDouble)
    
    infix (Prim) ("toInt", T withBound TNumeric, T :: MInt) implements codegen($cala, ${ $0.toInt })
    infix (Prim) ("toFloat", T withBound TNumeric, T :: MFloat) implements codegen($cala, ${ $0.toFloat })
    infix (Prim) ("toDouble", T withBound TNumeric, T :: MDouble) implements codegen($cala, ${ $0.toDouble })
    
    fimplicit (Prim) ("repInt2ToRepDouble", Nil, MInt :: MDouble) implements composite ${ $0.toDouble }
    fimplicit (Prim) ("repInt2ToRepFloat", Nil, MInt :: MFloat) implements composite ${ $0.toFloat }
    fimplicit (Prim) ("repFloat2ToRepDouble", Nil, MFloat :: MDouble) implements composite ${ $0.toDouble }
    
    infix (Prim) ("%", Nil, (MInt,MInt) :: MInt) implements codegen($cala, ${$0 % $1})
    
    // specialized versions for primitives     
    // the forge_ prefix is to avoid conflicting with LMS primitive ops
    val int_plus = direct (Prim) ("forge_int_plus", Nil, (MInt,MInt) :: MInt) 
    impl (int_plus) (codegen($cala, ${$0 + $1}))
    impl (int_plus) (codegen(cuda, ${$0 + $1}))
    impl (int_plus) (codegen(cpp, ${$0 + $1}))
    val int_minus = direct (Prim) ("forge_int_minus", Nil, (MInt,MInt) :: MInt) 
    impl (int_minus) (codegen($cala, ${$0 - $1}))
    impl (int_minus) (codegen(cuda, ${$0 - $1}))
    impl (int_minus) (codegen(cpp, ${$0 - $1}))
    val int_times = direct (Prim) ("forge_int_times", Nil, (MInt,MInt) :: MInt) 
    impl (int_times) (codegen($cala, ${$0 * $1}))
    impl (int_times) (codegen(cuda, ${$0 * $1}))
    impl (int_times) (codegen(cpp, ${$0 * $1}))
    val int_divide = direct (Prim) ("forge_int_divide", Nil, (MInt,MInt) :: MInt) 
    impl (int_divide) (codegen($cala, ${$0 / $1}))
    impl (int_divide) (codegen(cuda, ${$0 / $1}))
    impl (int_divide) (codegen(cpp, ${$0 / $1}))

    direct (Prim) ("forge_int_shift_left", Nil, (MInt,MInt) :: MInt) implements codegen($cala, ${$0 << $1})
    
    direct (Prim) ("forge_float_plus", Nil, (MFloat,MFloat) :: MFloat) implements codegen($cala, ${$0 + $1})
    direct (Prim) ("forge_float_minus", Nil, (MFloat,MFloat) :: MFloat) implements codegen($cala, ${$0 - $1})
    direct (Prim) ("forge_float_times", Nil, (MFloat,MFloat) :: MFloat) implements codegen($cala, ${$0 * $1})
    direct (Prim) ("forge_float_divide", Nil, (MFloat,MFloat) :: MFloat) implements codegen($cala, ${$0 / $1})    
    
    val double_plus = direct (Prim) ("forge_double_plus", Nil, (MDouble,MDouble) :: MDouble) 
    impl (double_plus) (codegen($cala, ${$0 + $1}))
    impl (double_plus) (codegen(cuda, ${$0 + $1}))
    impl (double_plus) (codegen(cpp, ${$0 + $1}))
    val double_minus = direct (Prim) ("forge_double_minus", Nil, (MDouble,MDouble) :: MDouble)
    impl (double_minus) (codegen($cala, ${$0 - $1}))
    impl (double_minus) (codegen(cuda, ${$0 - $1}))
    impl (double_minus) (codegen(cpp, ${$0 - $1}))
    val double_times = direct (Prim) ("forge_double_times", Nil, (MDouble,MDouble) :: MDouble)
    impl (double_times) (codegen($cala, ${$0 * $1}))
    impl (double_times) (codegen(cuda, ${$0 * $1}))
    impl (double_times) (codegen(cpp, ${$0 * $1}))
    val double_divide = direct (Prim) ("forge_double_divide", Nil, (MDouble,MDouble) :: MDouble)        
    impl (double_divide) (codegen($cala, ${$0 / $1}))
    impl (double_divide) (codegen(cuda, ${$0 / $1}))
    impl (double_divide) (codegen(cpp, ${$0 / $1}))
    
    // can we auto-generate these? the tricky part is the bodies, which require explicit conversions..
    // infix (Prim) ("+", Nil, enumerate(CInt,MInt,CFloat,MFloat,CDouble,MDouble)) implements codegen($cala, quotedArg(0) + " + " + quotedArg(1))    
    infix (Prim) ("+", Nil, (CInt,MInt) :: MInt) implements redirect ${ forge_int_plus(unit($0),$1) }
    infix (Prim) ("+", Nil, (CInt,MFloat) :: MFloat) implements redirect ${ forge_float_plus(unit($0.toFloat),$1) }
    infix (Prim) ("+", Nil, (CInt,MDouble) :: MDouble) implements redirect ${ forge_double_plus(unit($0.toDouble),$1) }
    infix (Prim) ("+", Nil, (CFloat,MInt) :: MFloat) implements redirect ${ forge_float_plus(unit($0),$1.toFloat) }
    infix (Prim) ("+", Nil, (CFloat,MFloat) :: MFloat) implements redirect ${ forge_float_plus(unit($0),$1) }
    infix (Prim) ("+", Nil, (CFloat,MDouble) :: MDouble) implements redirect ${ forge_double_plus(unit($0.toDouble),$1) }
    infix (Prim) ("+", Nil, (CDouble,MInt) :: MDouble) implements redirect ${ forge_double_plus(unit($0),$1.toDouble) }
    infix (Prim) ("+", Nil, (CDouble,MFloat) :: MDouble) implements redirect ${ forge_double_plus(unit($0),$1.toDouble) }
    infix (Prim) ("+", Nil, (CDouble,MDouble) :: MDouble) implements redirect ${ forge_double_plus(unit($0),$1) }
    infix (Prim) ("+", Nil, (MInt,CInt) :: MInt) implements redirect ${ forge_int_plus($0,unit($1)) }
    infix (Prim) ("+", Nil, (MInt,CFloat) :: MFloat) implements redirect ${ forge_float_plus($0.toFloat,unit($1)) }
    infix (Prim) ("+", Nil, (MInt,CDouble) :: MDouble) implements redirect ${ forge_double_plus($0.toDouble,unit($1)) }
    infix (Prim) ("+", Nil, (MFloat,CInt) :: MFloat) implements redirect ${ forge_float_plus($0,unit($1.toFloat)) }
    infix (Prim) ("+", Nil, (MFloat,CFloat) :: MFloat) implements redirect ${ forge_float_plus($0,unit($1)) }
    infix (Prim) ("+", Nil, (MFloat,CDouble) :: MDouble) implements redirect ${ forge_double_plus($0.toDouble,unit($1)) }
    infix (Prim) ("+", Nil, (MDouble,CInt) :: MDouble) implements redirect ${ forge_double_plus($0,unit($1.toDouble)) }
    infix (Prim) ("+", Nil, (MDouble,CFloat) :: MDouble) implements redirect ${ forge_double_plus($0.toDouble,unit($1)) }
    infix (Prim) ("+", Nil, (MDouble,CDouble) :: MDouble) implements redirect ${ forge_double_plus($0,unit($1)) }
    infix (Prim) ("+", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_plus($0,$1) }
    infix (Prim) ("+", Nil, (MInt,MFloat) :: MFloat) implements redirect ${ forge_float_plus($0.toFloat,$1) }
    infix (Prim) ("+", Nil, (MInt,MDouble) :: MDouble) implements redirect ${ forge_double_plus($0.toDouble,$1) }
    infix (Prim) ("+", Nil, (MFloat,MInt) :: MFloat) implements redirect ${ forge_float_plus($0,$1.toFloat) }
    infix (Prim) ("+", Nil, (MFloat,MFloat) :: MFloat) implements redirect ${ forge_float_plus($0,$1) }
    infix (Prim) ("+", Nil, (MFloat,MDouble) :: MDouble) implements redirect ${ forge_double_plus($0.toDouble,$1) }
    infix (Prim) ("+", Nil, (MDouble,MInt) :: MDouble) implements redirect ${ forge_double_plus($0,$1.toDouble) }
    infix (Prim) ("+", Nil, (MDouble,MFloat) :: MDouble) implements redirect ${ forge_double_plus($0,$1.toDouble) }
    infix (Prim) ("+", Nil, (MDouble,MDouble) :: MDouble) implements redirect ${ forge_double_plus($0,$1) }    
    
    infix (Prim) ("-", Nil, (CInt,MInt) :: MInt) implements redirect ${ forge_int_minus(unit($0),$1) }
    infix (Prim) ("-", Nil, (CInt,MFloat) :: MFloat) implements redirect ${ forge_float_minus(unit($0.toFloat),$1) }
    infix (Prim) ("-", Nil, (CInt,MDouble) :: MDouble) implements redirect ${ forge_double_minus(unit($0.toDouble),$1) }
    infix (Prim) ("-", Nil, (CFloat,MInt) :: MFloat) implements redirect ${ forge_float_minus(unit($0),$1.toFloat) }
    infix (Prim) ("-", Nil, (CFloat,MFloat) :: MFloat) implements redirect ${ forge_float_minus(unit($0),$1) }
    infix (Prim) ("-", Nil, (CFloat,MDouble) :: MDouble) implements redirect ${ forge_double_minus(unit($0.toDouble),$1) }
    infix (Prim) ("-", Nil, (CDouble,MInt) :: MDouble) implements redirect ${ forge_double_minus(unit($0),$1.toDouble) }
    infix (Prim) ("-", Nil, (CDouble,MFloat) :: MDouble) implements redirect ${ forge_double_minus(unit($0),$1.toDouble) }
    infix (Prim) ("-", Nil, (CDouble,MDouble) :: MDouble) implements redirect ${ forge_double_minus(unit($0),$1) }
    infix (Prim) ("-", Nil, (MInt,CInt) :: MInt) implements redirect ${ forge_int_minus($0,unit($1)) }
    infix (Prim) ("-", Nil, (MInt,CFloat) :: MFloat) implements redirect ${ forge_float_minus($0.toFloat,unit($1)) }
    infix (Prim) ("-", Nil, (MInt,CDouble) :: MDouble) implements redirect ${ forge_double_minus($0.toDouble,unit($1)) }
    infix (Prim) ("-", Nil, (MFloat,CInt) :: MFloat) implements redirect ${ forge_float_minus($0,unit($1.toFloat)) }
    infix (Prim) ("-", Nil, (MFloat,CFloat) :: MFloat) implements redirect ${ forge_float_minus($0,unit($1)) }
    infix (Prim) ("-", Nil, (MFloat,CDouble) :: MDouble) implements redirect ${ forge_double_minus($0.toDouble,unit($1)) }
    infix (Prim) ("-", Nil, (MDouble,CInt) :: MDouble) implements redirect ${ forge_double_minus($0,unit($1.toDouble)) }
    infix (Prim) ("-", Nil, (MDouble,CFloat) :: MDouble) implements redirect ${ forge_double_minus($0.toDouble,unit($1)) }
    infix (Prim) ("-", Nil, (MDouble,CDouble) :: MDouble) implements redirect ${ forge_double_minus($0,unit($1)) }
    infix (Prim) ("-", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_minus($0,$1) }
    infix (Prim) ("-", Nil, (MInt,MFloat) :: MFloat) implements redirect ${ forge_float_minus($0.toFloat,$1) }
    infix (Prim) ("-", Nil, (MInt,MDouble) :: MDouble) implements redirect ${ forge_double_minus($0.toDouble,$1) }
    infix (Prim) ("-", Nil, (MFloat,MInt) :: MFloat) implements redirect ${ forge_float_minus($0,$1.toFloat) }
    infix (Prim) ("-", Nil, (MFloat,MFloat) :: MFloat) implements redirect ${ forge_float_minus($0,$1) }
    infix (Prim) ("-", Nil, (MFloat,MDouble) :: MDouble) implements redirect ${ forge_double_minus($0.toDouble,$1) }
    infix (Prim) ("-", Nil, (MDouble,MInt) :: MDouble) implements redirect ${ forge_double_minus($0,$1.toDouble) }
    infix (Prim) ("-", Nil, (MDouble,MFloat) :: MDouble) implements redirect ${ forge_double_minus($0,$1.toDouble) }
    infix (Prim) ("-", Nil, (MDouble,MDouble) :: MDouble) implements redirect ${ forge_double_minus($0,$1) }    
    
    infix (Prim) ("*", Nil, (CInt,MInt) :: MInt) implements redirect ${ forge_int_times(unit($0),$1) }
    infix (Prim) ("*", Nil, (CInt,MFloat) :: MFloat) implements redirect ${ forge_float_times(unit($0.toFloat),$1) }
    infix (Prim) ("*", Nil, (CInt,MDouble) :: MDouble) implements redirect ${ forge_double_times(unit($0.toDouble),$1) }
    infix (Prim) ("*", Nil, (CFloat,MInt) :: MFloat) implements redirect ${ forge_float_times(unit($0),$1.toFloat) }
    infix (Prim) ("*", Nil, (CFloat,MFloat) :: MFloat) implements redirect ${ forge_float_times(unit($0),$1) }
    infix (Prim) ("*", Nil, (CFloat,MDouble) :: MDouble) implements redirect ${ forge_double_times(unit($0.toDouble),$1) }
    infix (Prim) ("*", Nil, (CDouble,MInt) :: MDouble) implements redirect ${ forge_double_times(unit($0),$1.toDouble) }
    infix (Prim) ("*", Nil, (CDouble,MFloat) :: MDouble) implements redirect ${ forge_double_times(unit($0),$1.toDouble) }
    infix (Prim) ("*", Nil, (CDouble,MDouble) :: MDouble) implements redirect ${ forge_double_times(unit($0),$1) }
    infix (Prim) ("*", Nil, (MInt,CInt) :: MInt) implements redirect ${ forge_int_times($0,unit($1)) }
    infix (Prim) ("*", Nil, (MInt,CFloat) :: MFloat) implements redirect ${ forge_float_times($0.toFloat,unit($1)) }
    infix (Prim) ("*", Nil, (MInt,CDouble) :: MDouble) implements redirect ${ forge_double_times($0.toDouble,unit($1)) }
    infix (Prim) ("*", Nil, (MFloat,CInt) :: MFloat) implements redirect ${ forge_float_times($0,unit($1.toFloat)) }
    infix (Prim) ("*", Nil, (MFloat,CFloat) :: MFloat) implements redirect ${ forge_float_times($0,unit($1)) }
    infix (Prim) ("*", Nil, (MFloat,CDouble) :: MDouble) implements redirect ${ forge_double_times($0.toDouble,unit($1)) }
    infix (Prim) ("*", Nil, (MDouble,CInt) :: MDouble) implements redirect ${ forge_double_times($0,unit($1.toDouble)) }
    infix (Prim) ("*", Nil, (MDouble,CFloat) :: MDouble) implements redirect ${ forge_double_times($0.toDouble,unit($1)) }
    infix (Prim) ("*", Nil, (MDouble,CDouble) :: MDouble) implements redirect ${ forge_double_times($0,unit($1)) }
    infix (Prim) ("*", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_times($0,$1) }
    infix (Prim) ("*", Nil, (MInt,MFloat) :: MFloat) implements redirect ${ forge_float_times($0.toFloat,$1) }
    infix (Prim) ("*", Nil, (MInt,MDouble) :: MDouble) implements redirect ${ forge_double_times($0.toDouble,$1) }
    infix (Prim) ("*", Nil, (MFloat,MInt) :: MFloat) implements redirect ${ forge_float_times($0,$1.toFloat) }
    infix (Prim) ("*", Nil, (MFloat,MFloat) :: MFloat) implements redirect ${ forge_float_times($0,$1) }
    infix (Prim) ("*", Nil, (MFloat,MDouble) :: MDouble) implements redirect ${ forge_double_times($0.toDouble,$1) }
    infix (Prim) ("*", Nil, (MDouble,MInt) :: MDouble) implements redirect ${ forge_double_times($0,$1.toDouble) }
    infix (Prim) ("*", Nil, (MDouble,MFloat) :: MDouble) implements redirect ${ forge_double_times($0,$1.toDouble) }
    infix (Prim) ("*", Nil, (MDouble,MDouble) :: MDouble) implements redirect ${ forge_double_times($0,$1) }    

    infix (Prim) ("/", Nil, (CInt,MInt) :: MInt) implements redirect ${ forge_int_divide(unit($0),$1) }
    infix (Prim) ("/", Nil, (CInt,MFloat) :: MFloat) implements redirect ${ forge_float_divide(unit($0.toFloat),$1) }
    infix (Prim) ("/", Nil, (CInt,MDouble) :: MDouble) implements redirect ${ forge_double_divide(unit($0.toDouble),$1) }
    infix (Prim) ("/", Nil, (CFloat,MInt) :: MFloat) implements redirect ${ forge_float_divide(unit($0),$1.toFloat) }
    infix (Prim) ("/", Nil, (CFloat,MFloat) :: MFloat) implements redirect ${ forge_float_divide(unit($0),$1) }
    infix (Prim) ("/", Nil, (CFloat,MDouble) :: MDouble) implements redirect ${ forge_double_divide(unit($0.toDouble),$1) }
    infix (Prim) ("/", Nil, (CDouble,MInt) :: MDouble) implements redirect ${ forge_double_divide(unit($0),$1.toDouble) }
    infix (Prim) ("/", Nil, (CDouble,MFloat) :: MDouble) implements redirect ${ forge_double_divide(unit($0),$1.toDouble) }
    infix (Prim) ("/", Nil, (CDouble,MDouble) :: MDouble) implements redirect ${ forge_double_divide(unit($0),$1) }
    infix (Prim) ("/", Nil, (MInt,CInt) :: MInt) implements redirect ${ forge_int_divide($0,unit($1)) }
    infix (Prim) ("/", Nil, (MInt,CFloat) :: MFloat) implements redirect ${ forge_float_divide($0.toFloat,unit($1)) }
    infix (Prim) ("/", Nil, (MInt,CDouble) :: MDouble) implements redirect ${ forge_double_divide($0.toDouble,unit($1)) }
    infix (Prim) ("/", Nil, (MFloat,CInt) :: MFloat) implements redirect ${ forge_float_divide($0,unit($1.toFloat)) }
    infix (Prim) ("/", Nil, (MFloat,CFloat) :: MFloat) implements redirect ${ forge_float_divide($0,unit($1)) }
    infix (Prim) ("/", Nil, (MFloat,CDouble) :: MDouble) implements redirect ${ forge_double_divide($0.toDouble,unit($1)) }
    infix (Prim) ("/", Nil, (MDouble,CInt) :: MDouble) implements redirect ${ forge_double_divide($0,unit($1.toDouble)) }
    infix (Prim) ("/", Nil, (MDouble,CFloat) :: MDouble) implements redirect ${ forge_double_divide($0.toDouble,unit($1)) }
    infix (Prim) ("/", Nil, (MDouble,CDouble) :: MDouble) implements redirect ${ forge_double_divide($0,unit($1)) }
    infix (Prim) ("/", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_divide($0,$1) }
    infix (Prim) ("/", Nil, (MInt,MFloat) :: MFloat) implements redirect ${ forge_float_divide($0.toFloat,$1) }
    infix (Prim) ("/", Nil, (MInt,MDouble) :: MDouble) implements redirect ${ forge_double_divide($0.toDouble,$1) }
    infix (Prim) ("/", Nil, (MFloat,MInt) :: MFloat) implements redirect ${ forge_float_divide($0,$1.toFloat) }
    infix (Prim) ("/", Nil, (MFloat,MFloat) :: MFloat) implements redirect ${ forge_float_divide($0,$1) }
    infix (Prim) ("/", Nil, (MFloat,MDouble) :: MDouble) implements redirect ${ forge_double_divide($0.toDouble,$1) }
    infix (Prim) ("/", Nil, (MDouble,MInt) :: MDouble) implements redirect ${ forge_double_divide($0,$1.toDouble) }
    infix (Prim) ("/", Nil, (MDouble,MFloat) :: MDouble) implements redirect ${ forge_double_divide($0,$1.toDouble) }
    infix (Prim) ("/", Nil, (MDouble,MDouble) :: MDouble) implements redirect ${ forge_double_divide($0,$1) }

    infix (Prim) ("<<",Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_shift_left($0,$1) }
  }
  
  def importMisc() = {
    val Misc = grp("Misc")
    
    direct (Misc) ("exit", Nil, MInt :: MUnit, effect = simple) implements codegen($cala, ${sys.exit($0)})
    direct (Misc) ("print", Nil, MAny :: MUnit, effect = simple) implements codegen($cala, ${print($0)})
    val fatal = direct (Misc) ("fatal", Nil, MString :: MNothing, effect = simple) 
    impl (fatal) (codegen($cala, ${throw new Exception($0)}))
    impl (fatal) (codegen(cuda, ${assert(0)}))
    impl (fatal) (codegen(cpp, ${assert(0)}))
    
    val println = direct (Misc) ("println", List(), List(MAny) :: MUnit, effect = simple)
    val println2 = direct (Misc) ("println", List(), List() :: MUnit, effect = simple)
    impl (println) (codegen($cala, "println(" + quotedArg(0) + ")"))
    impl (println2) (codegen($cala, "println()"))
    
    val whileDo = direct (Misc) ("__whileDo", List(), List(MThunk(MBoolean),MThunk(MUnit)) :: MUnit)        
    
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
  
  def importCasts() = {
    val Cast = grp("Cast")
    val A = tpePar("A")    
    val B = tpePar("B")    
    
    // these don't work as infix_ methods
    noInfixList :::= List("AsInstanceOf", "IsInstanceOf") 
    
    infix (Cast) ("AsInstanceOf", (A,B), A :: B) implements codegen($cala, ${ $0.asInstanceOf[$t[B]] })
    infix (Cast) ("IsInstanceOf", (A,B), A :: MBoolean) implements codegen($cala, ${ $0.isInstanceOf[$t[B]] })
  }
  
  def importNumerics() = {
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
    
    val Frac = grp("Fractional")    
    val R = tpePar("R")
    val div = infix (Frac) ("/", List(T,R withBound TFractional), (T,R) :: R, T ==> R)
    impl (div) (codegen($cala, ${ implicitly[Fractional[$t[R]]].div($0,$1) }))
  }
  
  def importOrdering() = {
    val Ord = grp("Ordering2") // Ordering gets pulled in by DeliteArrayOps, need to disambiguate
    val A = tpePar("A")
    val B = tpePar("B")    
    val AC = tpePar("A", stage = now)
    val BC = tpePar("B", stage = now)
    
    direct (Ord) ("__equal", (A,B), (A,B) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (A,B), (MVar(A),B) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (A,B), (A,MVar(B)) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (A,BC), (A,BC) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (AC,B), (AC,B) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    
    val neq = infix (Ord) ("!=", (A,B), (A,B) :: MBoolean) 
    impl (neq) (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
    impl (neq) (codegen(cuda, quotedArg(0) + " != " + quotedArg(1)))
    impl (neq) (codegen(cpp, quotedArg(0) + " != " + quotedArg(1)))
    infix (Ord) ("!=", (A,BC), (A,BC) :: MBoolean) implements (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
    infix (Ord) ("!=", (AC,B), (AC,B) :: MBoolean) implements (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
    
    val lt = infix (Ord) ("<", List(A withBound TOrdering), List(A,A) :: MBoolean)    
    val lte = infix (Ord) ("<=", List(A withBound TOrdering), List(A,A) :: MBoolean)    
    val gt = infix (Ord) (">", List(A withBound TOrdering), List(A,A) :: MBoolean)    
    val gte = infix (Ord) (">=", List(A withBound TOrdering), List(A,A) :: MBoolean)    
    
    impl (lt) (codegen($cala, quotedArg(0) + " < " + quotedArg(1)))    
    impl (lte) (codegen($cala, quotedArg(0) + " <= " + quotedArg(1)))    
    impl (gt) (codegen($cala, quotedArg(0) + " > " + quotedArg(1)))
    impl (gte) (codegen($cala, quotedArg(0) + " >= " + quotedArg(1)))
  }
  
  def importStrings() = {
    val Str = grp("FString")
    lift (Str) (MString)
    
    // overloaded variants of string concat
    val T = tpePar("T") 

    infix (Str) ("toInt", Nil, MString :: MInt) implements codegen($cala, ${ $0.toInt })
    infix (Str) ("toFloat", Nil, MString :: MFloat) implements codegen($cala, ${ $0.toFloat })
    infix (Str) ("toDouble", Nil, MString :: MDouble) implements codegen($cala, ${ $0.toDouble })
    
    // not much we can do here to use "split" as long as Delite brings in LMS' version, since we can't overload on the return type
    // we should refactor LMS/Delite to only use the StringOpsExp trait and not StringOps
    infix (Str) ("fsplit", Nil, ((MString,MString) :: MArray(MString))) implements composite ${
      array_string_split($0,$1)  
    }
    
    infix (Str) ("trim", Nil, MString :: MString) implements codegen($cala, ${ $0.trim })
    infix (Str) ("fcharAt", Nil, (MString,MInt) :: MChar) implements codegen($cala, ${ $0.charAt($1) })
    
    // most of these variants collapse to a common back-end implementation:
    
    // maps to Rep[String], Rep[Any]
    val concat = infix (Str) ("+", List(T), List(CString, T) :: MString)
    val concat2 = infix (Str) ("+", List(T), List(MString, T) :: MString)
    val concat3 = infix (Str) ("+", List(T), List(CString, MVar(T)) :: MString)
    val concat4 = infix (Str) ("+", List(T), List(MString, MVar(T)) :: MString)    
    val concat5 = infix (Str) ("+", List(T), List(MVar(MString), T) :: MString)
    val concat6 = infix (Str) ("+", List(T), List(MVar(MString), MVar(T)) :: MString)
    
    // Rep[Any], Rep[String]
    val concat7 = infix (Str) ("+", List(T), List(T, CString) :: MString)
    val concat8 = infix (Str) ("+", List(T), List(T, MString) :: MString)
    val concat9 = infix (Str) ("+", List(T), List(MVar(T), CString) :: MString)
    val concat10 = infix (Str) ("+", List(T), List(MVar(T), MString) :: MString)
    val concat11 = infix (Str) ("+", List(T), List(MVar(T), MVar(MString)) :: MString)
    
    // Rep[String], Rep[String]
    // val concat9 = infix (Str) ("+", List(), List(MString, CString) :: MString)
    val concat12 = infix (Str) ("+", List(), List(CString, MString) :: MString)
    val concat13 = infix (Str) ("+", List(), List(MString, MString) :: MString)
    val concat14 = infix (Str) ("+", List(), List(MString, MVar(MString)) :: MString)    
    val concat15 = infix (Str) ("+", List(), List(MVar(MString), MString) :: MString)
    val concat16 = infix (Str) ("+", List(), List(MVar(MString), MVar(MString)) :: MString)

    
    // TODO: we would like overloaded variants to possibly use the same codegen impl instead of being redundant here    
    // only the impl of the first declared canonical signature is actually used
    // should overloading be more explicit in the spec to avoid this problem? (an 'overloaded' parameter?)
    // at the very least, we should check for inconsistent codegen rules (or impls) between overloaded variants that collapse to the same thing
    def scalaStrConcat = quotedArg(0)+".toString + " + quotedArg(1)+".toString"
    for (o <- List(concat,concat2,concat3,concat4,concat5,concat6,concat7,concat8,concat9,concat10,concat11,concat12,concat13,concat14,concat15,concat16)) {    
      impl (o) (codegen($cala, scalaStrConcat))    
    }    
  }
  
  def importMath() = {
    val Math = grp("Math")
    
    // (compile-time) constants
    direct (Math) ("INF", Nil, Nil :: MDouble) implements redirect ${ unit(Double.PositiveInfinity) }
    direct (Math) ("nINF", Nil, Nil :: MDouble) implements redirect ${ unit(Double.NegativeInfinity) }
    direct (Math) ("Pi", Nil, Nil :: MDouble) implements redirect ${ unit(java.lang.Math.PI) }
    direct (Math) ("E", Nil, Nil :: MDouble) implements redirect ${ unit(java.lang.Math.E) }
  
    // methods    
    val abs = static (Math) ("abs", Nil, MDouble :: MDouble)
    val exp = static (Math) ("exp", Nil, MDouble :: MDouble)
    val log = static (Math) ("log", Nil, MDouble :: MDouble)
    val log10 = static (Math) ("log10", Nil, MDouble :: MDouble)    
    val sqrt = static (Math) ("sqrt", Nil, MDouble :: MDouble)
    val ceil = static (Math) ("ceil", Nil, MDouble :: MDouble)
    val floor = static (Math) ("floor", Nil, MDouble :: MDouble)    
    val sin = static (Math) ("sin", Nil, MDouble :: MDouble)
    val sinh = static (Math) ("sinh", Nil, MDouble :: MDouble)
    val asin = static (Math) ("asin", Nil, MDouble :: MDouble)
    val cos = static (Math) ("cos", Nil, MDouble :: MDouble)
    val cosh = static (Math) ("cosh", Nil, MDouble :: MDouble)
    val acos = static (Math) ("acos", Nil, MDouble :: MDouble)
    val tan = static (Math) ("tan", Nil, MDouble :: MDouble)
    val tanh = static (Math) ("tanh", Nil, MDouble :: MDouble)
    val atan = static (Math) ("atan", Nil, MDouble :: MDouble)
    val atan2 = static (Math) ("atan2", Nil, (MDouble, MDouble) :: MDouble)
    val pow = static (Math) ("pow", Nil, (MDouble, MDouble) :: MDouble)
    val max = static (Math) ("max", Nil, (MDouble,MDouble) :: MDouble)
    val min = static (Math) ("min", Nil, (MDouble,MDouble) :: MDouble)  
    
    impl (abs) (codegen($cala, "java.lang.Math.abs(" + quotedArg(0) + ")"))
    impl (exp) (codegen($cala, "java.lang.Math.exp(" + quotedArg(0) + ")"))
    impl (log) (codegen($cala, "java.lang.Math.log(" + quotedArg(0) + ")"))
    impl (log10) (codegen($cala, "java.lang.Math.log10(" + quotedArg(0) + ")"))
    impl (sqrt) (codegen($cala, "java.lang.Math.sqrt(" + quotedArg(0) + ")"))
    impl (ceil) (codegen($cala, "java.lang.Math.ceil(" + quotedArg(0) + ")"))
    impl (floor) (codegen($cala, "java.lang.Math.floor(" + quotedArg(0) + ")"))
    impl (sin) (codegen($cala, "java.lang.Math.sin(" + quotedArg(0) + ")"))
    impl (sinh) (codegen($cala, "java.lang.Math.sinh(" + quotedArg(0) + ")"))
    impl (asin) (codegen($cala, "java.lang.Math.asin(" + quotedArg(0) + ")"))
    impl (cos) (codegen($cala, "java.lang.Math.cos(" + quotedArg(0) + ")"))
    impl (cosh) (codegen($cala, "java.lang.Math.cosh(" + quotedArg(0) + ")"))
    impl (acos) (codegen($cala, "java.lang.Math.acos(" + quotedArg(0) + ")"))
    impl (tan) (codegen($cala, "java.lang.Math.tan(" + quotedArg(0) + ")"))
    impl (tanh) (codegen($cala, "java.lang.Math.tanh(" + quotedArg(0) + ")"))
    impl (atan) (codegen($cala, "java.lang.Math.atan(" + quotedArg(0) + ")"))
    impl (atan2) (codegen($cala, "java.lang.Math.atan2(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
    impl (pow) (codegen($cala, "java.lang.Math.pow(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
    impl (max) (codegen($cala, "java.lang.Math.max(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
    impl (min) (codegen($cala, "java.lang.Math.min(" + quotedArg(0) + ", " + quotedArg(1) + ")"))

    for (g <- List(cuda, cpp)) {
      impl (abs) (codegen(g, "abs(" + quotedArg(0) + ")"))
      impl (exp) (codegen(g, "exp(" + quotedArg(0) + ")"))
      impl (log) (codegen(g, "log(" + quotedArg(0) + ")"))
      impl (sqrt) (codegen(g, "sqrt(" + quotedArg(0) + ")"))
      impl (ceil) (codegen(g, "ceil(" + quotedArg(0) + ")"))
      impl (floor) (codegen(g, "floor(" + quotedArg(0) + ")"))
      impl (sin) (codegen(g, "sin(" + quotedArg(0) + ")"))
      impl (cos) (codegen(g, "cos(" + quotedArg(0) + ")"))
      impl (acos) (codegen(g, "acos(" + quotedArg(0) + ")"))
      impl (tan) (codegen(g, "tan(" + quotedArg(0) + ")"))      
      impl (atan) (codegen(g, "atan(" + quotedArg(0) + ")"))
      impl (atan2) (codegen(g, "atan2(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
      impl (pow) (codegen(g, "pow(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
      impl (max) (codegen(g, "fmax(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
      impl (min) (codegen(g, "fmin(" + quotedArg(0) + ", " + quotedArg(1) + ")"))      
    }    
  }  
  
  // tuples need some work, the tricks bordering on hacks in here are pretty unsatisfying
  def importTuples() = {        
    val A = tpePar("A")
    val B = tpePar("B")
    val e = tpePar("_")
        
    // the abstract name needs to be different than the Scala name, since we don't want to shadow it. 
    val Tuple2 = tpe("Tup2", (A,B))    
            
    data(Tuple2, ("_1", A), ("_2", B))
    compiler (Tuple2) ("tuple2_get1", A, Tuple2(A,e) :: A) implements getter(0, "_1")
    compiler (Tuple2) ("tuple2_get2", B, Tuple2(e,B) :: B) implements getter(0, "_2")
    
    fimplicit (Tuple2) ("t2", (A,B), Tuple2(A,B) :: CTuple2(A,B)) implements composite ${ ((tuple2_get1($0), tuple2_get2($0))) }      
    fimplicit (Tuple2) ("make_tuple2", (A,B), CTuple2(A,B) :: Tuple2(A,B)) implements allocates(Tuple2, ${$0._1}, ${$0._2})
    fimplicit (Tuple2) ("chain_make_tuple2_var", (A,B), CTuple2(MVar(A),B) :: Tuple2(A,B)) implements composite ${ make_tuple2(($0._1,$0._2)) }
    fimplicit (Tuple2) ("chain_make_tuple2_var", (A,B), CTuple2(A,MVar(B)) :: Tuple2(A,B)) implements composite ${ make_tuple2(($0._1,$0._2)) }
    fimplicit (Tuple2) ("chain_make_tuple2_var", (A,B), CTuple2(MVar(A),MVar(B)) :: Tuple2(A,B)) implements composite ${ make_tuple2(($0._1,$0._2)) }
    
    // val C = tpePar("C")
    // val Tuple3 = tpe("Tup3", (A,B,C))
    // val CTuple3 = tpe("Tuple3", (A,B,C), stage = now)
            
    // val D = tpePar("D")    
    // val Tuple4 = tpe("Tup4", (A,B,C,D))
    // val CTuple4 = tpe("Tuple4", (A,B,C,D), stage = now)            
  }
}
