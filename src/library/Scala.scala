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
    val not = infix (Prim) ("unary_!", Nil, MBoolean :: MBoolean) 
    val or = infix (Prim) ("||", Nil, (MBoolean, MBoolean) :: MBoolean) 
    val and = infix (Prim) ("&&", Nil, (MBoolean, MBoolean) :: MBoolean)

    for (g <- List($cala, cuda, cpp)) {
      impl (not) (codegen(g, "!" + quotedArg(0))) 
      impl (or) (codegen(g, quotedArg(0) + " || " + quotedArg(1)))
      impl (and) (codegen(g, quotedArg(0) + " && " + quotedArg(1)))
    }

    lift (Prim) (MInt)
    lift (Prim) (MFloat)
    lift (Prim) (MDouble)

    val toInt = infix (Prim) ("toInt", T withBound TNumeric, T :: MInt)
    val toFloat = infix (Prim) ("toFloat", T withBound TNumeric, T :: MFloat)
    val toDouble = infix (Prim) ("toDouble", T withBound TNumeric, T :: MDouble)
    
    impl (toInt) (codegen($cala, ${ $0.toInt }))
    impl (toFloat) (codegen($cala, ${ $0.toFloat }))
    impl (toDouble) (codegen($cala, ${ $0.toDouble }))

    for (g <- List(cuda, cpp)) {
      impl (toInt) (codegen(g, ${ (int) $0 }))
      impl (toFloat) (codegen(g, ${ (float) $0 }))
      impl (toDouble) (codegen(g, ${ (double) $0 }))
    }

    fimplicit (Prim) ("repInt2ToRepDouble", Nil, MInt :: MDouble) implements composite ${ $0.toDouble }
    fimplicit (Prim) ("repInt2ToRepFloat", Nil, MInt :: MFloat) implements composite ${ $0.toFloat }
    fimplicit (Prim) ("repFloat2ToRepDouble", Nil, MFloat :: MDouble) implements composite ${ $0.toDouble }

    // specialized versions for primitives
    // the forge_ prefix is to avoid conflicting with LMS primitive ops
    val int_plus = direct (Prim) ("forge_int_plus", Nil, (MInt,MInt) :: MInt)
    val int_minus = direct (Prim) ("forge_int_minus", Nil, (MInt,MInt) :: MInt)
    val int_times = direct (Prim) ("forge_int_times", Nil, (MInt,MInt) :: MInt)
    val int_divide = direct (Prim) ("forge_int_divide", Nil, (MInt,MInt) :: MInt)
    val int_shift_left = direct (Prim) ("forge_int_shift_left", Nil, (MInt,MInt) :: MInt) 
    val int_mod = infix (Prim) ("%", Nil, (MInt,MInt) :: MInt) 
    
    val float_plus = direct (Prim) ("forge_float_plus", Nil, (MFloat,MFloat) :: MFloat) 
    val float_minus = direct (Prim) ("forge_float_minus", Nil, (MFloat,MFloat) :: MFloat) 
    val float_times = direct (Prim) ("forge_float_times", Nil, (MFloat,MFloat) :: MFloat) 
    val float_divide = direct (Prim) ("forge_float_divide", Nil, (MFloat,MFloat) :: MFloat)
    
    val double_plus = direct (Prim) ("forge_double_plus", Nil, (MDouble,MDouble) :: MDouble)
    val double_minus = direct (Prim) ("forge_double_minus", Nil, (MDouble,MDouble) :: MDouble)
    val double_times = direct (Prim) ("forge_double_times", Nil, (MDouble,MDouble) :: MDouble)
    val double_divide = direct (Prim) ("forge_double_divide", Nil, (MDouble,MDouble) :: MDouble)

    for (g <- List($cala, cuda, cpp)) {
      impl (int_plus) (codegen(g, ${$0 + $1}))
      impl (int_minus) (codegen(g, ${$0 - $1}))
      impl (int_times) (codegen(g, ${$0 * $1}))
      impl (int_divide) (codegen(g, ${$0 / $1}))
      impl (int_shift_left) (codegen(g, ${$0 << $1}))
      impl (int_mod) (codegen(g, ${$0 % $1}))

      impl (float_plus) (codegen(g, ${$0 + $1}))
      impl (float_minus) (codegen(g, ${$0 - $1}))
      impl (float_times) (codegen(g, ${$0 * $1}))
      impl (float_divide) (codegen(g, ${$0 / $1}))

      impl (double_plus) (codegen(g, ${$0 + $1}))
      impl (double_minus) (codegen(g, ${$0 - $1}))
      impl (double_times) (codegen(g, ${$0 * $1}))
      impl (double_divide) (codegen(g, ${$0 / $1}))  
    }

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

    infix (Prim) ("unary_-", Nil, MInt :: MInt) implements redirect ${ unit(-1)*$0 }
    infix (Prim) ("unary_-", Nil, MFloat :: MFloat) implements redirect ${ unit(-1f)*$0 }
    infix (Prim) ("unary_-", Nil, MDouble :: MDouble) implements redirect ${ unit(-1)*$0 }
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

    val exit = direct (Misc) ("exit", Nil, MInt :: MUnit, effect = simple)
    impl (exit) (codegen($cala, ${sys.exit($0)}))
    
    val print = direct (Misc) ("print", Nil, MAny :: MUnit, effect = simple)
    impl (print) (codegen($cala, ${print($0)}))

    val fatal = direct (Misc) ("fatal", Nil, MString :: MNothing, effect = simple)
    impl (fatal) (codegen($cala, ${throw new Exception($0)}))

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
    
    for (g <- List(cuda, cpp)) {
      impl (exit) (codegen(g, ${exit($0)}))
      impl (print) (codegen(g, ${std::cout << $0}))
      impl (fatal) (codegen(g, ${assert(0)}))
      impl (println) (codegen(g, ${std::cout << $0 << std::endl}))
      impl (println2) (codegen(g, ${std::cout << std::endl}))
      impl (immutable) (codegen(g, ${$0}))
    }
  }

  def importCasts() = {
    val Cast = grp("Cast")
    val A = tpePar("A")
    val B = tpePar("B")

    // these don't work as infix_ methods
    noInfixList :::= List("AsInstanceOf", "IsInstanceOf")

    val asinstance = infix (Cast) ("AsInstanceOf", (A,B), A :: B) 
    impl (asinstance) (codegen($cala, ${ $0.asInstanceOf[$t[B]] }))
    impl (asinstance) (codegen(cuda, ${ ($t[B])$0 }))
    impl (asinstance) (codegen(cpp, ${ ($t[B])$0 }))

    val isinstance = infix (Cast) ("IsInstanceOf", (A,B), A :: MBoolean) 
    impl (isinstance) (codegen($cala, ${ $0.isInstanceOf[$t[B]] }))
    // todo: how to implement isinstance for clike targets?
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

    val eq = direct (Ord) ("__equal", (A,B), (A,B) :: MBoolean) 
    impl (eq) (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    impl (eq) (codegen(cuda, quotedArg(0) + " == " + quotedArg(1)))
    impl (eq) (codegen(cpp, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (A,B), (MVar(A),B) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (A,B), (A,MVar(B)) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (A,B), (MVar(A),MVar(B)) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (A,BC), (A,BC) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (A,BC), (MVar(A),BC) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (AC,B), (AC,B) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (AC,B), (AC,MVar(B)) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))

    val neq = infix (Ord) ("!=", (A,B), (A,B) :: MBoolean)
    impl (neq) (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
    impl (neq) (codegen(cuda, quotedArg(0) + " != " + quotedArg(1)))
    impl (neq) (codegen(cpp, quotedArg(0) + " != " + quotedArg(1)))
    infix (Ord) ("!=", (A,B), (MVar(A),B) :: MBoolean) implements (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
    infix (Ord) ("!=", (A,B), (A,MVar(B)) :: MBoolean) implements (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
    infix (Ord) ("!=", (A,B), (MVar(A),MVar(B)) :: MBoolean) implements (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
    infix (Ord) ("!=", (A,BC), (A,BC) :: MBoolean) implements (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
    infix (Ord) ("!=", (A,BC), (MVar(A),BC) :: MBoolean) implements (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
    infix (Ord) ("!=", (AC,B), (AC,B) :: MBoolean) implements (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
    infix (Ord) ("!=", (AC,B), (AC,MVar(B)) :: MBoolean) implements (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))

    val lt = infix (Ord) ("<", List(A withBound TOrdering), List(A,A) :: MBoolean)
    val lte = infix (Ord) ("<=", List(A withBound TOrdering), List(A,A) :: MBoolean)
    val gt = infix (Ord) (">", List(A withBound TOrdering), List(A,A) :: MBoolean)
    val gte = infix (Ord) (">=", List(A withBound TOrdering), List(A,A) :: MBoolean)

    for (g <- List($cala, cuda, cpp)) {
      impl (lt) (codegen(g, quotedArg(0) + " < " + quotedArg(1)))
      impl (lte) (codegen(g, quotedArg(0) + " <= " + quotedArg(1)))
      impl (gt) (codegen(g, quotedArg(0) + " > " + quotedArg(1)))
      impl (gte) (codegen(g, quotedArg(0) + " >= " + quotedArg(1)))
    }
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
    infix (Str) ("startsWith", Nil, (MString,MString) :: MBoolean) implements codegen($cala, ${ $0.startsWith($1) })

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

    // constants
    direct (Math) ("INF", Nil, Nil :: MDouble) implements codegen($cala, "Double.PositiveInfinity")
    direct (Math) ("nINF", Nil, Nil :: MDouble) implements codegen($cala, "Double.NegativeInfinity")
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
      impl (log10) (codegen(g, "log10(" + quotedArg(0) + ")"))
      impl (sqrt) (codegen(g, "sqrt(" + quotedArg(0) + ")"))
      impl (ceil) (codegen(g, "ceil(" + quotedArg(0) + ")"))
      impl (floor) (codegen(g, "floor(" + quotedArg(0) + ")"))
      impl (sin) (codegen(g, "sin(" + quotedArg(0) + ")"))
      impl (sinh) (codegen(g, "sinh(" + quotedArg(0) + ")"))
      impl (asin) (codegen(g, "asin(" + quotedArg(0) + ")"))
      impl (cos) (codegen(g, "cos(" + quotedArg(0) + ")"))
      impl (cosh) (codegen(g, "cosh(" + quotedArg(0) + ")"))
      impl (acos) (codegen(g, "acos(" + quotedArg(0) + ")"))
      impl (tan) (codegen(g, "tan(" + quotedArg(0) + ")"))
      impl (tanh) (codegen(g, "tan(" + quotedArg(0) + ")"))
      impl (atan) (codegen(g, "atan(" + quotedArg(0) + ")"))
      impl (atan2) (codegen(g, "atan2(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
      impl (pow) (codegen(g, "pow(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
      impl (max) (codegen(g, "fmax(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
      impl (min) (codegen(g, "fmin(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
    }
  }

  def importTuples() = {
    val e = tpePar("_")

    // only go to 10 for the sake of compile-time overhead (for now)
    for (arity <- (2 until maxTuples)) {
      val pars = (0 until arity).map(i => tpePar(('A'.toInt+i).toChar.toString)).toList
      val elems = (0 until arity).map(i => "_" + (i+1))

      // the abstract name needs to be different than the Scala name, since we don't want to shadow it.
      val TT = tpe("Tup" + arity, pars)
      data(TT, elems.zip(pars): _*)

      for (i <- 0 until arity) {
        val concrete = pars.zipWithIndex.map(t => if (t._2 == i) t._1 else e)
        infix (TT) ("_"+(i+1), pars(i), TT(concrete: _*) :: pars(i)) implements getter(0, elems(i))
      }

      val CT = tpe("Tuple"+arity, pars, stage = compile)
      val unpackTupleStr = "(" + elems.zipWithIndex.map(t => "tup"+arity+"__"+(t._2+1)+"(t)").mkString("(",",",")") + ")"
      val parStr = elems.map(e => "t."+e)

      direct (TT) ("unpack", pars, ("t",TT(pars: _*)) :: CT(pars: _*)) implements composite ${ \$unpackTupleStr }
      direct (TT) ("pack", pars, ("t",CT(pars: _*)) :: TT(pars: _*)) implements composite ("internal_pack" + arity + "(" + parStr.mkString(",") + ")")

      // internal_pack is necessary so that we don't store a stage = now type (CT) in a Delite IR node, which expects Reps.
      val argStr = (0 until pars.length).map(i => unit(quotedArg(i)))
      compiler (TT) ("internal_pack" + arity, pars, (pars :: TT(pars: _*))) implements allocates(TT, argStr: _*)

      val makeTupleStrStr = "\"(\"+" + (1 to arity).map(i => "t._"+i).mkString("+\",\"+") + "+\")\""
      infix (TT) ("toString", pars, ("t",TT(pars: _*)) :: MString) implements composite ${ \$makeTupleStrStr }
    }

    // add pack for Var combinations inside Tuple2s. We don't do this for all of them,
    // since the number of T,Var[T],Rep[T] combinations is exponential in the size of the tuple
    val Tuple2 = lookupTpe("Tup2")
    val A = tpePar("A")
    val B = tpePar("B")
    direct (Tuple2) ("pack", (A,B), CTuple2(MVar(A),B) :: Tuple2(A,B)) implements redirect ${ tup2_pack(($0._1,$0._2)) }
    direct (Tuple2) ("pack", (A,B), CTuple2(A,MVar(B)) :: Tuple2(A,B)) implements redirect ${ tup2_pack(($0._1,$0._2)) }
    direct (Tuple2) ("pack", (A,B), CTuple2(MVar(A),MVar(B)) :: Tuple2(A,B)) implements redirect ${ tup2_pack(($0._1,$0._2)) }
  }
}
