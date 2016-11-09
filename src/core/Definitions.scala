package ppl.dsl.forge
package core

trait Definitions extends DerivativeTypes {
  this: Forge =>

  /**
   * String constants
   */
  val opIdentifierPrefix = "mn"
  val opArgPrefix = "__arg"
  val implicitOpArgPrefix = "__imp"
  val implicitCtxBoundArgPrefix = "__cb"
  val qu = "__quote"
  val symMarker = "__sym"

  /**
   * Other constants
   */
  val maxTuples = 10

  /**
   * Built-in types
   * M - stands for "Meta"    (a staged symbolic expression)
   * C - stands for "Current" (a staged constant)
   * S - stands for "Scala"   (always a constant, never staged)
   */

  // TODO: These aren't exactly consistent
  // FIXME: Need to resolve discrepancy between 'compile' and 'now'. Ideally everything should have 'compile' behavior, but it breaks
  //        string lifting in Scala.scala, for example.

  lazy val MAny = tpe("Any")
  lazy val CAny = tpe("Any", stage = now)
  lazy val SAny = tpe("Any", stage = compile)   // Scala Any (used during compilation)
  lazy val MInt = tpe("Int")
  lazy val CInt = tpe("Int", stage = now)
  lazy val SInt = tpe("Int", stage = compile)   // Scala integer (used as constant during compilation)
  lazy val MLong = tpe("Long")
  lazy val CLong = tpe("Long", stage = now)
  lazy val SLong = tpe("Long", stage = compile)
  lazy val MFloat = tpe("Float")
  lazy val CFloat = tpe("Float", stage = now)
  lazy val SFloat = tpe("Float", stage = compile)
  lazy val MDouble = tpe("Double")
  lazy val CDouble = tpe("Double", stage = now)
  lazy val SDouble = tpe("Double", stage = compile)
  lazy val MBoolean = tpe("Boolean")
  lazy val CBoolean = tpe("Boolean", stage = now)
  lazy val SBoolean = tpe("Boolean", stage = compile)
  lazy val MString = tpe("String")
  lazy val CString = tpe("String", stage = now)
  lazy val SString = tpe("String", stage = compile)
  lazy val MShort = tpe("Short")
  lazy val CShort = tpe("Short", stage = now)
  lazy val SShort = tpe("Short", stage = compile)
  lazy val MByte = tpe("Byte")
  lazy val CByte = tpe("Byte", stage = now)
  lazy val SByte = tpe("Byte", stage = compile)
  lazy val MChar = tpe("Char")
  lazy val CChar = tpe("Char", stage = now)
  lazy val SChar = tpe("Char", stage = compile)
  lazy val CTuple2 = tpe("Tuple2", (tpePar("A"),tpePar("B")), stage = compile)
  lazy val CTuple3 = tpe("Tuple3", (tpePar("A"),tpePar("B"),tpePar("C")), stage = compile)
  lazy val CTuple4 = tpe("Tuple4", (tpePar("A"),tpePar("B"),tpePar("C"),tpePar("D")), stage = compile)
  lazy val CTuple5 = tpe("Tuple5", (tpePar("A"),tpePar("B"),tpePar("C"),tpePar("D"),tpePar("E")), stage = compile)
  lazy val CTuple6 = tpe("Tuple6", List(tpePar("A"),tpePar("B"),tpePar("C"),tpePar("D"),tpePar("E"),tpePar("F")), stage = compile)
  lazy val MUnit = tpe("Unit")
  lazy val CUnit = tpe("Unit", stage = now)
  lazy val SUnit = tpe("Unit", stage = compile) // Useful?
  lazy val MNothing = tpe("Nothing")
  lazy val CNothing = tpe("Nothing", stage = now)
  lazy val SNothing = tpe("Nothing", stage = now)
  lazy val byName = tpe("Thunk")
  def MThunk(ret: Rep[DSLType], freq: Frequency = normal) = ftpe(List(forge_arg("", byName, None)),ret,freq) // TODO
  // unstaged (inlined) functions
  def MFunction(args: List[Rep[Any]], ret: Rep[DSLType], freq: Frequency = normal) = ftpe(args.zipWithIndex.map(anyToArg),ret,freq)
  // staged (reified) function of A => B (note that A can be a tuple for multiple arguments)
  lazy val MLambda = tpe("Function1", (tpePar("A"), tpePar("B")))
  lazy val MSourceContext = tpe("SourceContext", stage = compile)

  // generic types
  // should these return a different Forge type (e.g. Rep[TypeConstructor] or Rep[GenericType]) than concrete types?
  lazy val MVar = tpe("Var", tpePar("A"))
  lazy val CVar = tpe("Var", tpePar("A"), stage = compile)
  lazy val MArray = tpe("ForgeArray", tpePar("A"))
  lazy val MArrayBuffer = tpe("ForgeArrayBuffer", tpePar("A"))
  lazy val MHashMap = tpe("ForgeHashMap",(tpePar("K"),tpePar("V"))) // Forge HashMap (immutable)
  lazy val MInputStream = tpe("ForgeFileInputStream")
  lazy val MOutputStream = tpe("ForgeFileOutputStream")

  lazy val SList = tpe("List", tpePar("A"), stage = compile)
  lazy val SSeq = tpe("Seq", tpePar("A"), stage = compile)
  lazy val SManifest = tpe("Manifest", tpePar("A"), stage = compile)
  lazy val SOption = tpe("Option", tpePar("A"), stage = compile)

  /* whitelist for primitive types (i.e. we should not generate a Forge shadow) */
  var primitiveTpePrefix = scala.List("scala","java")

  // Both of these need to be appended to explicitly
  // Used when we don't want Forge to generate the class definition for this type
  // (Either because it's a Scala primitive or because the type is defined in the DSL's extern files)
  var primitiveStructs = List[Rep[DSLType]]() // Staged structs which are defined in extern
  var primitiveTypes = List[Rep[DSLType]]()   // Primitive types which are defined in extern

  /**
   * DSLType placeholders
   */
  def varArgs(tpeArg: Rep[DSLType]): Rep[DSLType]

  /**
   * Delite parallel strategies
   */
  // def parBuffer: Rep[DeliteParallelStrategy]
  // def parFlat: Rep[DeliteParallelStrategy]

  /**
   * stage tags - only 2 stages ('now' types get lifted into Reps, but 'compile' types never do)
   * TBD: is the distinction between 'now' and 'compile' useful? should we ever promote 'now's to Reps?
   *   'now's can theoretically enable us to simplify the back-end, since everything there can be Reps and therefore we need less combinations.
   *   this should be cleaned up when we clean up how overloaded front-end variants map to a single back-end implementation.
   */
  case object future extends StageTag { override def toString = "future" }
  case object now extends StageTag { override def toString = "now" }
  case object compile extends StageTag { override def toString = "compile" }

  /**
   * code generators
   */
  case object $cala extends CodeGenerator { def name = "Scala" }  // odd things happen if you try to re-use the existing object name 'scala'
  case object cuda extends CodeGenerator { def name = "Cuda" }
  case object opencl extends CodeGenerator { def name = "OpenCL" }
  case object cpp extends CodeGenerator { def name = "C" }
  case object restage extends CodeGenerator { def name = "Restage" }

  val generators = List($cala, cuda, opencl, cpp, restage)

  /**
   * Type classes
   * DSLs can extend these by adding their own
   */
  case object TManifest extends TypeClassSignature {
    def name = "Manifest"
    def prefix = "_m"
    def wrapper = Some("mtype")
  }
  case object TNumeric extends TypeClassSignature {
    def name = "Numeric"
    def prefix = "_num"
    def wrapper = Some("ntype")
  }
  case object TFractional extends TypeClassSignature {
    def name = "Fractional"
    def prefix = "_frac"
    def wrapper = Some("frtype")
  }
  case object TOrdering extends TypeClassSignature {
    def name = "Ordering"
    def prefix = "_ord"
    def wrapper = Some("otype")
  }
  object TReppable extends TypeClassSignature {
    def name = "Reppable"
    def prefix = "_rep"
    def wrapper = Some("rtype")
  }

  /**
   * Method syntax types
   */
  case object staticMethod extends MethodType
  case object infixMethod extends MethodType
  case object directMethod extends MethodType
  case object compilerMethod extends MethodType
  case object implicitMethod extends MethodType

  // blacklist for op names that cannot be expressed with infix methods
  // we also blacklist some operators for improved compilation performance or to avoid ambiguities in the REPL version
  // unfortunately, blacklisting arithmetic operators causes some erroneous type errors in application code for combinations that should work. however, using infix does appear to have a significant compilation cost
  // the two lists used below are used interchangeably depending on the configuration (normal or fast compile mode)
  var noInfixList = List("apply", "update", /*"+",*/ "-", "*", "/", "<", ">", "<=", ">=")  // string + doesn't resolve correctly in the compiler version using only implicits
  var mustInfixList = List("+"/*,"-","*","/"*/,"!=","length","toString")

  // blacklist for op names that need to be overridden in instance methods
  var overrideList = Set("toString", "hashCode", "equals")

  // blacklist for op names that need the SourceContext implicit parameter to be surpressed (usually because they construct an object with an apply method)
  var noSourceContextList = List[String]()

  /**
   * Effect types
   */
  case object pure extends EffectType
  case object mutable extends EffectType
  case object simple extends EffectType
  case class write(args: Int*) extends EffectType
  case object global extends EffectType

  /**
   * Alias hints
   */
  case object nohint extends AliasHint
  case class AliasInfo(aliases: Option[List[Int]], contains: Option[List[Int]], extracts: Option[List[Int]], copies: Option[List[Int]]) extends AliasHint
  case class AliasCopies(args: List[Int]) extends AliasHint

  // generic alias hint constructor
  def info(aliases: Option[List[Int]], contains: Option[List[Int]], extracts: Option[List[Int]], copies: Option[List[Int]]) = AliasInfo(aliases, contains, extracts, copies)

  // convenience methods for constructing common alias hints
  def aliases(arg: Int): AliasHint = aliases(List(arg))
  def aliases(args: List[Int]) =  info(Some(args),None,None,None)
  def contains(arg: Int): AliasHint = contains(List(arg))
  def contains(args: List[Int]) =  info(None,Some(args),None,None)
  def extracts(arg: Int): AliasHint = extracts(List(arg))
  def extracts(args: List[Int]) =  info(None,None,Some(args),None)
  def copies(arg: Int): AliasHint = copies(List(arg))
  def copies(args: List[Int]) = AliasCopies(args)  // TODO: why do we have a separate constructor for this?
  // others? aliasesSome(..)?

  /**
   * Frequency annotations for code motion
   */
  case object normal extends Frequency
  case object hot extends Frequency
  case object cold extends Frequency

  /**
   * Parallel collections
   */
  case class ParallelCollection(val tpeArg: Rep[DSLType], val alloc: Rep[DSLOp], val size: Rep[DSLOp], val apply: Rep[DSLOp], val update: Rep[DSLOp]) extends ForgeCollectionType
  case class ParallelCollectionBuffer(
    val tpeArg: Rep[DSLType], val alloc: Rep[DSLOp], val size: Rep[DSLOp], val apply: Rep[DSLOp], val update: Rep[DSLOp],
    /*val parallelization: Rep[DSLOp],*/ val setSize: Rep[DSLOp], val appendable: Rep[DSLOp], val append: Rep[DSLOp], val copy: Rep[DSLOp]
  ) extends ForgeCollectionType

  /**
   * Op types
   */
  abstract class DeliteOpType extends OpType

  /**
   * Codegenerated
   */
  def forge_codegen(generator: CodeGenerator, rule: Rep[String]): OpType
  object codegen {
    def apply(generator: CodeGenerator, rule: Rep[String]) = forge_codegen(generator,rule)
  }

  /**
   * Composite
   *
   * @param retTpe    R, the return type of the function
   * @param func      string representation of the function ( => R)
   */
  def forge_composite(func: Rep[String]): OpType
  object composite {
    def apply(func: Rep[String]) = forge_composite(func)
  }

  /**
   * Redirect
   *
   * Similar to composite, except no abstract methods will be created (the unrolling happens in the front-end).
   * As a consequence, no DSL implicit conversions are in scope for a redirect.
   *
   * @param retTpe    R, the return type of the function
   * @param func      string representation of the function ( => R)
   */
  def forge_redirect(func: Rep[String]): OpType
  object redirect {
    def apply(func: Rep[String]) = forge_redirect(func)
  }

  /**
   * Getters / setters for DSL structs
   */
  def forge_getter(structArgIndex: Int, field: String): OpType
  object getter {
    def apply(structArgIndex: Int, field: String) = forge_getter(structArgIndex,field)
  }

  def forge_setter(structArgIndex: Int, field: String, value: Rep[String]): OpType
  object setter {
    def apply(structArgIndex: Int, field: String, value: Rep[String]) = forge_setter(structArgIndex,field,value)
  }

  /**
   * Allocates
   *
   * @param data     The data struct that this op allocates
   * @param init     A sequence of tuples (fieldName, initialValue)
   */
  def forge_allocates(tpe: Rep[DSLType], init: Seq[Rep[String]]): DeliteOpType
  object allocates {
    def apply(tpe: Rep[DSLType], init: Rep[String]*) = forge_allocates(tpe, init)
  }


  /**
   * SingleTask
   *
   * @param retTpe    R, the return type of the function
   * @param func      string representation of the function ( => R)
   */
  def forge_single(func: Rep[String]): DeliteOpType
  object single {
    def apply(func: Rep[String]) = forge_single(func)
  }


  // --
  // TODO: we should adopt the Delite pattern of only generating the generalized loops to simplify this, and
  //       have all simpler (user-facing patterns) instantiate a generalized loop instance. Forge should thus
  //       only deal with the generators (collect, reduce, hashcollect, hashreduce, foreach). (Issue #37)
  // --


  /**
   * Map
   *
   * @param tpePars   [A,R]
   * @param argIndex  index of op argument that corresponds to input collection
   * @param func      string representation of a map function A => R
   */
   def forge_map(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String], numDynamicChunks: Any = None): DeliteOpType
   object map {
     def apply(tpePars: (Rep[DSLType],Rep[DSLType]), mapArgIndex: Int, func: Rep[String], numDynamicChunks: Any = None) = forge_map(tpePars, mapArgIndex, func, numDynamicChunks)
   }

  /**
   * ZipWith
   *
   * @param tpePars       [A,B,R]
   * @param argIndices    index of op arguments that correspond to zip arguments inA, inB (first and second collection respectively)
   * @param func          string representation of a zip function (A, B) => R
   */
  def forge_zip(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndices: (Int,Int), func: Rep[String], numDynamicChunks: Any = None): DeliteOpType
  object zip {
    // def apply[T](x: (T,T) => T)
    def apply(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), zipArgIndices: (Int,Int), func: Rep[String], numDynamicChunks: Any = None) = forge_zip(tpePars, zipArgIndices, func, numDynamicChunks)
  }

  /**
   * Reduce
   *
   * @param tpePars   A
   * @param argIndex  index of op argument that corresponds to input collection
   * @param zero      string representation of a function => A
   * @param func      string representation of a reduce function (A, A) => A
   */
   def forge_reduce(tpePar: Rep[DSLType], argIndex: Int, zero: Rep[String], func: Rep[String], numDynamicChunks: Any = None): DeliteOpType
   object reduce {
     def apply(tpePar: Rep[DSLType], redArgIndex: Int, zero: Rep[String], func: Rep[String], numDynamicChunks: Any = None) = forge_reduce(tpePar, redArgIndex, zero, func, numDynamicChunks)
   }

  /**
   * MapReduce
   *
   * Note: this pattern should be obsoleted by the introduction of abstract IR nodes and lowerings for composite ops.
   *
   * @param tpePars     [A,R]
   * @param argIndex    index of op argument that corresponds to input collection
   * @param map         string representation of a function A => R
   * @param zero        string representation of a function => R
   * @param reduce      string representation of a reduce function (R, R) => R
   * @param cond        optional string representation of a condition function A => Boolean
   */
   def forge_mapreduce(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, map: Rep[String], zero: Rep[String], reduce: Rep[String], cond: Option[Rep[String]], numDynamicChunks: Any = None): DeliteOpType
   object mapReduce {
     def apply(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, map: Rep[String], zero: Rep[String], reduce: Rep[String], cond: Option[Rep[String]] = None, numDynamicChunks: Any = None) = forge_mapreduce(tpePars, argIndex, map, zero, reduce, cond, numDynamicChunks)
   }

  /**
   * Filter
   *
   * @param tpePars   [A,R]
   * @param argIndex  index of op argument that correspond to input collection
   * @param cond      string representation of predicate function A => Boolean
   * @param func      string representation of a map function A => R
   */
   def forge_filter(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, cond: Rep[String], func: Rep[String], numDynamicChunks: Any = None): DeliteOpType
   object filter {
     def apply(tpePars: (Rep[DSLType],Rep[DSLType]), filterArgIndex: Int, cond: Rep[String], func: Rep[String], numDynamicChunks: Any = None) = forge_filter(tpePars, filterArgIndex, cond, func, numDynamicChunks)
   }

  /**
   * FlatMap
   *
   * @param tpePars   [A,R]
   * @param argIndex  index of op argument that correspond to input collection
   * @param func      string representation of a function A => Col[R]
   */
   def forge_flatmap(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String], numDynamicChunks: Any = None): DeliteOpType
   object flatMap {
     def apply(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String], numDynamicChunks: Any = None) = forge_flatmap(tpePars, argIndex, func, numDynamicChunks)
   }

  /**
   * GroupBy
   *
   * GroupBy returns a ForgeHashMap[K,ForgeArrayBuffer[V]]. Ideally, we would return a ForgeHashMap[K,CV],
   * but there are issues with rewrites in Delite when we used a DSL type instead of ArrayBuffer.
   *
   * @param tpePars   [A,K,V] //[A,K,V,CV]
   * @param argIndex  index of op argument that correspond to input collection
   * @param key       string representation of a function A => K
   * @param map       string representation of a function A => V
   * @param cond      optional string representation of predicate function A => Boolean
   */
   def forge_groupby(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, key: Rep[String], map: Rep[String], cond: Option[Rep[String]] = None, numDynamicChunks: Any = None): DeliteOpType
   object groupBy {
     def apply(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]/*,Rep[DSLType]*/), hashArgIndex: Int, key: Rep[String], map: Rep[String], cond: Option[Rep[String]] = None, numDynamicChunks: Any = None) = forge_groupby(/*tpePars*/(tpePars._1,tpePars._2,tpePars._3,MArrayBuffer(tpePars._3)), hashArgIndex, key, map, cond, numDynamicChunks)
   }

  /**
   * GroupByReduce
   *
   * GroupByReduce returns a ForgeHashMap[K,V].
   *
   * @param tpePars   [A,K,V]
   * @param argIndex  index of op argument that correspond to input collection
   * @param key       string representation of a function A => K
   * @param map       string representation of a function A => V
   * @param zero      string representation of a function => V
   * @param reduce    string representation of a reduce function (V, V) => V
   * @param cond      optional string representation of predicate function A => Boolean
   */
   def forge_groupby_reduce(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, key: Rep[String], map: Rep[String], zero: Rep[String], reduce: Rep[String], cond: Option[Rep[String]] = None, numDynamicChunks: Any = None): DeliteOpType
   object groupByReduce {
     def apply(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), hashArgIndex: Int, key: Rep[String], map: Rep[String], zero: Rep[String], reduce: Rep[String], cond: Option[Rep[String]] = None, numDynamicChunks: Any = None) = forge_groupby_reduce(tpePars, hashArgIndex, key, map, zero, reduce, cond, numDynamicChunks)
   }

  /**
   * Foreach
   *
   * @param tpePar    A
   * @param argIndex  index of op argument that correspond to foreach argument in
   * @param func      string representation of a foreach function A => Unit
   */
   def forge_foreach(tpePar: Rep[DSLType], argIndex: Int, func: Rep[String], numDynamicChunks: Any = None): DeliteOpType
   object foreach {
     def apply(tpePar: Rep[DSLType], foreachArgIndex: Int, func: Rep[String], numDynamicChunks: Any = None) = forge_foreach(tpePar, foreachArgIndex, func, numDynamicChunks)
   }
}


trait DefinitionsExp extends Definitions with DerivativeTypesExp {
  this: ForgeExp =>

  /**
   * DSLType placeholders
   */

   // T*
   case class VarArgs(tpeArg: Rep[DSLType]) extends Def[DSLType]
   def varArgs(tpeArg: Rep[DSLType]) = VarArgs(tpeArg)

  /**
   * Delite parallel strategies
   */
   // case class ParBuffer() extends Def[DeliteParallelStrategy]
   // case class ParFlat() extends Def[DeliteParallelStrategy]
   //
   // def parBuffer = ParBuffer()
   // def parFlat = ParFlat()

  case class CodeGenDecl(decl: Rep[String])
  case class CodeGen(decls: scala.collection.mutable.HashMap[CodeGenerator,CodeGenDecl]) extends OpType
  def forge_codegen(generator: CodeGenerator, rule: Rep[String]) = {
    CodeGen(scala.collection.mutable.HashMap(generator -> CodeGenDecl(rule)))
  }

  case class Getter(structArgIndex: Int, field: String) extends OpType
  def forge_getter(structArgIndex: Int, field: String) = Getter(structArgIndex,field)

  case class Setter(structArgIndex: Int, field: String, value: Rep[String]) extends OpType
  def forge_setter(structArgIndex: Int, field: String, value: Rep[String]) = Setter(structArgIndex,field,value)

  case class Composite(func: Rep[String]) extends OpType
  def forge_composite(func: Rep[String]) = Composite(func)

  case class Redirect(func: Rep[String]) extends OpType
  def forge_redirect(func: Rep[String]) = Redirect(func)

  /**
   * Delite ops
   */
  case class Allocates(tpe: Rep[DSLType], init: Seq[Rep[String]]) extends DeliteOpType
  def forge_allocates(tpe: Rep[DSLType], init: Seq[Rep[String]]) = Allocates(tpe,init)

  case class SingleTask(func: Rep[String]) extends DeliteOpType
  def forge_single(func: Rep[String]) = SingleTask(func)

  case class Map(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String], numDynamicChunks: Any) extends DeliteOpType
  def forge_map(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String], numDynamicChunks: Any) = Map(tpePars, argIndex, func, numDynamicChunks)

  case class Zip(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndices: (Int,Int), func: Rep[String], numDynamicChunks: Any) extends DeliteOpType
  def forge_zip(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndices: (Int,Int), func: Rep[String], numDynamicChunks: Any) = Zip(tpePars, argIndices, func, numDynamicChunks)

  case class Reduce(tpePar: Rep[DSLType], argIndex: Int, zero: Rep[String], func: Rep[String], numDynamicChunks: Any) extends DeliteOpType
  def forge_reduce(tpePar: Rep[DSLType], argIndex: Int, zero: Rep[String], func: Rep[String], numDynamicChunks: Any) = Reduce(tpePar, argIndex, zero, func, numDynamicChunks)

  case class MapReduce(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, map: Rep[String], zero: Rep[String], reduce: Rep[String], cond: Option[Rep[String]], numDynamicChunks: Any) extends DeliteOpType
  def forge_mapreduce(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, map: Rep[String], zero: Rep[String], reduce: Rep[String], cond: Option[Rep[String]], numDynamicChunks: Any) = MapReduce(tpePars, argIndex, map, zero, reduce, cond, numDynamicChunks)

  case class Filter(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, cond: Rep[String], func: Rep[String], numDynamicChunks: Any) extends DeliteOpType
  def forge_filter(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, cond: Rep[String], func: Rep[String], numDynamicChunks: Any) = Filter(tpePars, argIndex, cond, func, numDynamicChunks)

  case class FlatMap(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String], numDynamicChunks: Any) extends DeliteOpType
  def forge_flatmap(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String], numDynamicChunks: Any) = FlatMap(tpePars, argIndex, func, numDynamicChunks)

  case class GroupBy(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, key: Rep[String], map: Rep[String], cond: Option[Rep[String]], numDynamicChunks: Any) extends DeliteOpType
  def forge_groupby(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, key: Rep[String], map: Rep[String], cond: Option[Rep[String]], numDynamicChunks: Any) = GroupBy(tpePars, argIndex, key, map, cond, numDynamicChunks)

  case class GroupByReduce(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, key: Rep[String], map: Rep[String], zero: Rep[String], reduce: Rep[String], cond: Option[Rep[String]], numDynamicChunks: Any) extends DeliteOpType
  def forge_groupby_reduce(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, key: Rep[String], map: Rep[String], zero: Rep[String], reduce: Rep[String], cond: Option[Rep[String]], numDynamicChunks: Any) = GroupByReduce(tpePars, argIndex, key, map, zero, reduce, cond, numDynamicChunks)

  case class Foreach(tpePar: Rep[DSLType], argIndex: Int, func: Rep[String], numDynamicChunks: Any) extends DeliteOpType
  def forge_foreach(tpePar: Rep[DSLType], argIndex: Int, func: Rep[String], numDynamicChunks: Any) = Foreach(tpePar, argIndex, func, numDynamicChunks)
}
