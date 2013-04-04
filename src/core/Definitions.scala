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
  val qu = "__quote"
    
  /**
   * Built-in types
   */    
  // concrete types (M stands for "Meta")
  lazy val MAny = tpe("Any")
  lazy val MInt = tpe("Int")
  lazy val MDouble = tpe("Double")
  lazy val MBoolean = tpe("Boolean")
  lazy val MString = tpe("String")
  lazy val MUnit = tpe("Unit")
  lazy val byName = tpe("Thunk")
  // gibbons4 - hacky names for thunk and function
  var thunkCount = 0
  def MThunk(ret: Rep[DSLType], freq: Frequency = normal) = {
    thunkCount += 1
    ftpe(List(("thunk"+thunkCount, byName)),ret,freq)
  }
  def MFunction(args: List[Rep[DSLType]], ret: Rep[DSLType]) = {
    ftpe(args.zipWithIndex.map(x => (quotedArg(x._2), x._1)),ret,normal)
  }
/*
  def MFunction(args: List[Rep[DSLType]], ret: Rep[DSLType], freq: Frequency) = {
    ftpe(args.zipWithIndex.map(x => (quotedArg(x._2), x._1)),ret,freq)
  }
*/
  def MFunction(args: List[(String, Rep[DSLType])], ret: Rep[DSLType], freq: Frequency = normal) = ftpe(args,ret,freq)
  lazy val MSourceContext = tpe("SourceContext")
  
  // generic types
  // should these return a different Forge type (e.g. Rep[TypeConstructor] or Rep[GenericType]) than concrete types?
  def GVar(tpePar: Rep[TypePar]) = tpe("Var", List(tpePar)) 
  def GArray(tpePar: Rep[TypePar]) = tpe("Array", List(tpePar))     
  
  /**
   * DSLType placeholders
   */
  def varArgs(tpeArg: Rep[DSLType]): Rep[DSLType]  
  
  /**
   * stage tags - only 2 stages
   */
  object future extends StageTag
  object now extends StageTag
  
  /**
   * code generators
   */  
  object $cala extends CodeGenerator { def name = "Scala" }  // odd things happen if you try to re-use the existing object name 'scala'
  object cuda extends CodeGenerator { def name = "Cuda" }  
  object opencl extends CodeGenerator { def name = "OpenCL" }  
  object c extends CodeGenerator { def name = "C" }  
  
  val generators = List($cala, cuda, opencl, c)
  
  /**
   * Type classes
   * DSLs can extend these by adding their own
   */
  object TManifest extends TypeClass {
    def name = "Manifest"
    def prefix = "_m"
  }
  object TNumeric extends TypeClass {
    def name = "Numeric"
    def prefix = "_num"
  }
  object TOrdering extends TypeClass {
    def name = "Ordering"
    def prefix = "_ord"
  }

  
  /**
   * Method syntax types
   */
  object static extends MethodType
  object infix extends MethodType
  object direct extends MethodType
  
  // blacklist for op names that cannot be expressed with infix methods
  val noInfixList = List("apply", "update") 
  
  /**
   * Effect types
   */  
  object pure extends EffectType
  object mutable extends EffectType  
  object simple extends EffectType
  case class write(args: Int*) extends EffectType
  
  /**
   * Alias hints
   */
  object nohint extends AliasHint  
  case class AliasInfo(aliases: Option[List[Int]], contains: Option[List[Int]], extracts: Option[List[Int]], copies: Option[List[Int]]) extends AliasHint
  case class AliasCopies(args: List[Int]) extends AliasHint
  
  // generic alias hint constructor
  def info(aliases: Option[List[Int]], contains: Option[List[Int]], extracts: Option[List[Int]], copies: Option[List[Int]]) = AliasInfo(aliases, contains, extracts, copies)
  
  // convenience methods for constructing common alias hints
  def copies(arg: Int): AliasHint = copies(List(arg))
  def copies(args: List[Int]) = AliasCopies(args)
  // others? aliasesSome(..)?
  
  /**
   * Frequency annotations for code motion
   */
  object normal extends Frequency
  object hot extends Frequency
  object cold extends Frequency
   
  /**
   * Delite op types
   */
  object codegenerated extends OpType
  
  abstract class DeliteOpType extends OpType

  /**
   * SingleTask
   * 
   * @param retTpe    R, the return type of the function
   * @param func      string representation of the function ( => R)
   */
  def forge_single(retTpe: Rep[DSLType], func: Rep[String]): DeliteOpType
  object single {
    def apply(retTpe: Rep[DSLType], func: Rep[String]) = forge_single(retTpe, func)
  }
  
  /**
   * Map
   * 
   * @param tpePars   [A,R,C[R]]
   * @param argIndex  index of op argument that correspond to map argument in (collection to be mapped)
   * @param func      string representation of a map function A => R
   */
   def forge_map(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String]): DeliteOpType
   object map {
     def apply(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), mapArgIndex: Int, func: Rep[String]) = forge_map(tpePars, mapArgIndex, func)
   }  
   
  /**
   * ZipWith
   * 
   * @param tpePars       [A,B,R,C[R]]
   * @param argIndices    index of op arguments that correspond to zip arguments inA, inB (first and second collection respectively)
   * @param func          string representation of a zip function (A, B) => R
   */
  def forge_zip(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndices: (Int,Int), func: Rep[String]): DeliteOpType
  object zip {
    // def apply[T](x: (T,T) => T)
    def apply(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType],Rep[DSLType]), zipArgIndices: (Int,Int), func: Rep[String]) = forge_zip(tpePars, zipArgIndices, func)
  }
  
  /**
   * Reduce
   * 
   * @param tpePars   [A,C[A]]
   * @param func      string representation of a reduce function (A, A) => A
   */
   def forge_reduce(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, zero: Rep[DSLOp], func: Rep[String]): DeliteOpType
   object reduce {
     def apply(tpePars: (Rep[DSLType],Rep[DSLType]), redArgIndex: Int, zero: Rep[DSLOp], func: Rep[String]) = forge_reduce(tpePars, redArgIndex, zero, func)
   }
    
  /**
   * Filter
   * 
   * TODO: This is broken! We are missing DeliteCollection buffer functionality that is required for Filters.
   * 
   * @param tpePars   [A,R,C[R]]
   * @param argIndex  index of op argument that correspond to filter argument in (collection to be filtered)
   * @param cond      string representation of predicate function A => Boolean
   * @param func      string representation of a map function A => R
   */
   def forge_filter(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, cond: Rep[String], func: Rep[String]): DeliteOpType
   object filter {
     def apply(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), filterArgIndex: Int, cond: Rep[String], func: Rep[String]) = forge_filter(tpePars, filterArgIndex, cond, func)
   }  
   
  
  /**
   * Foreach
   * 
   * @param tpePars   [A, C[A]]
   * @param argIndex  index of op argument that correspond to foreach argument in 
   * @param func      string representation of a foreach function A => Unit
   */
   def forge_foreach(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String]): DeliteOpType
   object foreach {
     def apply(tpePars: (Rep[DSLType],Rep[DSLType]), foreachArgIndex: Int, func: Rep[String]) = forge_foreach(tpePars, foreachArgIndex, func)
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
   * Delite ops
   */
  case class SingleTask(retTpe: Rep[DSLType], func: Rep[String]) extends DeliteOpType
  def forge_single(retTpe: Rep[DSLType], func: Rep[String]) = SingleTask(retTpe, func)
  
  case class Map(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String]) extends DeliteOpType  
  def forge_map(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String]) = Map(tpePars, argIndex, func)
    
  case class Zip(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndices: (Int,Int), func: Rep[String]) extends DeliteOpType  
  def forge_zip(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndices: (Int,Int), func: Rep[String]) = Zip(tpePars, argIndices, func)
  
  case class Reduce(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, zero: Rep[DSLOp], func: Rep[String]) extends DeliteOpType
  def forge_reduce(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, zero: Rep[DSLOp], func: Rep[String]) = Reduce(tpePars, argIndex, zero, func)
  
  case class Filter(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, cond: Rep[String], func: Rep[String]) extends DeliteOpType  
  def forge_filter(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndex: Int, cond: Rep[String], func: Rep[String]) = Filter(tpePars, argIndex, cond, func)  
  
  case class Foreach(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String]) extends DeliteOpType  
  def forge_foreach(tpePars: (Rep[DSLType],Rep[DSLType]), argIndex: Int, func: Rep[String]) = Foreach(tpePars, argIndex, func)    
    
}
