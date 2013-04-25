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
  // concrete types (M stands for "Meta", C stands for "Current").. these aren't exactly consistent
  lazy val MAny = tpe("Any")
  lazy val CAny = tpe("Any", stage = now)
  lazy val MInt = tpe("Int")
  lazy val CInt = tpe("Int", stage = now)
  lazy val MDouble = tpe("Double")
  lazy val CDouble = tpe("Double", stage = now)
  lazy val MBoolean = tpe("Boolean")
  lazy val CBoolean = tpe("Boolean", stage = now)
  lazy val MString = tpe("String")
  lazy val CString = tpe("String", stage = now)   
  lazy val MUnit = tpe("Unit")
  lazy val CUnit = tpe("Unit", stage = now)
  lazy val byName = tpe("Thunk")
  def MThunk(ret: Rep[DSLType], freq: Frequency = normal) = ftpe(List(forge_arg("", byName, None)),ret,freq) // TODO
  def MFunction(args: List[Rep[DSLArg]], ret: Rep[DSLType], freq: Frequency = normal) = ftpe(args,ret,freq)
  lazy val MSourceContext = tpe("SourceContext")
  
  // generic types
  // should these return a different Forge type (e.g. Rep[TypeConstructor] or Rep[GenericType]) than concrete types?
  def MVar(tpePar: Rep[TypePar]) = tpe("Var", List(tpePar)) 
  def MArray(tpePar: Rep[TypePar]) = tpe("ForgeArray", List(tpePar))     
  
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
  object cpp extends CodeGenerator { def name = "C" }  
  
  val generators = List($cala, cuda, opencl, cpp)
  
  /**
   * Type classes
   * DSLs can extend these by adding their own
   */
  object TManifest extends TypeClass {
    def name = "Manifest"
    def prefix = "_m"
    def wrapper = Some("mtype")
  }
  object TNumeric extends TypeClass {
    def name = "Numeric"
    def prefix = "_num"
    def wrapper = Some("ntype") 
  }
  object TOrdering extends TypeClass {
    def name = "Ordering"
    def prefix = "_ord"
    def wrapper = Some("otype")
  }

  
  /**
   * Method syntax types
   */
  object static extends MethodType
  object infix extends MethodType
  object direct extends MethodType
  object compiler extends MethodType
  
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
}
