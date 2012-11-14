package ppl.dsl.forge
package core

trait Definitions {
  this: Forge =>

  /**
   * String constants
   */
  val opIdentifierPrefix = "mn"
  val opArgPrefix = "__arg"
  val implicitOpArgPrefix = "__imp"
  
  /**
   * Built-in types
   */  
  
  lazy val MAny = tpe("Any")
  lazy val MInt = tpe("Int")
  lazy val MDouble = tpe("Double")
  lazy val MBoolean = tpe("Boolean")
  lazy val MString = tpe("String")
  lazy val MUnit = tpe("Unit")
  lazy val byName = tpe("Thunk")
  def MThunk(ret: Rep[DSLType]) = ftpe(List(byName), ret)
  def MFunction(args: List[Rep[DSLType]], ret: Rep[DSLType]) = ftpe(args,ret)  
  // TODO: need to handle concrete type instances (e.g. Var[Int]) properly
  // tpeArg/tpeArgs should get renamed to tpePar/tpePars. Should be able
  // to instantiate a tpe with n tpePars with n tpeArgs.
  object MVar {
    def apply(tpeArg: Rep[TypeArg]) = tpe("Var", List(tpeArg)) 
    // TODO: quote is only available inside code generators..
    // def apply(tp: Rep[DSLType]) = tpe("Var" + quote(tp)) // a concrete instance
  }
  object MArray {
    def apply(tpeArg: Rep[TypeArg]) = tpe("Array", List(tpeArg)) 
    // def apply(tp: Rep[DSLType]) = tpe("Array" + quote(tp)) 
  }
  
  lazy val MSourceContext = tpe("SourceContext")
  
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
   * Delite op types
   */
  object codegenerated extends OpType
  
  abstract class DeliteOpType extends OpType
  
  case class Zip(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndices: (Int,Int), func: String) extends DeliteOpType
  object zip {
    // def apply[T](x: (T,T) => T)
    def apply(tpePars: (Rep[DSLType],Rep[DSLType],Rep[DSLType],Rep[DSLType]), argIndices: (Int,Int), x: String) = Zip(tpePars, argIndices, x)
  }
    
                         
}


trait DefinitionsExp extends Definitions with DerivativeTypes {
  this: ForgeExp =>
  
}