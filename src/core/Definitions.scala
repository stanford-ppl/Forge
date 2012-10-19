package ppl.dsl.meta
package core

trait Definitions extends DerivativeTypes {
  this: MetaDSL =>

  /**
   * String constants
   */
  val opIdentifierPrefix = "mn"
  val opArgPrefix = "__arg"
  val implicitOpArgPrefix = "__imp"
  
  /**
   * primitive types
   */  
  lazy val MInt = tpe("Int")
  lazy val MDouble = tpe("Double")
  lazy val MBoolean = tpe("Boolean")
  lazy val MUnit = tpe("Unit")
  def MArray(tpeArgs: List[Rep[TypeArg]]) = tpe("Array", tpeArgs)
  lazy val MSourceContext = tpe("SourceContext")
    
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
  
  /**
   * Method syntax types
   */
  object static extends MethodType
  object infix extends MethodType
  
  // blacklist for op names that cannot be expressed with infix methods
  val noInfixList = List("apply", "update") 
  
  /**
   * Effect types
   */  
  object pure extends EffectType
  object mutable extends EffectType  
  case class write(args: Rep[DSLType]*) extends EffectType
  
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
    
  /**
   * LMS ops
   */    
  object arrayBufferOps extends LMSOps { def name = "ArrayBufferOps" }
  object arrayOps extends LMSOps { 
    def name = "ArrayOps"
    override def opt = "ArrayOpsExpOpt"
  }
  object booleanOps extends LMSOps { 
    def name = "BooleanOps"
    override def lift = Some("LiftBoolean")
  }
  object castingOps extends LMSOps { def name = "CastingOps" }
  object equalOps extends LMSOps { 
    def name = "Equal"
    override def opt = "EqualExpOpt"
    override def lift = Some("LiftEquals")
  }
  object exceptionOps extends LMSOps {
    def name = "ExceptionOps"
    override def targets = List($cala)
  }
  object fractionalOps extends LMSOps { def name = "FractionalOps" }
  // object functionBlockOps extends LMSOps {}
  object functionOps extends LMSOps { def name = "Functions" }
  object tupledFunctionOps extends LMSOps {
    def name = "TupledFunctions"
    override def targets = List($cala)
  }  
  object hashMapOps extends LMSOps { def name = "HashMapOps" }
  object ifThenElseOps extends LMSOps {
    def name = "IfThenElse"
    override def opt = "IfThenElseExpOpt"
  }
  object implicitOps extends LMSOps { def name = "ImplicitOps" }
  object ioOps extends LMSOps { def name = "IOOps" }
  object iterableOps extends LMSOps { def name = "IterableOps" }
  object listOps extends LMSOps { def name = "ListOps" }
  object mathOps extends LMSOps { def name = "MathOps" }
  object miscOps extends LMSOps { def name = "MiscOps" }
  object numericOps extends LMSOps {
    def name = "NumericOps"
    override def opt = "NumericOpsExpOpt"
    override def lift = Some("LiftNumeric")
  }
  object objectOps extends LMSOps { def name = "ObjectOps" }
  object orderingOps extends LMSOps { def name = "OrderingOps" }
  object primitiveOps extends LMSOps {
    def name = "PrimitiveOps"
    override def lift = Some("LiftPrimitives")
  }
  object rangeOps extends LMSOps { def name = "RangeOps" }
  object seqOps extends LMSOps { def name = "SeqOps" }
  object setOps extends LMSOps { def name = "SetOps" }
  object stringOps extends LMSOps { 
    def name = "StringOps" 
    override def lift = Some("LiftString")
  }
  // object structOps extends LMSOps {}
  object synchronizedArrayBufferOps extends LMSOps { def name = "SynchronizedArrayBufferOps" }
  object tupleOps extends LMSOps {
    def name = "TupleOps"
    override def targets = List($cala)
  }
  object variableOps extends LMSOps {
    def name = "Variables"
    override def opt = "VariablesExpOpt"
    override def lift = Some("LiftVariables")
  }
  object whileOps extends LMSOps { def name = "While" }
  // object allOps extends LMSOps {
  //   def name = "ScalaOpsPkg"
  //   def codegen = g => g.name+"CodeGenPkg"
  // }
  val allLMSOps = List(arrayBufferOps, arrayOps, booleanOps, castingOps, equalOps, exceptionOps, fractionalOps, /*functionBlockOps,*/
                       functionOps, tupledFunctionOps, hashMapOps, ifThenElseOps, ioOps, iterableOps, listOps, mathOps, miscOps, 
                       numericOps, objectOps, orderingOps, primitiveOps, rangeOps, seqOps, setOps, stringOps, /*structOps,*/
                       synchronizedArrayBufferOps, tupleOps, variableOps, whileOps)
                         
}