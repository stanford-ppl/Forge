package ppl.dsl.forge
package library

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Primitive math seems to be a hot-spot in compile times for our DSLs due to the large number of variants
 * of +,-,*,/. For the same reason, it is also difficult to ensure that all combinations of lhs and rhs
 * will actually resolve correctly. In this trait, we experiment with different ways of generating these methods.
 */
trait PrimitiveMathGen {
  this: ForgeApplication =>

  /**
   * Helper methods
   */

  def returnValue(lhs: Rep[DSLType], rhs: Rep[DSLType]) = (canonicalName(lhs),canonicalName(rhs)) match {
    // Respect precision promotion rules
    case (l,_) if l.contains("Double") => MDouble
    case (_,r) if r.contains("Double") => MDouble
    case (l,_) if l.contains("Float") => MFloat
    case (_,r) if r.contains("Float") => MFloat
    case (l,_) if l.contains("Long") => MLong
    case (_,r) if r.contains("Long") => MLong
    case (l,_) if l.contains("Int") => MInt
    case (_,r) if r.contains("Int") => MInt
  }

  // Inject explicit conversions when necessary. These rely on methods defined in LMS.
  def formatArg(argName: String, arg: Rep[DSLType], ret: Rep[DSLType]) = {
    if (arg.name == "Var") {
      if (arg.tpeArgs.apply(0).name != ret.name) {
        "readVar(" + argName + ").to" + ret.name
      }
      else {
        "readVar(" + argName + ")"
      }
    }
    else if (arg.stage != future) {
      if (arg.name != ret.name) {
        "unit(" + argName + ".to" + ret.name + ")"
      }
      else {
        "unit(" + argName + ")"
      }
    }
    else if (arg.name != ret.name) {
      argName + ".to" + ret.name
    }
    else {
      argName
    }
  }


  /**
   * Generate all primitive combinations as type class implementations
   *
   * NOTE: The output from this generation is not complete. It still requires variance annotations to be added manually
   *       to BinOp, and either infix or implicit methods added to implement +,-,*,/ methods to forward to the type class.
   *
   * PERF: This variant was added to test compile time performance compared to infix enumeration. It showed a modest
   *       improvement (~20%), but also resulted in some compile errors when compiling OptiML applications. In other
   *       words, dispatch via enumerated type classes appears less robust than dispatch via enumerated infix or implicits.
   */

  // An example of what we currently do not auto-generate:

  /*
  implicit def intToBinOps(x: Int): PrimitiveBinOps[Rep[Int]] = new PrimitiveBinOps(unit(x))
  implicit def repIntToBinOps(x: Rep[Int]): PrimitiveBinOps[Rep[Int]] = new PrimitiveBinOps(x)
  implicit def varIntToBinOps(x: Var[Int]): PrimitiveBinOps[Rep[Int]] = new PrimitiveBinOps(readVar(x))

  implicit def longToBinOps(x: Long): PrimitiveBinOps[Rep[Long]] = new PrimitiveBinOps(unit(x))
  implicit def repLongToBinOps(x: Rep[Long]): PrimitiveBinOps[Rep[Long]] = new PrimitiveBinOps(x)
  implicit def varLongToBinOps(x: Var[Long]): PrimitiveBinOps[Rep[Long]] = new PrimitiveBinOps(readVar(x))

  implicit def floatToBinOps(x: Float): PrimitiveBinOps[Rep[Float]] = new PrimitiveBinOps(unit(x))
  implicit def repFloatToBinOps(x: Rep[Float]): PrimitiveBinOps[Rep[Float]] = new PrimitiveBinOps(x)
  implicit def varFloatToBinOps(x: Var[Float]): PrimitiveBinOps[Rep[Float]] = new PrimitiveBinOps(readVar(x))

  implicit def doubleToBinOps(x: Double): PrimitiveBinOps[Rep[Double]] = new PrimitiveBinOps(unit(x))
  implicit def repDoubleToBinOps(x: Rep[Double]): PrimitiveBinOps[Rep[Double]] = new PrimitiveBinOps(x)
  implicit def varDoubleToBinOps(x: Var[Double]): PrimitiveBinOps[Rep[Double]] = new PrimitiveBinOps(readVar(x))

  // Our universal operators
  class PrimitiveBinOps[A](lhs: A) {
    def +[B,R](rhs: B)(implicit op: BinOp[A,B,R], __pos: SourceContext): R = op.plus(lhs, rhs)
    def -[B,R](rhs: B)(implicit op: BinOp[A,B,R], __pos: SourceContext): R = op.minus(lhs, rhs)
    def *[B,R](rhs: B)(implicit op: BinOp[A,B,R], __pos: SourceContext): R = op.times(lhs, rhs)
    def /[B,R](rhs: B)(implicit op: BinOp[A,B,R], __pos: SourceContext): R = op.divide(lhs, rhs)
  }

  trait BinOp[-A,-B,+R] { ... }
  */

  def importPrimitiveMathTypeClasses() = {
    val A = tpePar("A", stage = compile)
    val B = tpePar("B", stage = compile)
    val R = tpePar("R", stage = compile)

    object TBinOp extends TypeClassSignature {
      def name = "BinOp"
      def prefix = "_bop"
      def wrapper = Some("boptype")
    }

    val BinOp = tpeClass("BinOp", TBinOp, (A,B,R))

    infix (BinOp) ("plus", Nil, (A,B) :: R)
    infix (BinOp) ("minus", Nil, (A,B) :: R)
    infix (BinOp) ("times", Nil, (A,B) :: R)
    infix (BinOp) ("divide", Nil, (A,B) :: R)

    // Force Forge to generate Rep[..] wrapper in type parameters. This is normally not the desired behavior, but in this case it is.
    def makeRawTpe(t: Rep[DSLType]) = {
      if (t.stage == future) ephemeralTpe("Rep[" + t.name + "]")
      else t
    }

    // We need versions of these types that will never be lifted to Rep, so that we can define our type class
    // implementations on the unlifted Scala type. This is a bit awkward. See comment in Definitions.scala.
    val SDouble = tpe("Double", stage = compile)
    val SFloat = tpe("Float", stage = compile)
    val SInt = tpe("Int", stage = compile)
    val SLong = tpe("Long", stage = compile)

    val primitives = List(SDouble,MDouble,CVar(SDouble),SFloat,MFloat,CVar(SFloat),SInt,MInt,CVar(SInt),SLong,MLong,CVar(SLong))

    // All primitive combinations
    val uniqueCombinations = (for (lhs <- primitives.filter(_.stage == future); rhs <- primitives) yield (lhs,rhs)).distinct
    for ((lhs,rhs) <- uniqueCombinations) {
      val ret = returnValue(lhs,rhs)
      val name = ("BinOp"+canonicalName(lhs)+canonicalName(rhs))
      val thisBinOp = tpeClassInst(name, Nil, BinOp(makeRawTpe(lhs),makeRawTpe(rhs),makeRawTpe(ret)))
      val (leftArg, rightArg) = (formatArg("lhs", lhs, ret), formatArg("rhs", rhs, ret))
      infix (thisBinOp) ("plus", Nil, ((("lhs",lhs),("rhs",rhs)) :: ret)) implements composite ${ forge_\${ret.name.toLowerCase}_plus(\$leftArg, \$rightArg) }
      infix (thisBinOp) ("minus", Nil, ((("lhs",lhs),("rhs",rhs)) :: ret)) implements composite ${ forge_\${ret.name.toLowerCase}_minus(\$leftArg, \$rightArg) }
      infix (thisBinOp) ("times", Nil, ((("lhs",lhs),("rhs",rhs)) :: ret)) implements composite ${ forge_\${ret.name.toLowerCase}_times(\$leftArg, \$rightArg) }
      infix (thisBinOp) ("divide", Nil, ((("lhs",lhs),("rhs",rhs)) :: ret)) implements composite ${ forge_\${ret.name.toLowerCase}_divide(\$leftArg, \$rightArg) }
    }
  }


  /**
   * Generate all primitive combinations as infix (or implicit) methods. Whether these generate as infix or implicits depends
   * on the contents of the 'noInfixList' and 'mustInfixList' lists.
   */
  def importPrimitiveMathInfix(Prim: Rep[DSLGroup]) {    
    val primitives = List(CDouble,MDouble,MVar(MDouble),CFloat,MFloat,MVar(MFloat),CInt,MInt,MVar(MInt),CLong,MLong,MVar(MLong))

    // All primitive combinations
    val uniqueCombinations = (for (lhs <- primitives; rhs <- primitives) yield (lhs,rhs)).distinct
    for ((lhs,rhs) <- uniqueCombinations) {
      if (!(lhs.stage == now && rhs.stage == now)) {
        val ret = returnValue(lhs,rhs)
        val (leftArg, rightArg) = (formatArg("lhs", lhs, ret), formatArg("rhs", rhs, ret))
        infix (Prim) ("+", Nil, ((("lhs",lhs),("rhs",rhs)) :: ret)) implements redirect ${ forge_\${ret.name.toLowerCase}_plus(\$leftArg, \$rightArg) }
        infix (Prim) ("-", Nil, ((("lhs",lhs),("rhs",rhs)) :: ret)) implements redirect ${ forge_\${ret.name.toLowerCase}_minus(\$leftArg, \$rightArg) }
        infix (Prim) ("*", Nil, ((("lhs",lhs),("rhs",rhs)) :: ret)) implements redirect ${ forge_\${ret.name.toLowerCase}_times(\$leftArg, \$rightArg) }
        infix (Prim) ("/", Nil, ((("lhs",lhs),("rhs",rhs)) :: ret)) implements redirect ${ forge_\${ret.name.toLowerCase}_divide(\$leftArg, \$rightArg) }
      }
    }
  }
}
