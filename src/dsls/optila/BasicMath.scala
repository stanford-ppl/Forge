package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

import scala.collection.mutable.HashMap

trait BasicMathOps {
  this: OptiLADSL =>

  // TODO: Migrate generalization of this to Forge? (Ideally would work more like type classes)
  object UnaryMath /* extends FunctionGroupSignature? something like that.. */ {
    val ops = List("exp", "log", "log10", "sqrt", "sin", "sinh", "asin", "cos", "cosh", "acos", "tan", "tanh", "atan")
    val tpemap = HashMap[Rep[DSLType], Rep[DSLType]]()

    def apply(k: Rep[DSLType], v: Rep[DSLType]) { tpemap(k) = v }
    def forall(f: (String, Rep[DSLType], Rep[DSLType]) => Any) { ops foreach {op => tpemap foreach {tps => f(op, tps._1, tps._2) } } }
    def forType(k: Rep[DSLType])(f: (String, Rep[DSLType]) => Any) { ops foreach {op => f(op, tpemap(k)) }}
  
    def foreachType(f: (Rep[DSLType], Rep[DSLType]) => Any) { tpemap foreach {t => f(t._1, t._2) } }
  }

  def importBasicMathOps() {
    val T = tpePar("T")
    val A = tpePar("A")
    val B = tpePar("B")

    val Math = grp("BasicMath")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    // -- aliases for Math._ ( instead of Math.foo(args), in the DSL we can just use foo(args) )
    for (Num <- List(MDouble, MFloat, MLong, MInt)) {
      val cast = if (Num == MDouble) "" else ".toDouble"
      UnaryMath.ops foreach { op => direct (Math) (op, Nil, Num :: MDouble) implements redirect { "Math." + op + "(" + quotedArg(0) + cast + ")"} }
      UnaryMath(Num, MDouble)
    }
    
    // -- other math ops
    direct (Math) ("pow", Nil, (MDouble,MDouble) :: MDouble) implements redirect ${ Math.pow($0,$1) }
    infix (Math)  ("**", Nil, (MDouble,MInt) :: MDouble) implements redirect ${ Math.pow($0,$1) }
    
    direct (Math) ("round", Nil, MDouble :: MLong) implements redirect ${ Math.round($0) }
    direct (Math) ("atan2", Nil, (MDouble,MDouble) :: MDouble) implements redirect ${ Math.atan2($0,$1) }

    direct (Math) ("sigmoid", Nil, MDouble :: MDouble) implements composite ${ 1.0 / (1.0 + exp(-$0)) }
    direct (Math) ("asinh", Nil, MDouble :: MDouble) implements composite ${ log($0 + sqrt( square($0) + 1)) }
    direct (Math) ("acosh", Nil, MDouble :: MDouble) implements composite ${ log($0 + sqrt( square($0) - 1)) }
    direct (Math) ("atanh", Nil, MDouble :: MDouble) implements composite ${ 0.5 * (log($0 + 1) - log(1 - $0)) }

    // -- pdfs
    direct (Math) ("normpdf", Nil, (("x",MDouble),("mu",MDouble),("sigma",MDouble)) :: MDouble) implements single ${
      (1.0 / (sigma * sqrt(2.0*Pi))) * exp(-((x-mu)*(x-mu)) / (2.0*sigma*sigma))
    }

    direct (Math) ("normpdf", Nil, (("x",DenseVector(MDouble)),("mu",DenseVector(MDouble)),("sigma",DenseVector(MDouble))) :: DenseVector(MDouble)) implements composite ${
      (0::x.length) { i => normpdf(x(i), mu(i), sigma(i)) }
    }

    direct (Math) ("normpdf", Nil, (("x",DenseMatrix(MDouble)),("mu",DenseMatrix(MDouble)),("sigma",DenseMatrix(MDouble))) :: DenseMatrix(MDouble)) implements composite ${
      (0::x.numRows, 0::x.numCols) { (i,j) => normpdf(x(i,j), mu(i,j), sigma(i,j)) }
    }

    for (Col <- List(DenseVector(MDouble),DenseMatrix(MDouble))) {
      direct (Math) ("normpdf", Nil, (("x",Col),("mu",MDouble),("sigma",Col)) :: Col) implements redirect ${
        x.zip(sigma) { (a,b) => normpdf(a, mu, b) }
      }
      direct (Math) ("normpdf", Nil, (("x",Col),("mu",Col),("sigma",MDouble)) :: Col) implements redirect ${
        x.zip(mu) { (a,b) => normpdf(a, b, sigma) }
      }
      direct (Math) ("normpdf", Nil, (("x",Col),("mu",MDouble),("sigma",MDouble)) :: Col) implements redirect ${
        x.map { e => normpdf(e, mu, sigma) }
      }
    }

    // -- math on sequences
    // can't have a sequence-based sum, because it makes sum(0,n) { i => ... } ambiguous for int arguments
    // direct (Math) ("sum", T, varArgs(T) :: T, TArith(T)) implements composite ${ densevector_fromarray(array_fromseq($0),true).sum }
    direct (Math) ("mean", T, varArgs(T) :: MDouble, (TArith(T), TNumeric(T))) implements composite ${ vector_fromarray(array_fromseq($0)).mean }
    direct (Math) ("min", T, varArgs(T) :: T, TOrder(T)) implements composite ${ vector_fromarray(array_fromseq($0)).min }
    direct (Math) ("max", T, varArgs(T) :: T, TOrder(T)) implements composite ${ vector_fromarray(array_fromseq($0)).max }
    direct (Math) ("median", T, varArgs(T) :: MDouble, (TNumeric(T),TOrder(T))) implements composite ${ vector_fromarray(array_fromseq($0)).median }
  }
}
