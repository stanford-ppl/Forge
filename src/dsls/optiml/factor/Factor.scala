package ppl.dsl.forge
package dsls.optiml
package factor

import core.{ForgeApplication,ForgeApplicationRunner}

trait FactorOps extends TableFactorOps with FunctionFactorOps {
  this: OptiMLDSL =>

  def importAllFactorOps() {
    importFactorVariableOps()
    importTableFactorOps()
    importFunctionFactorOps()
    importFactorOps()
  }

  def importFactorVariableOps() {
    val DenseVector = lookupTpe("DenseVector")

    val FVariable = tpe("FactorVariable")
    data(FVariable, ("_id", MInt), ("_isPositive", MBoolean), ("_domain", DenseVector(MDouble)), ("_position", MInt))

    static (FVariable) ("apply", Nil, (("id", MInt), ("isPositive", MBoolean), ("domain", DenseVector(MDouble)), ("position", MInt)) :: FVariable) implements allocates(FVariable, ${$0}, ${$1}, ${$2}, ${$3})

    val FVariableOps = withTpe(FVariable)
    FVariableOps {
      infix ("id") (Nil :: MInt) implements getter(0, "_id")
      infix ("isPositive") (Nil :: MBoolean) implements getter(0, "_isPositive")
      infix ("domain") (Nil :: DenseVector(MDouble)) implements getter(0, "_domain")
      infix ("position") (Nil :: MInt) implements getter(0, "_position")
    }
  }

  // -- Factor type-class
  // the main issue with this organization is that we cannot store multiple factor types in a single graph
  // unless we store a separate map per factor type. we should look into an interface / struct inheritance model.

  object TFactor extends TypeClassSignature {
    def name = "Factor"
    def prefix = "_fact"
    def wrapper = Some("facttype")
  }

  def importFactorOps() {
    val T = tpePar("T")
    val DenseVector = lookupTpe("DenseVector")
    val FVariable = lookupTpe("FactorVariable")

    val Factor = tpeClass("Factor", TFactor, T)

    // Factor interface
    infix (Factor) ("vars", T, T :: DenseVector(FVariable))
    infix (Factor) ("valueOfAssignment", T, (T, DenseVector(MDouble)) :: MDouble)
    infix (Factor) ("weightId", T, T :: MDouble)

    // TableFactor impl
    val TableFactor = lookupTpe("TableFactor")
    val FactorTableFactor = tpeClassInst("FactorTableFactor", Nil, Factor(TableFactor))

    infix (FactorTableFactor) ("vars", Nil, TableFactor :: DenseVector(FVariable)) implements composite ${ $0.vars }
    infix (FactorTableFactor) ("valueOfAssignment", Nil, (TableFactor, DenseVector(MDouble)) :: MDouble) implements composite ${
      // this is slow, since we need to figure out the logical assignment from the value assignment
      // is there a better way?
      val assignment = $0.vars.map(_.domain).zip($1) { (domain, value) => domain.find(_ == value).first }
      val index = assignmentToIndex(assignment)
      $0.vals.apply(index)
    }
    infix (FactorTableFactor) ("weightId", Nil, TableFactor :: MDouble) implements composite ${ unit(0.0) } // TODO

    // FunctionFactor impl
    val FunctionFactor = lookupTpe("FunctionFactor")
    val FactorFunctionFactor = tpeClassInst("FactorFunctionFactor", Nil, Factor(FunctionFactor))

    infix (FactorFunctionFactor) ("vars", Nil, FunctionFactor :: DenseVector(FVariable)) implements composite ${ $0.vars }
    infix (FactorFunctionFactor) ("valueOfAssignment", Nil, (FunctionFactor, DenseVector(MDouble)) :: MDouble) implements composite ${
      $0.evaluate($1)
    }
    infix (FactorFunctionFactor) ("weightId", Nil, FunctionFactor :: MDouble) implements composite ${ $0.weightId }
  }
}
