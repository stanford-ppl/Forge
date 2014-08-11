package ppl.dsl.forge
package dsls.optiml
package factor

import core.{ForgeApplication,ForgeApplicationRunner}

trait TableFactorOps {
  this: OptiMLDSL =>

  def importTableFactorOps() {
    val DenseVector = lookupTpe("DenseVector")
    val FVariable = lookupTpe("FactorVariable")
    val TableFactor = tpe("TableFactor")

    // here, _val is a prod(_card) length vector with a value for every assignment
    data(TableFactor, ("_vars", DenseVector(FVariable)), ("_vals", DenseVector(MDouble)))

    static (TableFactor) ("apply", Nil, (("vars", DenseVector(FVariable)), ("vals", DenseVector(MDouble))) :: TableFactor) implements
      allocates(TableFactor, ${$0}, ${$1})


    // TODO: should generalize flatten / unflatten to use vectors instead of tuples and then implement these using flatten / unflatten?
    direct (TableFactor) ("assignmentToIndex", Nil, DenseVector(MInt) :: MInt) implements composite ${
      fatal("TBD")
    }

    direct (TableFactor) ("indexToAssignment", Nil, MInt :: DenseVector(MInt)) implements composite ${
      fatal("TBD")
    }

    val TableFactorOps = withTpe(TableFactor)
    TableFactorOps {
      infix ("vars") (Nil :: DenseVector(FVariable)) implements getter(0, "_vars")
      infix ("vals") (Nil :: DenseVector(MDouble)) implements getter(0, "_vals")
    }
  }
}
