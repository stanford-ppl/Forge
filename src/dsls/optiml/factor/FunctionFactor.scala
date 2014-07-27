package ppl.dsl.forge
package dsls.optiml
package factor

import core.{ForgeApplication,ForgeApplicationRunner}

trait FunctionFactorOps {
  this: OptiMLDSL =>

  def importFunctionFactorOps() {
    val DenseVector = lookupTpe("DenseVector")
    val FVariable = lookupTpe("FactorVariable")
    val FunctionFactor = tpe("FunctionFactor")

    data(FunctionFactor, ("_id", MInt), ("_vars", DenseVector(FVariable)), ("_weightId", MInt), ("_funcId", MInt))

    static (FunctionFactor) ("apply", Nil, (("id", MInt), ("vars", DenseVector(FVariable)), ("weightId", MInt), ("func", MInt)) :: FunctionFactor) implements
      composite ${ function_factor_alloc_helper($0, $1, $2, $3) }

    compiler (FunctionFactor) ("function_factor_alloc_helper", Nil, (("id", MInt), ("vars", DenseVector(FVariable)), ("weightId", MInt), ("funcId", MInt)) :: FunctionFactor) implements
      allocates(FunctionFactor, ${$0}, ${$1}, ${$2}, ${$3})

    // some useful factor functions
    // maps from an assignment of the variable values to an output value

    compiler (FunctionFactor) ("or_factor_func", Nil, DenseVector(MDouble) :: MDouble) implements composite ${
      if ($0.length == 0 || $0.filter(_ == 1.0).length > 0) 1.0 else 0.0
    }

    compiler (FunctionFactor) ("and_factor_func", Nil, DenseVector(MDouble) :: MDouble) implements composite ${
      if ($0.length == 0 || $0.filter(_ == 1.0).length == $0.length) 1.0 else 0.0
    }

    compiler (FunctionFactor) ("imply_factor_func", Nil, DenseVector(MDouble) :: MDouble) implements composite ${
      if ($0.length == 1) {
        $0(0)
      }
      else if ($0.take($0.length - 1).contains(0.0)) {
        1.0
      }
      else if ($0.last == 0.0) {
        0.0
      }
      else {
        1.0
      }
    }

    compiler (FunctionFactor) ("equal_factor_func", Nil, DenseVector(MDouble) :: MDouble) implements composite ${
      if ($0.length == 2) {
        if ($0(0) == $0(1)) 1.0
        else 0.0
      }
      else {
        fatal("cannot evaluate equality between more than 2 variables")
      }
    }

    compiler (FunctionFactor) ("istrue_factor_func", Nil, DenseVector(MDouble) :: MDouble) implements composite ${
      if ($0.length == 1) $0.first
      else fatal("cannot evaluate isTrue for more than 1 variable")
    }

    // factor function directory. this mapping is needed because we cannot store non-primitive types in a data field,
    // so we have to map between the user abstraction to the back-end int to the actual run-time function.
    compiler (FunctionFactor) ("factor_func_to_int", Nil, MString :: MInt) implements composite ${
      if ($0 == "ImplyFactorFunction") 0
      else if ($0 == "OrFactorFunction") 1
      else if ($0 == "AndFactorFunction") 2
      else if ($0 == "EqualFactorFunction") 3
      else if ($0 == "IsTrueFactorFunction") 4
      else fatal("no factor function of type " + $0 + " found")
    }

    compiler (FunctionFactor) ("evaluate_factor_func", Nil, (("funcId", MInt), ("vals", DenseVector(MDouble))) :: MDouble) implements composite ${
      // if the conditional is known at staging time, we can inline the exact function
      // and as a consequence, the back-end 'funcId' field in the factor should be DFE'd
      if (funcId == 0) {
        imply_factor_func(vals)
      }
      else if (funcId == 1) {
        or_factor_func(vals)
      }
      else if (funcId == 2) {
        and_factor_func(vals)
      }
      else if (funcId == 3) {
        equal_factor_func(vals)
      }
      else if (funcId == 4) {
        istrue_factor_func(vals)
      }
      else {
        fatal("no factor func with id " + funcId + " found")
      }
    }

    val FunctionFactorOps = withTpe(FunctionFactor)
    FunctionFactorOps {
      infix ("id") (Nil :: MInt) implements getter(0, "_id")
      infix ("vars") (Nil :: DenseVector(FVariable)) implements getter(0, "_vars")
      infix ("weightId") (Nil :: MInt) implements getter(0, "_weightId")
      infix ("funcId") (Nil :: MInt) implements getter(0, "_funcId")
      infix ("evaluate") (DenseVector(MDouble) :: MDouble) implements composite ${ evaluate_factor_func($self.funcId, $1) }
    }
  }
}
