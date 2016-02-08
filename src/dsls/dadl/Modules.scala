package ppl.dsl.forge
package dsls
package dadl

trait ModuleOps {
  this: DADLDSL =>

  def importModules() = {
    val Module = grp("Module")
    val A = tpePar("A")
    val B = tpePar("B")

    for (arity <- 2 until maxTuples) {
      val pars = (0 until arity).map(i => tpePar(('A'.toInt+i).toChar.toString)).toList
      val R = tpePar(('A'.toInt+maxTuples).toChar.toString)

      val tupleSplit = (1 to arity).map{i => "tuple" + arity + "_get" + (i+1).toString + "(t)"}.toList.mkString(",")
      val tupledInputs = (1 to arity).map(i => quotedArg(i)).mkString(",")

      direct (Module) ("instance", pars:+R, CurriedMethodSignature(List(List(pars ==> R),pars), R)) implements composite ${
        // Create wrapper function: Rep[A,B,...] => Rep[R] instead of (Rep[A], Rep[B]...) => Rep[R]
        val lambda = (t: Rep[\${pars.mkString(",")}]) => $0(\${tupleSplit})
        // Stage wrapper function
        val stagedModule = doLambda(lambda)
        // Pack inputs from Rep[A], Rep[B], ... to Rep[A,B,...]
        val inputs = pack((\${tupledInputs}))
        // Apply staged method
        doApply(stagedModule, inputs)
      }

    }
  }
}
