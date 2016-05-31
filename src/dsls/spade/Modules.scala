package ppl.dsl.forge
package dsls
package spade

// This method for creating modules currently can't handle an arbitrary number of inputs or outputs, as it is
// fundamentally limited by typing using Scala tuples.
// As with most things in Forge relating to tuples, this leads to a huge amount of code bloat
// TODO: More general way to define/instantiate modules? Can always define records instead to get around this
// TODO: Bad things will probably happen here if maxTuples + maxOutputArity > 26
trait Modules {
  this: SpadeDSL =>

  val maxOutputArity = 4

  def importModules() = {
    val Module = grp("Module")

    def createVariations(tpes: List[String]) {
      val arity = tpes.length
      val AA = tpes.map(t => tpePar(t))
      val R = tpePar(('A'.toInt+maxTuples).toChar.toString)

      val packedType = "Rep[" + (if (arity > 1) "Tup" + arity + "[" + tpes.mkString(",") + "]" else tpes.head) + "]"

      val splitInputs = if (arity > 1) (1 to arity).map{i => "t._" + i}.mkString(",") else "t"
      val tupledInputs = if (arity > 1) "pack((" + (1 to arity).map(i => quotedArg(i)).mkString(",") + "))" else quotedArg(1)


      direct (Module) ("instance", AA:+R, CurriedMethodSignature(List(List(AA ==> R), AA), R)) implements composite ${
        // Create wrapper function: Rep[A,B,...] => Rep[R] instead of (Rep[A], Rep[B]...) => Rep[R]
        val lambda = (t: \${packedType}) => $0(\${splitInputs})
        // Stage wrapper function
        val stagedModule = doLambda(lambda)
        // Pack inputs from Rep[A], Rep[B], ... to Rep[A,B,...]
        val inputs = \${tupledInputs}
        // Apply staged method
        doApply(stagedModule, inputs)
      }

      for (outArity <- 2 to maxOutputArity) {
        val CT = lookupTpe("Tuple" + outArity, stage=compile)
        val TT = lookupTpe("Tup" + outArity)

        val RR = (0 until outArity).map{i => tpePar(('A'.toInt+maxTuples+i).toChar.toString)}.toList
        val B = TT(RR:_*)
        val R = CT(RR:_*)

        direct (Module) ("instance", AA++RR, CurriedMethodSignature(List(List(AA ==> B), AA), R)) implements composite ${
          // Create wrapper function: Rep[A,B,...] => Rep[R] instead of (Rep[A], Rep[B]...) => Rep[R]
          val lambda = (t: \${packedType}) => $0(\${splitInputs})
          // Stage wrapper function
          val stagedModule = doLambda(lambda)
          // Pack inputs from Rep[A], Rep[B], ... to Rep[A,B,...]
          val inputs = \${tupledInputs}
          // Apply staged method
          val out = doApply(stagedModule, inputs)
          // Unpack output Rep[A,B,...] to (Rep[A], Rep[B], ...)
          unpack(out)
        }
      }
    }

    for (arity <- 1 until maxTuples) {
      val tpes = (0 until arity).map{i => ('A'.toInt+i).toChar.toString}.toList
      createVariations(tpes)
    }
  }
}
