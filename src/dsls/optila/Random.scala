package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait RandomOps {
  this: OptiLADSL =>

  def importRandomOps() {
    val Rand = grp("Rand")
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val A = tpePar("A")

    direct (Rand) ("random", A, Nil :: A) implements composite ${
      val mA = manifest[A]
      mA match {
        case Manifest.Double => optila_rand_double.AsInstanceOf[A]
        case Manifest.Float => optila_rand_float.AsInstanceOf[A]
        case Manifest.Int => optila_rand_int.AsInstanceOf[A]
        case Manifest.Boolean => optila_rand_boolean.AsInstanceOf[A]
        case _ => sys.error("no random implementation available for type " + mA.toString)
      }
    }

    direct (Rand) ("randomElem", A, DenseVector(A) :: A, effect = simple) implements composite ${
      $0(randomInt($0.length))
    }

    val randomint = direct (Rand) ("randomInt", Nil, MInt :: MInt, effect = simple)
    impl (randomint) (codegen($cala, ${ Global.randRef.nextInt($0) }))
    impl (randomint) (codegen(cpp, ${ resourceInfo.rand->nextInt($0) }))

    val randomgaussian = direct (Rand) ("randomGaussian", Nil, Nil :: MDouble, effect = simple)
    impl (randomgaussian) (codegen($cala, ${ Global.randRef.nextGaussian() }))
    impl (randomgaussian) (codegen(cpp, ${ resourceInfo.rand->nextGaussian() }))

    val reseed = direct (Rand) ("reseed", Nil, Nil :: MUnit, effect = simple)
    impl (reseed) (codegen($cala, ${ Global.randRef.setSeed(Global.INITIAL_SEED) }))
    impl (reseed) (codegen(cpp, ${ resourceInfo.rand->setSeed(DeliteCppRandom.INITIAL_SEED) }))

    val randdouble = compiler (Rand) ("optila_rand_double", Nil, Nil :: MDouble, effect = simple)
    impl (randdouble) (codegen($cala, ${ Global.randRef.nextDouble() }))
    impl (randdouble) (codegen(cpp, ${ resourceInfo.rand->nextDouble() }))

    val randfloat = compiler (Rand) ("optila_rand_float", Nil, Nil :: MFloat, effect = simple)
    impl (randfloat) (codegen($cala, ${ Global.randRef.nextFloat() }))
    impl (randfloat) (codegen(cpp, ${ resourceInfo.rand->nextFloat() }))

    val randint = compiler (Rand) ("optila_rand_int", Nil, Nil :: MInt, effect = simple)
    impl (randint) (codegen($cala, ${ Global.randRef.nextInt() }))
    impl (randint) (codegen(cpp, ${ resourceInfo.rand->nextInt() }))

    val randboolean = compiler (Rand) ("optila_rand_boolean", Nil, Nil :: MBoolean, effect = simple)
    impl (randboolean) (codegen($cala, ${ Global.randRef.nextBoolean() }))
    impl (randboolean) (codegen(cpp, ${ resourceInfo.rand->nextBoolean() }))

    direct (Rand) ("shuffle", Nil, IndexVector :: IndexVector, effect = simple) implements composite ${
      indexvector_fromarray(densevector_raw_data(shuffle($0.toDense)), $0.isRow)
    }

    direct (Rand) ("shuffle", A, DenseVector(A) :: DenseVector(A), effect = simple) implements composite ${
      val v2 = $0.mutable
      v2.trim()
      val a = optila_shuffle_array(densevector_raw_data(v2))
      densevector_fromarray(a, $0.isRow)
    }

    direct (Rand) ("shuffle", A, DenseMatrix(A) :: DenseMatrix(A), effect = simple) implements composite ${
      val m2 = $0.mutable
      m2.trim()
      val a = optila_shuffle_array(densematrix_raw_data(m2))
      densematrix_fromarray(a, $0.numRows, $0.numCols)
    }

    // any good parallel implementation?
    compiler (Rand) ("optila_shuffle_array", A, MArray(A) :: MArray(A), effect = simple) implements single ${
      val len = array_length($0)
      val out = array_empty[A](len)
      array_copy($0, 0, out, 0, len)

      var i = len-1
      while (i > 1) {
        val swap = randomInt(i+1)
        val a = array_apply(out,i)
        array_update(out,i,array_apply(out,swap))
        array_update(out,swap,a)
        i -= 1
      }

      out.unsafeImmutable
    }
  }
}
