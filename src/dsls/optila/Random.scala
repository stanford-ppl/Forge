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
    val MultiArray  = lookupTpe("MultiArray")
   	//val Fixed       = lookupTpe("Fixed")
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

    direct (Rand) ("randomInt", Nil, MInt :: MInt, effect = simple) implements codegen($cala, ${
      Global.randRef.nextInt($0)
    }) /*exhibits behavior ${
      if ($0.max <= 0) {
        warn("Argument to randomInt is always less than or equal to zero, which will result in a runtime error")(__pos)
        SymRange[Int]
      }
      else SymRange[Int](0, $0.max - 1)
    }*/

    direct (Rand) ("randomGaussian", Nil, Nil :: MDouble, effect = simple) implements codegen($cala, ${
      Global.randRef.nextGaussian()
    })

    direct (Rand) ("reseed", Nil, Nil :: MUnit, effect = simple) implements codegen($cala, ${
      Global.randRef.setSeed(Global.INITIAL_SEED)
    })
    direct (Rand) ("seed", Nil, MLong :: MUnit, effect = simple) implements codegen($cala, ${
      Global.randRef.setSeed($0)
    })

    compiler (Rand) ("optila_rand_double", Nil, Nil :: MDouble, effect = simple) implements codegen($cala, ${
      Global.randRef.nextDouble()
    }) /*exhibits behavior ${ RealRange[Double](0, 1, false, false, false) }*/

    compiler (Rand) ("optila_rand_float", Nil, Nil :: MFloat, effect = simple) implements codegen($cala, ${
      Global.randRef.nextFloat()
    }) /*exhibits behavior ${ RealRange[Float](0, 1, false, false, false) }*/

    compiler (Rand) ("optila_rand_int", Nil, Nil :: MInt, effect = simple) implements codegen($cala, ${
      Global.randRef.nextInt()
    })
    compiler (Rand) ("optila_rand_boolean", Nil, Nil :: MBoolean, effect = simple) implements codegen($cala, ${
      Global.randRef.nextBoolean()
    })
    //compiler (Rand) ("optila_rand_fixed", Nil, Nil :: Fixed, effect = simple) implements single ${
    //  Fixed(optila_rand_int)
    //}


    direct (Rand) ("shuffle", Nil, IndexVector :: IndexVector, effect = simple) implements composite ${
    	indexvector_fromarray(fmultia_to_array(shuffle($0.toDense)), vector_dims($0))
    }

    direct (Rand) ("shuffle", A, DenseVector(A) :: DenseVector(A), effect = simple) implements composite ${
      val v2 = $0.mutable
      val a = optila_shuffle_array(fmultia_trimmed_array(v2))
      fmultia_fromarray(a, fmultia_dims($0), mutable = false)
    }

    direct (Rand) ("shuffle", A, DenseMatrix(A) :: DenseMatrix(A), effect = simple) implements composite ${
      val m2 = $0.mutable
      val a = optila_shuffle_array(fmultia_trimmed_array(m2))
      fmultia_fromarray(a, fmultia_dims($0), mutable = false)
    }

    direct (Rand) ("shuffle", A, MultiArray(A) :: MultiArray(A), effect = simple) implements composite ${
    	val md2 = $0.mutable
    	val a = optila_shuffle_array(fmultia_trimmed_array(md2))
    	fmultia_fromarray(a, fmultia_dims($0), mutable = false)
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
