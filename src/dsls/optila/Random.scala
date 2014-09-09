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

    direct (Rand) ("randomInt", Nil, MInt :: MInt, effect = simple) implements codegen($cala, ${
      Global.randRef.nextInt($0)
    })

    direct (Rand) ("randomGaussian", Nil, Nil :: MDouble, effect = simple) implements codegen($cala, ${
      Global.randRef.nextGaussian()
    })

    direct (Rand) ("reseed", Nil, Nil :: MUnit, effect = simple) implements codegen($cala, ${
      Global.randRef.setSeed(Global.INITIAL_SEED)
    })

    compiler (Rand) ("optila_rand_double", Nil, Nil :: MDouble, effect = simple) implements codegen($cala, ${
      Global.randRef.nextDouble()
    })
    compiler (Rand) ("optila_rand_float", Nil, Nil :: MFloat, effect = simple) implements codegen($cala, ${
      Global.randRef.nextFloat()
    })
    compiler (Rand) ("optila_rand_int", Nil, Nil :: MInt, effect = simple) implements codegen($cala, ${
      Global.randRef.nextInt()
    })
    compiler (Rand) ("optila_rand_boolean", Nil, Nil :: MBoolean, effect = simple) implements codegen($cala, ${
      Global.randRef.nextBoolean()
    })

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

    direct (Rand) ("sample", Nil, (("v",IndexVector), ("pct", MDouble)) :: IndexVector, effect = simple) implements composite ${
      IndexVector(sample($0.toDense,pct), $0.isRow)
    }

    direct (Rand) ("sample", A, (("v",DenseVector(A)), ("pct", MDouble)) :: DenseVector(A), effect = simple) implements composite ${
      val candidates = (0::v.length).mutable

      val sampled = DenseVector[A](0, v.isRow)
      val numSamples = ceil(v.length * pct)
      for (i <- 0 until numSamples){
        val r = i + randomInt(v.length-i)
        val idx = candidates(r)
        sampled <<= v(idx)

        // remove index r from consideration
        val t = candidates(r)
        candidates(r) = candidates(i)
        candidates(i) = t
      }

      sampled.unsafeImmutable
    }      

    direct (Rand) ("sample", A, MethodSignature(List(("m",DenseMatrix(A)), ("pct", MDouble), ("sampleRows", MBoolean, "unit(true)")), DenseMatrix(A)), effect = simple) implements composite ${      
      val numSamples = if (sampleRows) ceil(m.numRows*pct) else ceil(m.numCols*pct)
      val length = if (sampleRows) m.numRows else m.numCols
      val newRows = if (sampleRows) numSamples else m.numRows
      val newCols = if (sampleRows) m.numCols else numSamples

      val sampled = if (sampleRows) DenseMatrix[A](0, newCols) else DenseMatrix[A](0, newRows) // transposed for efficiency
      
      val candidates = (0::length).mutable

      // transpose to make constructing sampling more efficient
      val mt = if (sampleRows) m else m.t

      for (i <- 0 until numSamples){
        val r = i + randomInt(length-i)
        val idx = candidates(r)
        sampled <<= mt(idx).Clone

        // remove index r from consideration
        val t = candidates(r)
        candidates(r) = candidates(i)
        candidates(i) = t
      }

      if (sampleRows) sampled else sampled.t
    }
  }
}
