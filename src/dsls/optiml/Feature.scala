package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait FeatureOps {
  this: OptiMLDSL =>

  def importFeatureOps() {
    val DenseVector = lookupTpe("DenseVector")

    val ContinuousFeature = tpe("ContinuousFeature")
    data(ContinuousFeature, ("_default", MDouble), ("_min", MDouble), ("_max", MDouble))

    // we use a magic number for the default because we don't support optional values in generated DSL code (yet)
    static (ContinuousFeature) ("apply", Nil, (("default", MDouble, "unit(0.0209)"), ("min", MDouble, "math_ninf()"), ("max", MDouble, "math_inf()")) :: ContinuousFeature) implements allocates(ContinuousFeature, ${$0}, ${$1}, ${$2})

    val ContinuousFeatureOps = withTpe(ContinuousFeature)
    ContinuousFeatureOps {
      infix ("default") (Nil :: MDouble) implements getter(0, "_default")
      infix ("min") (Nil :: MDouble) implements getter(0, "_min")
      infix ("max") (Nil :: MDouble) implements getter(0, "_max")
      infix ("apply") (MString :: MDouble) implements composite ${
        if (self.default != 0.0209 && ($1 == "" || $1.toLowerCase == "null")) {
          $self.default
        }
        else {
          val num = $1.toDouble
          if (num < $self.min) { $self.min }
          else if (num > $self.max) { $self.max }
          else num
        }
      }
    }

    val DiscreteFeature = tpe("DiscreteFeature")
    data(DiscreteFeature, ("_features", MHashMap(MString, MInt)))
    compiler (DiscreteFeature) ("discrete_feature_alloc", Nil, (MHashMap(MString, MInt) :: DiscreteFeature)) implements allocates(DiscreteFeature, ${$0})

    // FIXME: this produces an illegal ordering of effect error when array_fromseq is mutable. What happens with MHashMap in this case?
    static (DiscreteFeature) ("apply", Nil, (varArgs(MString) :: DiscreteFeature)) implements composite ${
      val keys = array_fromseq($0)
      val values = array_fromfunction(array_length(keys), i => i)
      discrete_feature_alloc(fhashmap_from_arrays(keys, values))
    }

    val DiscreteFeatureOps = withTpe(DiscreteFeature)
    DiscreteFeatureOps {
      compiler ("getFeatures") (Nil :: MHashMap(MString, MInt)) implements getter(0, "_features")

      infix ("apply") (MString :: MInt) implements composite ${
        val featureMap = getFeatures($self)
        if (featureMap.contains($1)) featureMap($1) else 0
      }

      // convert this feature value into an indicator or dummy variable encoding
      infix ("indicator") (MString :: DenseVector(MInt)) implements composite ${
        val featureMap = getFeatures($self)
        val numKeys = fhashmap_size(featureMap)
        val e = if (featureMap.contains($1)) featureMap($1) else 0
        (0::numKeys) { i => if (i == e) 1 else 0 }
      }

      infix ("size") (Nil :: MInt) implements composite ${
        val featureMap = getFeatures($self)
        fhashmap_size(featureMap)
      }
    }

    val BinaryFeature = tpe("BinaryFeature")
    data(BinaryFeature, ("_default", MBoolean))
    static (BinaryFeature) ("apply", Nil, ("default", MBoolean, "unit(false)") :: BinaryFeature) implements allocates(BinaryFeature, ${$0})

    val BinaryFeatureOps = withTpe(BinaryFeature)
    BinaryFeatureOps {
      infix ("default") (Nil :: MBoolean) implements getter(0, "_default")

      infix ("apply") (MString :: MInt) implements composite ${
        fassert($1 == "" || $1 == "0" || $1 == "1" || $1 == "false" || $1 == "true", "illegal input to binary feature")
        if ($1 == "0" || $1 == "false") 0
        else if ($1 == "1" || $1 == "true") 1
        else if ($self.default) 1
        else 0
      }
    }

    val DateFeature = tpe("DateFeature")
    val SDateFormat = tpe("java.text.DateFormat")

    static (DateFeature) ("apply", Nil, MString :: SDateFormat) implements codegen($cala, ${
      new java.text.SimpleDateFormat($0)
    })

    infix (DateFeature) ("apply", Nil, (SDateFormat,MString) :: MDouble) implements codegen($cala, ${
      if ($1 == "") 0.0 // defaults to 1970, for better or worse... what else can we do?
      else {
        val date = $0.parse($1, new java.text.ParsePosition(0))
        if (date == null) 0.0
        else date.getTime.toDouble
      }
    })

  }

  def importFeatureHelperOps() {
    val FeatureHelper = grp("FeatureHelper")
    val DenseVector = lookupTpe("DenseVector")
    val Tup2 = lookupTpe("Tup2")

    /* Return a unique integer identifier for a given string */
    direct (FeatureHelper) ("unique", Nil, MString :: MInt) implements codegen($cala, ${
      MLGlobal.getId($0)
    })

    // direct (FeatureHelper) ("getUniqueMappings", Nil, Nil :: DenseVector(Tup2(MInt,MString))) implements composite ${
    //   val names = get_unique_names_helper
    //   val ids = get_unique_ids_helper
    //   fassert(names.length == ids.length, "names and ids in unique map are different lengths")
    //   val data = array_fromfunction(names.length,  i => pack((ids(i), names(i))))
    //   densevector_fromarray(data, true)
    // }

    // compiler (FeatureHelper) ("get_unique_names_helper", Nil, Nil :: MArray(MString)) implements codegen($cala, ${
    //   MLGlobal.getUniqueNames
    // })

    // compiler (FeatureHelper) ("get_unique_ids_helper", Nil, Nil :: MArray(MInt)) implements codegen($cala, ${
    //   MLGlobal.getUniqueIds
    // })

    direct (FeatureHelper) ("loadUniqueMappings", Nil, MString :: MInt, effect = simple) implements codegen($cala, ${
      MLGlobal.loadUniqueMappings($0)
    })

    direct (FeatureHelper) ("dumpUniqueMappings", Nil, MString :: MUnit, effect = simple) implements codegen($cala, ${
      MLGlobal.dumpUniqueMappings($0)
    })
  }
}
