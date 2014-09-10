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
        if ($1 == "" && $self.default != 0.0209) {
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

    // FIXME: this produces an illegal ordering of effect error when 'composite' instead of 'single'. related to combo of array_fromseq (mutable) and FHashMap?    
    static (DiscreteFeature) ("apply", Nil, (varArgs(MString) :: DiscreteFeature)) implements single ${
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
  }
}
