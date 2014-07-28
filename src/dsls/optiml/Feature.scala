package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait FeatureOps {
  this: OptiMLDSL =>

  def importFeatureOps() {
    val DenseVector = lookupTpe("DenseVector")
    
    val ContinuousFeature = tpe("ContinuousFeature")
    data(ContinuousFeature, ("_min", MDouble), ("_max", MDouble))
    static (ContinuousFeature) ("apply", Nil, (("min", MDouble, "math_ninf()"), ("max", MDouble, "math_inf()")) :: ContinuousFeature) implements allocates(ContinuousFeature, ${$0}, ${$1})

    val ContinuousFeatureOps = withTpe(ContinuousFeature)
    ContinuousFeatureOps {
      infix ("min") (Nil :: MDouble) implements getter(0, "_min")
      infix ("max") (Nil :: MDouble) implements getter(0, "_max")
      infix ("apply") (MString :: MDouble) implements composite ${ 
        val num = $1.toDouble
        if (num < $self.min) { $self.min }
        else if (num > $self.max) { $self.max }
        else num
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
    data(BinaryFeature)
    static (BinaryFeature) ("apply", Nil, (Nil :: BinaryFeature)) implements allocates(BinaryFeature)

    val BinaryFeatureOps = withTpe(BinaryFeature)
    BinaryFeatureOps {
      infix ("apply") (MString :: MInt) implements composite ${ 
        fassert($1.toInt == 0 || $1.toInt == 1, "illegal input to binary feature")
        $1.toInt
      }
    }
  }
}
