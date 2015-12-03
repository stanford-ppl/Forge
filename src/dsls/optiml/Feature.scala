package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}
import org.scala_lang.virtualized.virtualize

trait FeatureOps {
  this: OptiMLDSL =>

  def importFeatureOps() {
    val DenseVector = lookupTpe("DenseVector")

    val ContinuousFeature = tpe("ContinuousFeature")
    data(ContinuousFeature, ("_default", MDouble), ("_min", MDouble), ("_max", MDouble))

    // we use a magic number for the default because we don't support optional values in generated DSL code (yet)
    static (ContinuousFeature) ("apply", Nil, (("default", MDouble, "unit(0.0209)"), ("min", MDouble, "math_ninf()"), ("max", MDouble, "math_inf()")) :: ContinuousFeature) implements allocates(ContinuousFeature, ${$0}, ${$1}, ${$2})

    //val ContinuousFeatureOps = withTpe(ContinuousFeature)
    //ContinuousFeatureOps {
    magic()
    @virtualize
    def magic[R]() = withTpee(ContinuousFeature){
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

    // FIXME: this produces an illegal ordering of effect error when array_fromseq is mutable or when using 'single'.
    // What happens with MHashMap in this case?
    static (DiscreteFeature) ("apply", Nil, (varArgs(MString) :: DiscreteFeature)) implements composite ${
      val keys = array_fromseq($0)
      val values = array_fromfunction(array_length(keys), i => i)
      discrete_feature_alloc(fhashmap_from_arrays(keys, values))
    }

    //val DiscreteFeatureOps = withTpe(DiscreteFeature)
    //DiscreteFeatureOps {
    magic2()
    @virtualize
    def magic2[R]() = withTpee(DiscreteFeature){
      compiler ("getFeatures") (Nil :: MHashMap(MString, MInt)) implements getter(0, "_features")

      infix ("apply") (MString :: MDouble) implements composite ${
        val featureMap = getFeatures($self)
        if (featureMap.contains($1)) featureMap($1).toDouble else 0.0
      }

      // convert this feature value into an indicator or dummy variable encoding
      infix ("indicator") (MString :: DenseVector(MDouble)) implements composite ${
        val featureMap = getFeatures($self)
        val numKeys = fhashmap_size(featureMap)
        val e = if (featureMap.contains($1)) featureMap($1).toDouble else 0.0
        (0::numKeys) { i => if (i == e) 1.0 else 0.0 }
      }

      infix ("size") (Nil :: MInt) implements composite ${
        val featureMap = getFeatures($self)
        fhashmap_size(featureMap)
      }
    }

    val BinaryFeature = tpe("BinaryFeature")
    data(BinaryFeature, ("_default", MBoolean))
    static (BinaryFeature) ("apply", Nil, ("default", MBoolean, "unit(false)") :: BinaryFeature) implements allocates(BinaryFeature, ${$0})

    //val BinaryFeatureOps = withTpe(BinaryFeature)
    //BinaryFeatureOps {
    magic3()
    @virtualize
    def magic3[R]() = withTpee(BinaryFeature){
      infix ("default") (Nil :: MBoolean) implements getter(0, "_default")

      infix ("apply") (MString :: MDouble) implements composite ${
        fassert($1 == "" || $1 == "0" || $1 == "1" || $1 == "false" || $1 == "true", "illegal input to binary feature")
        if ($1 == "0" || $1 == "false") 0.0
        else if ($1 == "1" || $1 == "true") 1.0
        else if ($self.default) 1.0
        else 0.0
      }
    }

    val DateFeature = tpe("DateFeature")
    val SDateFormat = tpe("org.joda.time.format.DateTimeFormatter")
    val SDateTime = tpe("org.joda.time.DateTime")
    primitiveTpePrefix ::= "org.joda"

    static (DateFeature) ("apply", Nil, MString :: SDateFormat) implements codegen($cala, ${
      org.joda.time.format.DateTimeFormat.forPattern($0)
    })

    infix (DateFeature) ("apply", Nil, (SDateFormat, MString) :: MDouble) implements codegen($cala, ${
      try {
        $0.parseMillis($1).toDouble
      }
      catch {
        case e => 0.0 // defaults to 1970, for better or worse... what else can we do?
      }
    })

    // Internal DateTime operations. These are factored into separate methods so we don't
    // need to create multiple DateTime instances at run-time and can still define our own API.

    compiler (DateFeature) ("dt_internal", Nil, MDouble :: SDateTime) implements codegen($cala, ${
      new org.joda.time.DateTime($0.toLong)
    })

    compiler (DateFeature) ("dt_internal_year", Nil, SDateTime :: MInt) implements codegen($cala, ${
      $0.getYear()
    })

    compiler (DateFeature) ("dt_internal_month", Nil, SDateTime :: MInt) implements codegen($cala, ${
      $0.getMonthOfYear()
    })

    compiler (DateFeature) ("dt_internal_hour", Nil, SDateTime :: MInt) implements codegen($cala, ${
      $0.getHourOfDay()
    })

    compiler (DateFeature) ("dt_internal_day", Nil, SDateTime :: MInt) implements codegen($cala, ${
      $0.getDayOfMonth()
    })

    compiler (DateFeature) ("dt_internal_weekday", Nil, SDateTime :: MInt) implements codegen($cala, ${
      $0.getDayOfWeek()
    })

    compiler (DateFeature) ("dt_internal_days_between", Nil, (SDateTime, SDateTime) :: MInt) implements codegen($cala, ${
      org.joda.time.Days.daysBetween($0.toLocalDate(), $1.toLocalDate()).getDays()
    })

    compiler (DateFeature) ("dt_internal_months_between", Nil, (SDateTime, SDateTime) :: MInt) implements codegen($cala, ${
      org.joda.time.Months.monthsBetween($0.toLocalDate(), $1.toLocalDate()).getMonths()
    })

    compiler (DateFeature) ("dt_internal_years_between", Nil, (SDateTime, SDateTime) :: MInt) implements codegen($cala, ${
      org.joda.time.Years.yearsBetween($0.toLocalDate(), $1.toLocalDate()).getYears()
    })

    // User-facing DateTime operations.

    static (DateFeature) ("year", Nil, MDouble :: MInt) implements composite ${
      val dt = dt_internal($0)
      dt_internal_year(dt)
    }

    static (DateFeature) ("month", Nil, MDouble :: MInt) implements composite ${
      val dt = dt_internal($0)
      dt_internal_month(dt)
    }

    static (DateFeature) ("hour", Nil, MDouble :: MInt) implements composite ${
      val dt = dt_internal($0)
      dt_internal_hour(dt)
    }

    static (DateFeature) ("day", Nil, MDouble :: MInt) implements composite ${
      val dt = dt_internal($0)
      dt_internal_day(dt)
    }

    // Monday is 1, Sunday is 7
    // http://joda-time.sourceforge.net/apidocs/org/joda/time/DateTimeConstants.html
    static (DateFeature) ("weekday", Nil, MDouble :: MInt) implements composite ${
      val dt = dt_internal($0)
      dt_internal_weekday(dt)
    }

    static (DateFeature) ("daysBetween", Nil, (MDouble, MDouble) :: MInt) implements composite ${
      dt_internal_days_between(dt_internal($0), dt_internal($1))
    }

    static (DateFeature) ("monthsBetween", Nil, (MDouble, MDouble) :: MInt) implements composite ${
      dt_internal_months_between(dt_internal($0), dt_internal($1))
    }

    static (DateFeature) ("yearsBetween", Nil, (MDouble, MDouble) :: MInt) implements composite ${
      dt_internal_years_between(dt_internal($0), dt_internal($1))
    }
  }

  def importFeatureHelperOps() {
    val FeatureHelper = grp("FeatureHelper")
    val DenseVector = lookupTpe("DenseVector")
    val Tup2 = lookupTpe("Tup2")

    /* Return a unique integer identifier for a given string */
    direct (FeatureHelper) ("unique", Nil, MString :: MInt) implements codegen($cala, ${
      MLGlobal.getId($0)
    })

    /* Lookup the string corresponding to a given unique integer identifier */
    direct (FeatureHelper) ("reverseUnique", Nil, MInt :: MString) implements codegen($cala, ${
      MLGlobal.lookupId($0)
    })

    // direct (FeatureHelper) ("getUniqueMappings", Nil, Nil :: DenseVector(Tup2(MInt,MString))) implements composite ${
    //   val names = get_unique_names_helper
    //   val ids = get_unique_ids_helper
    //   fassert(names.length == ids.length, "names and ids in unique map are different lengths")
    //   val data = array_fromfunction(names.length,  i => pack((ids(i), names(i))))
    //   densevector_fromarray(data, true)
    // }

    direct (FeatureHelper) ("getUniqueIds", Nil, Nil :: MArray(MInt)) implements codegen($cala, ${
      MLGlobal.getUniqueIds
    })

    direct (FeatureHelper) ("getUniqueNames", Nil, Nil :: MArray(MString)) implements codegen($cala, ${
      MLGlobal.getUniqueNames
    })

    direct (FeatureHelper) ("loadUniqueMappings", Nil, MString :: MUnit, effect = simple) implements codegen($cala, ${
      MLGlobal.loadUniqueMappings($0)
    })

    direct (FeatureHelper) ("dumpUniqueMappings", Nil, MString :: MUnit, effect = simple) implements codegen($cala, ${
      MLGlobal.dumpUniqueMappings($0)
    })
  }
}
