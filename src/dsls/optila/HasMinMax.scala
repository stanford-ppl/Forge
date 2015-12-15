package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Defines the HasMinMax type class for scalars
 */
trait HasMinMaxOps {
  this: OptiLADSL =>

  object THasMinMax extends TypeClassSignature {
    def name = "HasMinMax"
    def prefix = "_hmm"
    def wrapper = Some("HMMtype")
  }

  def importHasMinMaxOps() {
    val T = tpePar("T")

    val HasMinMax = tpeClass("HasMinMax", THasMinMax, T)

    // HasMinMax type class interface
    infix (HasMinMax) ("min", T, Nil :: T)
    infix (HasMinMax) ("max", T, Nil :: T)

    // primitive implementations
    val DoubleHasMinMax = tpeClassInst("HasMinMaxDouble", Nil, HasMinMax(MDouble))
    infix (DoubleHasMinMax) ("min", Nil, Nil :: MDouble) implements composite ${ unit(scala.Double.MinValue) }
    infix (DoubleHasMinMax) ("max", Nil, Nil :: MDouble) implements composite ${ unit(scala.Double.MaxValue) }

    val FloatHasMinMax = tpeClassInst("HasMinMaxFloat", Nil, HasMinMax(MFloat))
    infix (FloatHasMinMax) ("min", Nil, Nil :: MFloat) implements composite ${ unit(scala.Float.MinValue) }
    infix (FloatHasMinMax) ("max", Nil, Nil :: MFloat) implements composite ${ unit(scala.Float.MaxValue) }

    val IntHasMinMax = tpeClassInst("HasMinMaxInt", Nil, HasMinMax(MInt))
    infix (IntHasMinMax) ("min", Nil, Nil :: MInt) implements composite ${ unit(scala.Int.MinValue) }
    infix (IntHasMinMax) ("max", Nil, Nil :: MInt) implements composite ${ unit(scala.Int.MaxValue) }

    val LongHasMinMax = tpeClassInst("HasMinMaxLong", Nil, HasMinMax(MLong))
    infix (LongHasMinMax) ("min", Nil, Nil :: MLong) implements composite ${ unit(scala.Long.MinValue) }
    infix (LongHasMinMax) ("max", Nil, Nil :: MLong) implements composite ${ unit(scala.Long.MaxValue) }
  }
}
