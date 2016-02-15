package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

// TODO:
// - Completeness checks for analyses
// - Convergence conditions for analyses
// - Pre- and Post-processing rules for analyses
// - Blocks?
// - figment should operate more like a redirect for library implementation
// - allow and generate inheritance for any type with no data structure definition (w/o datastruct defs)
// - add inheritance for identifiers (for enums)
// - separate metadata meet, etc. functions into separate functions generated in Impls

object OptiMADSLRunner extends ForgeApplicationRunner with OptiMADSL
trait OptiMADSL extends ForgeApplication with MultiArrays with MultiArrayImpls with MultiArrayMetadata
  with ArrayLowering with MultiArrayAnalysis with RangeOps {

  def dslName = "OptiMA"
  override def clearTraversals = true

  def specification() = {
    // our selection of Scala ops
    // we don't use Numeric or Fractional, since they are replaced by Arith
    //importMisc()
    importPrimitives()
    //importCasts()
    //importOrdering()
    //importStrings()
    //importMath()
    //importTuples()
    //importIndexingOps()
    //noInfixList :::= List("toInt", "toFloat", "toDouble", "toLong")

    importRanges()

    // MultiArray figment types (with subtyping)
    val T = tpePar("T")
    val ArrayND = figmentTpe("ArrayND", T)
    val Array1D = figmentTpe("Array1D", T) augments ArrayND
    val Array2D = figmentTpe("Array2D", T) augments ArrayND
    val Array3D = figmentTpe("Array3D", T) augments ArrayND

    val Indices = figmentTpe("Indices")
    val LoopIndices = figmentTpe("LoopIndices") augments Indices

    val ImplND  = figmentTpe("ImplND", T) augments ArrayND
    val FlatND  = figmentTpe("FlatND", T) augments ImplND

    importIndices()

    importMultiArrayMetadata()
    importMultiArrays()
    importMultiArrayImpls()
    //importRankAnalysis()
    //importArrayLowering()

    val RankAnalyzer = analyzer("Rank")
    //val RankChecker  = analyzer("RankCheck")
    //val ArrayWrapper = transformer("ArrayWrapper", isExtern=true)
    //val LayoutAnalyzer = analyzer("LayoutAnalyzer")
    val ArrayLowering = transformer("ArrayLowering")

    //schedule(RankAnalyzer)
    //schedule(RankChecker)
    //schedule(ArrayWrapper)
    //schedule(LayoutAnalyzer)
    //schedule(ArrayLowering)
    //schedule(MultiloopSoA)

    // rewrites
    //extern(grp("Rewrite"), targets = Nil)
    //extern(grp("Distributed"), targets = List($cala))
  }
}
