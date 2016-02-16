package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

// TODO:
// - Autodoc
// - Forge errors (source context is useless right now)
// - Completeness checks for analyses
// - Convergence conditions for analyses
// - Pre- and Post-processing rules for analyses
// - Library version of Blocks? How to handle transformation rules for these?
// - figment should operate more like a redirect for library implementation
// - separate metadata meet, etc. functions into separate functions generated in Impls
// - generate atomic writes from Forge
// - Why are if-then-else statements being staged even when the condition is a Scala constant?

// TEST:

// DONE:
// - disable error with field shortcutting on figment types (new rule for unapply on structs for figments)
// - Fix type arguments in creating RefinedManifest
// - allow and generate inheritance for any type with no data structure definition
// - Move FigmentStruct to extern (Records)
// - Add option to disable struct unwrapping in LMS
// - Add new sugar for modules in DADL

object OptiMADSLRunner extends ForgeApplicationRunner with OptiMADSL
trait OptiMADSL extends ForgeApplication with MultiArrays with MultiArrayImpls with MultiArrayMetadata
  with ArrayLowering with MultiArrayAnalysis with RangeOps {

  def dslName = "OptiMA"
  override def clearTraversals = true

  def specification() = {

    // our selection of Scala ops
    // we don't use Numeric or Fractional, since they are replaced by Arith
    importStrings()
    importMisc()
    importPrimitives()
    //importCasts()
    //importOrdering()
    //importMath()
    //importTuples()
    //noInfixList :::= List("toInt", "toFloat", "toDouble", "toLong")

    importRanges()

    // MultiArray figment types (with subtyping)
    val T = tpePar("T")
    val ArrayND = tpe("ArrayND", T)
    val Array1D = tpe("Array1D", T) augments ArrayND
    val Array2D = tpe("Array2D", T) augments ArrayND
    val Array3D = tpe("Array3D", T) augments ArrayND

    val Indices = figmentTpe("Indices")
    val LoopIndices = figmentTpe("LoopIndices") augments Indices

    val ImplND  = tpe("ImplND", T) augments ArrayND
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

    schedule(IRPrinter)
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
