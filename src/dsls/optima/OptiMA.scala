package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

// TODO:
// - Misc. operations for analysis (e.g. convergence messages, postprocessing)
//   - Don't want this to just be writing Delite code in Forge box, but what else can we do here?
// - Should be able to access internal direct methods (and others?) from allocates, etc.
//   - Option 1: Redirection in the library implementation for allocates
//   - Option 2: Add another trait so Wrapper can view LibraryOps without viewing lifts

object OptiMADSLRunner extends ForgeApplicationRunner with OptiMADSL
trait OptiMADSL extends ForgeApplication with MultiArrayOps with RankAnalysis with VisibilityTestOps {

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

    importVisibilityTest()

    // MultiArray figment types (with subtyping)
    val T = tpePar("T")
    val ArrayND = figmentTpe("ArrayND", T)
    val Array1D = figmentTpe("Array1D", T) isA ArrayND
    val Array2D = figmentTpe("Array2D", T) isA ArrayND
    val Array3D = figmentTpe("Array3D", T) isA ArrayND

    importMultiArrayOps()
    importRankAnalysis()

    val RankAnalyzer = analyzer("Rank")

    //val RankChecker  = analyzer("RankCheck")
    //val ArrayWrapper = transformer("ArrayWrapper", isExtern=true)
    //val LayoutAnalyzer = analyzer("LayoutAnalyzer")
    //val ArrayLowering = transformer("ArrayLowering")

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
