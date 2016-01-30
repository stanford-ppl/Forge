package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiMADSLRunner extends ForgeApplicationRunner with OptiMADSL
trait OptiMADSL extends ForgeApplication with MultiArrayOps with VisibilityTestOps {

  def dslName = "OptiMA"
  override def clearTraversals = true

  def specification() = {
    // our selection of Scala ops
    // we don't use Numeric or Fractional, since they are replaced by Arith
    //importMisc()
    //importPrimitives()
    //importCasts()
    //importOrdering()
    //importStrings()
    //importMath()
    //importTuples()
    //importIndexingOps()
    //noInfixList :::= List("toInt", "toFloat", "toDouble", "toLong")

    importVisibilityTest()
    importVisibilityTest2()

    /*
    importHashMap()
    importConcurrentHashMap()*/

    // MultiArray figment types (with subtyping)
    /*val T = tpePar("T")
    val ArrayND = figmentTpe("ArrayND", T)
    val Array1D = figmentTpe("Array1D", T) isA ArrayND
    val Array2D = figmentTpe("Array2D", T) isA ArrayND
    val Array3D = figmentTpe("Array3D", T) isA ArrayND*/

    //importMultiArrayOps()

    //val RankAnalyzer = analyzer("Rank")
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

    // Experimental metadata (move later)
    //val RankMetadata = metadata("Rank")

    // rewrites
    //extern(grp("Rewrite"), targets = Nil)
    //extern(grp("Distributed"), targets = List($cala))
  }
}
