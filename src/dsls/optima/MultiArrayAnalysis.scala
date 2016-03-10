package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait MultiArrayAnalysis { this: OptiMADSL =>

  def importRankAnalysis() {
    val RankAnalyzer = analyzer("MRank")
    val ArrayND = lookupTpe("ArrayND")
    val Array1D = lookupTpe("Array1D")
    val Array2D = lookupTpe("Array2D")
    val Array3D = lookupTpe("Array3D")

    val RankAnalysisRules = withAnalyzer(RankAnalyzer)
    RankAnalysisRules {
      analyze (ArrayND, "ma_new") using rule ${ rank(lhs) = $0.length }
      analyze (ArrayND, "ma_new_immutable") using rule ${ rank(lhs) = $0.length }
    }
  }
}