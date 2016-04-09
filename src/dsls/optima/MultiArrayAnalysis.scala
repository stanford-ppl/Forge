package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait MultiArrayAnalysis { this: OptiMADSL =>

  def importMultiArrayAnalyses() {
    importRankAnalysis()
    importLayoutAnalysis()
  }

  def importRankAnalysis() {
    val RankAnalyzer = analyzer("Rank")
    val ArrayND = lookupTpe("ArrayND")
    val Array1D = lookupTpe("Array1D")
    val Array2D = lookupTpe("Array2D")
    val Array3D = lookupTpe("Array3D")

    val RankAnalysisRules = withAnalyzer(RankAnalyzer)
    RankAnalysisRules {
      analyze (ArrayND, "ma_new") using rule ${ rank(lhs) = $0.length }
      analyze (ArrayND, "ma_new_immutable") using rule ${ rank(lhs) = $0.length }
      analyze (ArrayND, "ma_view") using rule ${
        rank(lhs) = $1.length - $2.length
        enableView(lhs)
        if (isPhysBuffer($0)) setMetadata(lhs, MBuffer(Form(Partial_3)))
      }
      analyze (ArrayND, "ma_update") using rule ${ setUpdated($self) }

    }
  }

  def importLayoutAnalysis() {
    // TODO: Can this be described in Forge, or does this need to stay in extern?
  }

}