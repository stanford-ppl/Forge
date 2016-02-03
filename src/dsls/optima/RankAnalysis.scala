package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait RankAnalysis { this: OptiMADSL =>

  def importRankAnalysis() {
    val RankAnalyzer = analyzer("Rank")

    val ArrayND = lookupTpe("ArrayND")
    val Array1D = lookupTpe("Array1D")
    val Array2D = lookupTpe("Array2D")
    val Array3D = lookupTpe("Array3D")

    // --- Metadata definitions
    val ImplForm = metadata("ImplForm", ("v", SInt))
    meet (ImplForm) ${ if (this != that) PhysOnly else that }
    compiler (ImplForm) ("NoImpl", Nil, Nil :: ImplForm) implements composite ${ ImplForm(0) }
    compiler (ImplForm) ("PhysOnly", Nil, Nil :: ImplForm) implements composite ${ ImplForm(1) }
    compiler (ImplForm) ("TrueImpl", Nil, Nil :: ImplForm) implements composite ${ ImplForm(2) }

    val MBuffer = metadata("MBuffer", ("impl", ImplForm))
    meet (MBuffer) ${ MBuffer(meet(this.impl, that.impl)) }

    val MView = metadata("MView", ("impl", ImplForm))
    meet (MView) ${ MView(meet(this.impl, that.impl)) }

    for (MT <- List(MBuffer,MView)) {
      compiler.infix (MT) ("isTrue", Nil, MT :: SBoolean) implements composite ${ $0.impl == TrueImpl }
      compiler.infix (MT) ("isPhys", Nil, MT :: SBoolean) implements composite ${ $0.impl == PhysImpl }
    }

    // MultiArray rank
    val Rank = metadata("MRank", ("rank", SInt))
    val rank = grp("rank")
    meet (Rank) ${ MRank(this.rank) }

    // MultiArray may be updated
    val MayUpdate = metadata("MayUpdate", ("mayUpdate", SBoolean))
    meet (MayUpdate) ${ MayUpdate(this.mayUpdate || that.mayUpdate) }

    // TODO: Bit annoying to specify both versions - better way to generate both from one?
    for (T <- List(SymProps, MAny)) {
      compiler (MView) ("getView", Nil, T :: SOption(MView)) implements composite ${ meta[MView]($0) }
      compiler (MView) ("isPhysView", Nil, T :: SBoolean) implements composite ${ getView($0).map{_.isPhys}.getOrElse(false) }
      compiler (MView) ("isTrueView", Nil, T :: SBoolean) implements composite ${ getView($0).map{_.isTrue}.getOrElse(false) }

      compiler (MBuffer) ("getBuffer", Nil, T :: SOption(MBuffer)) implements composite ${ meta[MBuffer]($0) }
      compiler (MBuffer) ("isPhysBuffer", Nil, T :: SBoolean) implements composite ${ getBuffer($0).map{_.isPhys}.getOrElse(false) }
      compiler (MBuffer) ("isTrueBuffer", Nil, T :: SBoolean) implements composite ${ getBuffer($0).map{_.isTrue}.getOrElse(false) }

      compiler (Rank) ("getRank", Nil, T :: SOption(Rank)) implements composite ${ meta[MRank]($0) }
      // Note rank() will throw an exception if used before all ranks have been assigned!
      compiler.static (rank) ("apply", Nil, T :: Rank) implements composite ${ getRank($0).get.rank }

      compiler (MayUpdate) ("getMayUpdate", Nil, T :: SOption(MayUpdate)) implements composite ${ meta[MayUpdate]($0) }
      compiler (MayUpdate) ("mayUpdate", Nil, T :: SBoolean) implements composite ${ getMayUpdate($0).map{_.mayUpdate}.getOrElse(false) }
    }

    compiler.static (rank) ("update", Nil, (MAny, SInt) :: MUnit) implements composite ${ setMetadata($0, MRank($1)) }


    // TODO: Better way to have pointers to these ops?
    val multia_new = lookupOp(ArrayND, "multia_new")

    // Testing analysis scope - not working yet
    /*val RankAnalysisRules = withAnalyzer(RankAnalyzer)
    RankAnalysisRules {
      analyze (multia_new) using rule ${ rank(lhs) = $0.length }
    }*/
  }


}