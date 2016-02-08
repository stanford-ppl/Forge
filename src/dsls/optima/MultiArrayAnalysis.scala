package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait MultiArrayAnalysis { this: OptiMADSL =>

  def importMultiArrayAnalysis() {
    importRankAnalysis()
    importLayoutAnalysis()
  }

  def importRankAnalysis() {
    val RankAnalyzer = analyzer("Rank")

    val ArrayND = lookupTpe("ArrayND")
    val Array1D = lookupTpe("Array1D")
    val Array2D = lookupTpe("Array2D")
    val Array3D = lookupTpe("Array3D")

    // --- Metadata definitions
    val ImplForm = metadata("ImplForm", ("v", SInt))
    meet (ImplForm) ${ if (this != that) PhysImpl else that }
    compiler (ImplForm) ("NoImpl", Nil, Nil :: ImplForm) implements composite ${ ImplForm(0) }
    compiler (ImplForm) ("PhysImpl", Nil, Nil :: ImplForm) implements composite ${ ImplForm(1) }
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
    canMeet (Rank) ${ this.rank == that.rank }

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
      compiler.static (rank) ("apply", Nil, T :: SInt) implements composite ${
        getLayout($0).map(_.rank).getOrElse(getRank($0).get.rank)
      }

      compiler (MayUpdate) ("getMayUpdate", Nil, T :: SOption(MayUpdate)) implements composite ${ meta[MayUpdate]($0) }
      compiler (MayUpdate) ("mayUpdate", Nil, T :: SBoolean) implements composite ${ getMayUpdate($0).map{_.mayUpdate}.getOrElse(false) }
    }

    compiler.static (rank) ("update", Nil, (MAny, SInt) :: MUnit, effect = simple) implements composite ${ setMetadata($0, MRank($1)) }
    compiler.static (rank) ("update", Nil, (MAny, SOption(Rank)) :: MUnit, effect = simple) implements composite ${ setMetadata($0, $1) }

    defaultMetadata(MArray) ${ MRank(1) }
    defaultMetadata(Array1D) ${ MRank(1) }
    defaultMetadata(Array2D) ${ MRank(2) }
    defaultMetadata(Array3D) ${ MRank(3) }

    // Testing analysis scope - not working yet
    val RankAnalysisRules = withAnalyzer(RankAnalyzer)
    RankAnalysisRules {
      analyze (ArrayND, "multia_new") using rule ${ rank(lhs) = $0.length }

      for (i <- 1 to 3) {
        analyze (ArrayND, "multia_as_"+i+"d") using rule ${
          if (getRank($0).isDefined) {
            assert(rank($0) == \$i, "Cannot cast " + rank($0) + "D array to \${i}D array")
            rank(lhs) = rank($0)
          }
        }
      }
    }

  }

  def importLayoutAnalysis() {
    val Layout = metadata("Layout", ("rank", SInt), ("subtype", SInt), ("layout", SInt))
    meet (Layout) ${ that }
    canMeet (Layout) ${ this == that }

    compiler (Layout) ("Plain", Nil, Nil :: SInt) implements composite ${ 0 }
    compiler (Layout) ("View", Nil, Nil :: SInt) implements composite ${ 1 }
    compiler (Layout) ("Buffer", Nil, Nil :: SInt) implements composite ${ 2 }
    compiler (Layout) ("BuffView", Nil, Nil :: SInt) implements composite ${ 3 }

    val FlatLayout = grp("FlatLayout")
    compiler.static (FlatLayout) ("apply", Nil, (SInt,SInt) :: Layout) implements composite ${ Layout($0, $1, 0) }
    compiler.static (FlatLayout) ("unapply", Nil, Layout :: SOption(CTuple2(SInt,SInt))) implements composite ${
      if ($0.subtype == 0) Some(($0.rank, $0.subtype)) else None
    }

    for (T <- List(SymProps, MAny)) {
      compiler (Layout) ("getLayout", Nil, T :: SOption(Layout)) implements composite ${ meta[Layout]($0) }
      compiler (Layout) ("layout", Nil, T :: Layout) implements composite ${ getLayout($0).get }
    }

  }

}