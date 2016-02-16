package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait MultiArrayMetadata { this: OptiMADSL =>

  def importMultiArrayMetadata() {
    val ArrayND = lookupTpe("ArrayND")
    val Array1D = lookupTpe("Array1D")
    val Array2D = lookupTpe("Array2D")
    val Array3D = lookupTpe("Array3D")

    //--------------------------
    //--- Metadata definitions
    //--------------------------

    // --- Implementation form (used for views and buffers)
    // TODO: Change to use enums?
    val ImplForm = metadata("ImplForm", ("v", SInt))
    meet (ImplForm) ${ if (this != that) ImplForm(1) else that }

    internal (ImplForm) ("NoImpl", Nil, Nil :: ImplForm) implements composite ${ ImplForm(0) }
    internal (ImplForm) ("PhysImpl", Nil, Nil :: ImplForm) implements composite ${ ImplForm(1) }
    internal (ImplForm) ("TrueImpl", Nil, Nil :: ImplForm) implements composite ${ ImplForm(2) }

    // --- MultiArray isBuffer
    val MBuffer = metadata("MBuffer", ("impl", ImplForm))
    meet (MBuffer) ${ MBuffer(meet(this.impl, that.impl)) }

    // --- MultiArray isView
    val MView = metadata("MView", ("impl", ImplForm))
    meet (MView) ${ MView(meet(this.impl, that.impl)) }

    for (MT <- List(MBuffer,MView)) {
      internal.infix (MT) ("isTrue", Nil, MT :: SBoolean) implements composite ${ $0.impl == TrueImpl }
      internal.infix (MT) ("isPhys", Nil, MT :: SBoolean) implements composite ${ $0.impl == PhysImpl }
    }

    // --- MultiArray rank
    val MRank = metadata("MRank", ("rank", SInt))
    val rank = grp("rank")
    meet (MRank) ${ MRank(this.rank) }
    canMeet (MRank) ${ this.rank == that.rank }

    // --- MultiArray may be updated
    val MayUpdate = metadata("MayUpdate", ("mayUpdate", SBoolean))
    meet (MayUpdate) ${ MayUpdate(this.mayUpdate || that.mayUpdate) }

    // TODO: Bit annoying to specify both versions - better way to generate both from one?
    for (T <- List(SymProps, MAny)) {
      internal (MView) ("getView", Nil, T :: SOption(MView)) implements composite ${ meta[MView]($0) }
      internal (MView) ("isPhysView", Nil, T :: SBoolean) implements composite ${ getView($0).map{_.isPhys}.getOrElse(false) }
      internal (MView) ("isTrueView", Nil, T :: SBoolean) implements composite ${ getView($0).map{_.isTrue}.getOrElse(false) }

      internal (MBuffer) ("getBuffer", Nil, T :: SOption(MBuffer)) implements composite ${ meta[MBuffer]($0) }
      internal (MBuffer) ("isPhysBuffer", Nil, T :: SBoolean) implements composite ${ getBuffer($0).map{_.isPhys}.getOrElse(false) }
      internal (MBuffer) ("isTrueBuffer", Nil, T :: SBoolean) implements composite ${ getBuffer($0).map{_.isTrue}.getOrElse(false) }

      internal (MRank) ("getRank", Nil, T :: SOption(MRank)) implements composite ${ meta[MRank]($0) }
      // Note rank() will throw an exception if used before all ranks have been assigned!
      internal.static (rank) ("apply", Nil, T :: SInt) implements composite ${
        getLayout($0).map(_.rank).getOrElse(getRank($0).get.rank)
      }

      internal (MayUpdate) ("getMayUpdate", Nil, T :: SOption(MayUpdate)) implements composite ${ meta[MayUpdate]($0) }
      internal (MayUpdate) ("mayUpdate", Nil, T :: SBoolean) implements composite ${ getMayUpdate($0).map{_.mayUpdate}.getOrElse(false) }
    }

    internal.static (rank) ("update", Nil, (MAny, SInt) :: MUnit, effect = simple) implements composite ${ setMetadata($0, MRank($1)) }
    internal.static (rank) ("update", Nil, (MAny, SOption(MRank)) :: MUnit, effect = simple) implements composite ${ setMetadata($0, $1) }

    defaultMetadata(MArray) ${ MRank(1) }
    defaultMetadata(Array1D) ${ MRank(1) }
    defaultMetadata(Array2D) ${ MRank(2) }
    defaultMetadata(Array3D) ${ MRank(3) }

    // --- MultiArray layout
    val LayoutType = tpe("LayoutType", stage=compile)
    identifier (LayoutType) ("Flat")

    val LayoutSubtype = tpe("LayoutSubtype", stage=compile)
    identifier (LayoutSubtype) ("Plain")
    identifier (LayoutSubtype) ("View")
    identifier (LayoutSubtype) ("Buffer")
    identifier (LayoutSubtype) ("BuffView")

    val MLayout = metadata("MLayout", ("rank", SInt), ("tpe", LayoutType), ("subtpe", LayoutSubtype))
    meet (MLayout) ${ that }
    canMeet (MLayout) ${ this == that }

    internal.infix (MLayout) ("isView", Nil, MLayout :: SBoolean) implements composite ${ $0.subtpe == View || $0.subtpe == BuffView }

    val layout = grp("layout")

    for (T <- List(SymProps, MAny)) {
      internal (MLayout) ("getLayout", Nil, T :: SOption(MLayout)) implements composite ${ meta[MLayout]($0) }
      internal.static (layout) ("apply", Nil, T :: MLayout) implements composite ${ getLayout($0).get }
    }

    internal.static (layout) ("update", Nil, (MAny, MLayout) :: MUnit, effect = simple) implements composite ${ setMetadata($0, $1) }
    internal.static (layout) ("update", Nil, (MAny, SOption(MLayout)) :: MUnit, effect = simple) implements composite ${ setMetadata($0, $1) }
  }
}