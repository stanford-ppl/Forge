package ppl.dsl.forge
package dsls
package dhdl

trait DHDLSugar {
  this: DHDLDSL =>

  def importSugar() {
    importIndices()
    importRanges()
    importLoopRanges()
  }

  def importIndices() {
    val Indices = lookupTpe("Indices")
    val Idx     = lookupAlias("SInt")

    internal (Indices) ("indices_new", Nil, SList(Idx) :: Indices) implements record(Indices, ("i", SList(Idx), quotedArg(0)))
    internal (Indices) ("indices_create", Nil, SList(Idx) :: Indices) implements composite ${
      val inds = indices_new($0)
      sizeOf(inds) = $0.length
      inds
    }
    direct (Indices) ("getIndex", Nil, (Indices, SInt) :: Idx) implements composite ${ field[SInt]($0, "i_" + $1) }

    static (Indices) ("apply", Nil, varArgs(Idx) :: Indices) implements composite ${ indices_create($0.toList) }
    infix (Indices) ("apply", Nil, (Indices, SInt) :: Idx) implements redirect ${ getIndex($0, $1) }
    internal.infix (Indices) ("toList", Nil, Indices :: SList(Idx)) implements composite ${ List.tabulate(sizeOf($0)){i => $0(i)} }
  }


  def importRanges() {
    val Range = lookupTpe("Range")
    val Idx   = lookupAlias("SInt")
    val FixPt = lookupTpe("FixPt")

    data(Range, ("_start", Idx), ("_end", Idx), ("_len", Idx))
    internal.infix (Range) ("start", Nil, Range :: Idx) implements getter(0, "_start")
    internal.infix (Range) ("end", Nil, Range :: Idx) implements getter(0, "_end")
    internal.infix (Range) ("len", Nil, Range :: Idx) implements getter(0, "_len")

    internal (Range) ("range_new", Nil, (Idx, Idx, Idx) :: Range) implements allocates(Range, ${$0}, ${$1}, ${$2})
    infix (Range) ("::", Nil, (Idx, Idx) :: Range) implements composite ${
      val range = range_new($1, $0, $0 - $1)
      isUnit(range) = false
      range
    }

    internal (Range) ("unitRange", Nil, Idx :: Range) implements composite ${
      val range = range_new($0, $0 + 1.as[SInt], 1.as[SInt])
      isUnit(range) = true
      range
    }

    // Causes scalac to get stuck in implicit lookup :(
    /*fimplicit (Range) ("idx_to_range", Nil, Idx :: Range) implements composite ${
      val range = range_new($0, $0 + 1, 1)
      isUnit(range) = true
      range
    }*/
    // Causes compiler to have a hard time figuring out how to lift Ints... Fortunately not used too often
    /*fimplicit (Range) ("int_to_range", Nil, SInt :: Range) implements composite ${
      val range = range_new($0, $0 + 1, 1)
      isUnit(range) = true
      range
    }*/
  }

  // Need LoopRange for syntax like a until by c - need to be able to access start/end (could also use metadata...)
  def importLoopRanges() {
    val Counter   = lookupTpe("Counter")
    val LoopRange = lookupTpe("LoopRange")
    val Idx       = lookupAlias("SInt")

    data(LoopRange, ("_start", Idx), ("_end", Idx), ("_step", Idx))
    internal.infix (LoopRange) ("start", Nil, LoopRange :: Idx) implements getter(0, "_start")
    internal.infix (LoopRange) ("end", Nil, LoopRange :: Idx) implements getter(0, "_end")
    internal.infix (LoopRange) ("step", Nil, LoopRange :: Idx) implements getter(0, "_step")
    static (LoopRange) ("apply", Nil, (Idx,Idx,Idx) :: LoopRange) implements allocates(LoopRange, ${$0},${$1},${$2})

    fimplicit (LoopRange) ("rangeToCounter", Nil, LoopRange :: Counter) implements composite ${ counter_create(None, $0.start, $0.end, $0.step, 1) }

    // --- API
    infix (LoopRange) ("until", Nil, (Idx,Idx) :: LoopRange) implements composite ${ LoopRange($0, $1, fixPt[Int,Signed,B32,B0](1)) }
    infix (LoopRange) ("by", Nil, (LoopRange, Idx) :: LoopRange) implements composite ${ LoopRange($0.start, $0.end, $1) }
    infix (LoopRange) ("by", Nil, (Idx, Idx) :: LoopRange) implements composite ${ LoopRange(fixPt[Int,Signed,B32,B0](0), $0, $1) }
  }

}
