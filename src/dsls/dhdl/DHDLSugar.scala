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
    val Idx     = lookupAlias("Index")

    internal (Indices) ("indices_new", Nil, SList(Idx) :: Indices) implements record(Indices, ("i", SList(Idx), quotedArg(0)))
    internal (Indices) ("indices_create", Nil, SList(Idx) :: Indices) implements composite ${
      val inds = indices_new($0)
      lenOf(inds) = $0.length
      inds
    }
    /** @nodoc **/
    direct (Indices) ("indices_get_index", Nil, (Indices, SInt) :: Idx) implements composite ${ field[SInt]($0, "i_" + $1) }
    /** @nodoc **/
    direct (Indices) ("indices_to_list", Nil, Indices :: SList(Idx)) implements composite ${ List.tabulate(lenOf($0)){i => $0(i)} }

    /** @nodoc **/
    static (Indices) ("apply", Nil, varArgs(Idx) :: Indices) implements composite ${ indices_create($0.toList) }
    infix (Indices) ("apply", Nil, (Indices, SInt) :: Idx) implements redirect ${ indices_get_index($0, $1) }
    internal.infix (Indices) ("toList", Nil, Indices :: SList(Idx)) implements redirect ${ indices_to_list($0) }
  }


  def importRanges() {
    val Range = lookupTpe("Range")
    val Idx   = lookupAlias("Index")
    val FixPt = lookupTpe("FixPt")

    data(Range, ("_start", Idx), ("_end", Idx), ("_len", Idx))
    internal.infix (Range) ("start", Nil, Range :: Idx) implements getter(0, "_start")
    internal.infix (Range) ("end", Nil, Range :: Idx) implements getter(0, "_end")
    internal.infix (Range) ("len", Nil, Range :: Idx) implements getter(0, "_len")

    internal (Range) ("range_new", Nil, (Idx, Idx, Idx) :: Range) implements allocates(Range, ${$0}, ${$1}, ${$2})
    /** Creates a range with specified start (inclusive) and end (noninclusive)
     * @param end
     * @param start
     **/
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

    // Causes scalac to get stuck in implicit lookup?
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
    val Idx       = lookupAlias("Index")

    data(LoopRange, ("_start", Idx), ("_end", Idx), ("_step", Idx))
    internal.infix (LoopRange) ("start", Nil, LoopRange :: Idx) implements getter(0, "_start")
    internal.infix (LoopRange) ("end", Nil, LoopRange :: Idx) implements getter(0, "_end")
    internal.infix (LoopRange) ("step", Nil, LoopRange :: Idx) implements getter(0, "_step")
    /** @nodoc **/
    static (LoopRange) ("apply", Nil, (Idx,Idx,Idx) :: LoopRange) implements allocates(LoopRange, ${$0},${$1},${$2})

    fimplicit (LoopRange) ("rangeToCounter", Nil, LoopRange :: Counter) implements composite ${
      counter_create(None, $0.start, $0.end, $0.step, unit(1))
    }

    // --- API
    /** Creates a LoopRange with specified start (inclusive) and end (noninclusive) and step 1
     * @param start
     * @param end
     **/
    infix (LoopRange) ("until", Nil, (Idx,Idx) :: LoopRange) implements composite ${ LoopRange($0, $1, fixPt[Int,Signed,B32,B0](1)) }
    /** Changes given LoopRange's step to specified value
     * @param step
     **/
    infix (LoopRange) ("by", Nil, (LoopRange, Idx) :: LoopRange) implements composite ${ LoopRange($0.start, $0.end, $1) }
    /** Creates a LoopRange with start of 0 (inclusive), specified end (noninclusive) and step
     * @param end
     * @param step
     **/
    infix (LoopRange) ("by", Nil, (Idx, Idx) :: LoopRange) implements composite ${ LoopRange(fixPt[Int,Signed,B32,B0](0), $0, $1) }
    /** @nodoc - syntax TBD **/
    infix (LoopRange) ("par", Nil, (Idx, MInt) :: Counter) implements composite ${
      counter_create(None, fixPt[Int,Signed,B32,B0](0), $0, fixPt[Int,Signed,B32,B0](1), $1)
    }
    /** @nodoc - syntax TBD **/
    infix (LoopRange) ("par", Nil, (LoopRange, MInt) :: Counter) implements composite ${
      counter_create(None, $0.start, $0.end, $0.step, $1)
    }
  }

}
