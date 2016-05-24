package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait RangeOps { this: OptiMADSL =>

  def importRanges() {
    // --- Ranges
    // TODO: No 3 input user-facing static Range function for now since it's a bit unclear what the natural
    // ordering of end/stride is.

    val Range = tpe("Range")
    data(Range, ("_start", MInt), ("_end", MInt), ("_stride", MInt), ("_len", MInt))
    internal.infix (Range) ("start", Nil, Range :: MInt) implements getter(0, "_start")
    internal.infix (Range) ("end", Nil, Range :: MInt) implements getter(0, "_end")
    internal.infix (Range) ("stride", Nil, Range :: MInt) implements getter(0, "_stride")
    internal.infix (Range) ("len", Nil, Range :: MInt) implements getter(0, "_len")

    internal (Range) ("range_new", Nil, (MInt,MInt,MInt,MInt) :: Range) implements allocates(Range, ${$0},${$1},${$2},${$3})
    static (Range) ("apply", Nil, (MInt,MInt) :: Range) implements composite ${ range_new($0, $1, unit(1), $1 - $0) }

    infix (Range) ("@@", Nil, (MInt, MInt) :: Range) implements composite ${ range_new($1, $1 + $0, 1, $0) }
    infix (Range) ("::", Nil, (MInt, MInt) :: Range) implements composite ${ range_new($1, $0, 1, $0 - $1) }
    infix (Range) ("::", Nil, (Range, MInt) :: Range) implements composite ${ range_new($1, $0.end, $0.start, ($0.end - $1) / $0.start) }
    noSourceContextList ::= "::"



    // --- Loop ranges (internal use only)
    val LoopRange = tpe("LoopRange")
    data(LoopRange, ("_start", MInt), ("_end", MInt), ("_stride", MInt))
    internal.infix (LoopRange) ("start", Nil, LoopRange :: MInt) implements getter(0, "_start")
    internal.infix (LoopRange) ("end", Nil, LoopRange :: MInt) implements getter(0, "_end")
    internal.infix (LoopRange) ("stride", Nil, LoopRange :: MInt) implements getter(0, "_stride")
    internal.static (LoopRange) ("apply", Nil, (MInt,MInt,MInt) :: LoopRange) implements allocates(LoopRange, ${$0},${$1},${$2})

    internal.infix (LoopRange) ("to", Nil, (MInt, MInt) :: LoopRange) implements composite ${ LoopRange($0, $1+1, 1) }
    internal.infix (LoopRange) ("until", Nil, (MInt,MInt) :: LoopRange) implements composite ${ LoopRange($0, $1, 1) }
    internal.infix (LoopRange) ("by", Nil, (LoopRange, MInt) :: LoopRange) implements composite ${ LoopRange($0.start, $0.end, $1) }
    internal.infix (LoopRange) ("foreach", Nil, (LoopRange, MInt ==> MUnit) :: MUnit) implements composite ${ seqloop($0.start,$0.end,$0.stride,$1) }

    // Extract fields to avoid having LoopRanges codegened as much as possible (and since codegen nodes can't have struct inputs anyway)
    val seqloop = internal (LoopRange) ("seqloop", Nil, (("start",MInt),("end",MInt),("stride",MInt),("func",MInt ==> MUnit)) :: MUnit)

    impl (seqloop) (codegen($cala, ${
      var i = $start
      while (i < $end) {
        $b[func](i)
        i += $stride
      }
    }))

    impl (seqloop) (codegen(cpp, ${
      for(int i=$start ; i<$end ; i+=$stride) {
        $b[func](i)
      }
    }))
  }
}