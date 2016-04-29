package ppl.dsl.forge
package dsls
package optiwrangler

import core.{ForgeApplication,ForgeApplicationRunner}

trait RangeOps {
  this: OptiWranglerDSL =>

  def importRangeOps() {
  	val A = tpePar("A")
  	val Range = tpe("Range")
  	data (Range, "start" -> MInt, "end" -> MInt)
	static (Range) ("apply", Nil, (MInt, MInt) :: Range) implements allocates (Range, ${$0}, ${$1})

	val RangeOps = withTpe (Range)
	RangeOps {
	  internal ("startIndex") (Nil :: MInt) implements getter (0, "start")
	  internal ("endIndex") (Nil :: MInt) implements getter (0, "end")
	  internal ("length") (Nil :: MInt) implements single ${endIndex($self) - startIndex($self)}
	  internal ("getElement") (MInt :: MInt) implements single ${$1 + startIndex($self)}

	  internal ("range_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("Range cannot be allocated from a parallel op") }
	  internal ("range_illegalupdate") ((MInt,MInt) :: MNothing, effect = simple) implements composite ${ fatal("Range cannot be updated") }

	  infix ("foreach") ((MInt ==> MUnit) :: MUnit) implements foreach(MInt, 0, ${ e => $1(e) })

	  parallelize as ParallelCollection (MInt, lookupOp("range_illegalalloc"), lookupOp("length"), lookupOp("getElement"), lookupOp("range_illegalupdate"))
	}
  }
}