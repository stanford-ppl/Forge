/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: A simple container for an atomic integer array.
Used as a bitmap for BFS (could be optimized further).
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait ParBitSetOps {
	this: OptiGraphDSL =>
	def importParBitSetOps() {
    val SBitSet = ephemeralTpe("java.util.BitSet")
    val ParBitSet = tpe("ParBitSet")
    data(ParBitSet,("_data",SBitSet))

    static (ParBitSet) ("apply", Nil, MArray(MInt) :: ParBitSet) implements allocates(ParBitSet ,${sbitset_sbitsetfromarray($0)})
    static (ParBitSet) ("apply", Nil, SBitSet :: ParBitSet) implements allocates(ParBitSet ,${$0})

    val ParBitSetOps = withTpe(ParBitSet)
    ParBitSetOps{
      infix("&")(SBitSet :: ParBitSet) implements single ${ ParBitSet(get_bitset($self) & $1)}
      infix("cardinality")(Nil :: MInt) implements single ${ get_bitset($self).cardinality }
      infix("length")(Nil :: MInt) implements single ${ get_bitset($self).size }
      infix("apply")(MInt :: MBoolean) implements single ${ get_bitset($self).get($1) }

      compiler("pbs_apply")(MInt :: MBoolean) implements single ${ get_bitset($self).get($1) }
      compiler("pbs_update")( (("id",MInt),("n",MBoolean)) :: MUnit, effect=write(0)) implements composite ${ get_bitset($self).set($1,$2) }
      compiler("pbs_raw_alloc")(MInt :: ParBitSet) implements single ${ParBitSet(SBitSet($1))}
      compiler ("get_bitset") (Nil :: SBitSet) implements getter(0, "_data")
      parallelize as ParallelCollection(MBoolean, lookupOp("pbs_raw_alloc"), lookupOp("length"), lookupOp("pbs_apply"), lookupOp("pbs_update"))
    }
  }
}