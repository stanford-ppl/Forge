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
    val NodeIdView = lookupTpe("NodeIdView")
    val ParBitSet = tpe("ParBitSet")
    val SBitSet = ephemeralTpe("java.util.BitSet")
    val R = tpePar("R")

    data(ParBitSet,("_data",SBitSet))

    static (ParBitSet) ("apply", Nil, (MInt,MArray(MInt)) :: ParBitSet) implements allocates(ParBitSet ,${sbitset_sbitsetfromarray($0,$1)})
    static (ParBitSet) ("apply", Nil, SBitSet :: ParBitSet) implements allocates(ParBitSet ,${$0})

    val ParBitSetOps = withTpe(ParBitSet)
    ParBitSetOps{
      infix("&")(ParBitSet :: ParBitSet) implements single ${ ParBitSet(get_bitset($self) & get_bitset($1))}
      infix("cardinality")(Nil :: MInt) implements single ${ get_bitset($self).cardinality }
      infix("length")(Nil :: MInt) implements single ${ get_bitset($self).size }
      infix("apply")(MInt :: MBoolean) implements single ${ get_bitset($self).get($1) }

      infix ("mapreduce") ( (MInt ==> R,(R,R) ==> R, MInt==>MBoolean) :: R, TNumeric(R), addTpePars=R) implements composite ${
        val ids = NodeIdView($self.length)
        ids.mapreduce({ t=>
          if($self(t))
            $1(t)
          else
            numeric_zero[R]
        },$2,$3)
      }
      //mapReduce((T,R), 0, ${e => $1(e)}, ${numeric_zero[R]}, ${(a,b) => $2(a,b)}, Some(${c => $3(c)}) )

      //For Debug
      infix ("print") (Nil :: MUnit, effect = simple) implements single ${
        var i = 0
        println("BitSet Contents")
        while(i < $self.length){
          if($self(i))
            println("Set: " + i)
          i += 1
        }
      }

      //For ParallelCollection
      compiler("pbs_apply")(MInt :: MBoolean) implements single ${ get_bitset($self).get($1) }
      compiler("pbs_update")( (("id",MInt),("n",MBoolean)) :: MUnit, effect=write(0)) implements composite ${ get_bitset($self).set($1,$2) }
      compiler("pbs_raw_alloc")(MInt :: ParBitSet) implements single ${ParBitSet(SBitSet($1))}
      compiler ("get_bitset") (Nil :: SBitSet) implements getter(0, "_data")
      parallelize as ParallelCollection(MBoolean, lookupOp("pbs_raw_alloc"), lookupOp("length"), lookupOp("pbs_apply"), lookupOp("pbs_update"))
    }
    //fake out allocate for my NodeCollection routine
    compiler (ParBitSet) ("pbs_fake_alloc", Nil, Nil :: ParBitSet) implements single ${ParBitSet(0,array_empty[Int](0))}
  }
}