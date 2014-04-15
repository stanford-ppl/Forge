/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: A simple wrapper around the BitSet trait.  Here
we define parallel OP's on the bitset (ie mapreduce) that are
specific to a Graph DSL.

*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait GraphBitSetOps {
	this: OptiGraphDSL =>
	def importGraphBitSetOps() {
    val NodeIdView = lookupTpe("NodeIdView")
    val GraphBitSet = tpe("GraphBitSet")
    val BitSet = lookupTpe("BitSet")
    val R = tpePar("R")

    data(GraphBitSet,("_data",BitSet))

    static (GraphBitSet) ("apply", Nil, MArray(MInt) :: GraphBitSet) implements allocates(GraphBitSet ,${bs_alloc_from_array($0)})
    static (GraphBitSet) ("apply", Nil, BitSet :: GraphBitSet) implements allocates(GraphBitSet ,${$0})

    val GraphBitSetOps = withTpe(GraphBitSet)
    GraphBitSetOps{
      infix("&")(GraphBitSet :: GraphBitSet) implements single ${ GraphBitSet(gbs_get_bitset($self) & gbs_get_bitset($1)) }
      infix("andCardinality")(GraphBitSet :: MInt) implements single ${ gbs_get_bitset($self).andCardinality(gbs_get_bitset($1)) }
      infix("cardinality")(Nil :: MInt) implements single ${ gbs_get_bitset($self).cardinality }
      infix("length")(Nil :: MInt) implements single ${ gbs_get_bitset($self).length }
      infix("apply")(MInt :: MBoolean) implements single ${ gbs_get_bitset($self).apply($1) }

      infix ("foreach") ((MInt ==> MUnit) :: MUnit, effect = simple) implements composite ${
        val ids = NodeIdView($self.length)
        ids.foreach{ t =>
          if($self(t)) $1(t)
        }
      }
      infix ("mapreduce") ( (MInt ==> R,(R,R) ==> R, MInt==>MBoolean) :: R, TNumeric(R), addTpePars=R) implements composite ${
        val ids = NodeIdView($self.length)
        ids.mapreduce({ t=>
          if($self(t))
            $1(t)
          else
            numeric_zero[R]
        },$2,$3)
      }

      //For Debug
      infix ("print") (Nil :: MUnit, effect = simple) implements single ${gbs_get_bitset($self).print}

      //For ParallelCollection
      compiler("gbs_apply")(MInt :: MBoolean) implements single ${ gbs_get_bitset($self).apply($1) }
      compiler("gbs_update")( (("id",MInt),("n",MBoolean)) :: MUnit, effect=write(0)) implements composite ${ gbs_get_bitset($self).set(id,n) }
      compiler("gbs_raw_alloc")(MInt :: GraphBitSet) implements single ${GraphBitSet(BitSet($1))}
      compiler ("gbs_get_bitset") (Nil :: BitSet) implements getter(0, "_data")
      parallelize as ParallelCollection(MBoolean, lookupOp("gbs_raw_alloc"), lookupOp("length"), lookupOp("gbs_apply"), lookupOp("gbs_update"))
    }
    compiler (GraphBitSet) ("bs_alloc_from_array", Nil, MArray(MInt) :: BitSet) implements single ${BitSet($0)}
    direct(GraphBitSet) ("sum", R, CurriedMethodSignature(List(("gbs",GraphBitSet), ("data",MInt==>R) ),R), TNumeric(R)) implements composite ${
      gbs.mapreduce[R]( e => data(e), (a,b) => a+b, e => true)
    }

    //fake out allocate for my NodeCollection routine
    compiler (GraphBitSet) ("gbs_fake_alloc", Nil, Nil :: GraphBitSet) implements single ${GraphBitSet(BitSet(array_empty_imm[Long](0),0))}
  }
}