///////////////////////////////////////////////////////////////////////////////
//  Author: Christopher R. Aberger (caberger@stanford.edu)
//
//  File: BitSet.scala
//
//  Description: A general BitSet trait for use.  Implemented using an array
//  of longs.  Bits are set by finding word indexes and and shifting proper
//  bit value in.  Uses parallel ops and is defined as a parallel class.
//  Can be distributed and has no set backs of Java BitSet class (although
//  it is based highly off that source code).  For right now mutability is 
//  only allowed if you allocate a bitset based of an integer size.  The
//  bitset does not grow or shrink if you try to set a bit outset of the 
//  initial range and will error out.  If you want this support add it in
//  yourself.  A basic starter class that can be improved.
//
//  64 bits per word.  6 address bits needed per word.  Based off of:
//  http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/6-b14/java/util/BitSet.java
///////////////////////////////////////////////////////////////////////////////


package ppl.dsl.forge
package library

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

trait BitSetOps {
  this: ForgeApplication =>

  def importBitSetOps() {
    val BitSet = tpe("BitSet")

    // We just store an array of longs or an array of words
    // Size is defined here as the physical # of bits allocated. (physical size)
    // Length is defined as the known bit set to 1. (logical length)
    data(BitSet,("_words",MArray(MLong)),("_cardinality",MInt))

    // Allocate an bitset from an array of integers, setting each integers bit to 1 in a bitset
    static (BitSet) ("apply", Nil, MArray(MInt) :: BitSet) implements allocates(BitSet, ${bs_alloc_from_int_array($0)},${array_length($0)})

    // Allocate a mutable bitset with a size large enough to store a range 0-N, where N is input arg.
    static (BitSet) ("apply", Nil, MInt :: BitSet, effect=mutable) implements allocates(BitSet, ${array_empty[Long](bs_get_alloc_length($0))},${numeric_zero[Int]})

    // Allocate a bitset where your internal words are already ready.
    static (BitSet) ("apply", Nil, (MArray(MLong),MInt) :: BitSet) implements allocates(BitSet, ${$0},${$1})

    val BitSetOps = withTpe(BitSet)
    BitSetOps{

      //////////////////////////////Basic Accessors////////////////////////////////////////

      // Length is defined here as the physical # of bits allocated. (physical size)
      infix("length")(Nil :: MInt) implements single ${ $self.numWords << 6} //multiply by 64
      infix("numWords")(Nil :: MInt) implements single ${ array_length(bs_get_words($self)) }
      // Number of bits set to 1 in this bitset
      infix("cardinality")(Nil :: MInt) implements getter(0, "_cardinality")
      infix("apply")(MInt :: MBoolean) implements single ${ bs_get($self,$1) }
      infix("set")((MInt,MBoolean) :: MUnit, effect=write(0)) implements single ${
        if ($1 < 0 || $1 > $0.length)  fatal("Cannot set bit set index: " + $1 + " BitSet physical range is 0-" + $0.length)
        else {
          if($2) bs_set($self,$1)
          else bs_clear($self,$1)
        }
      }

      //////////////////////////////Bit Set Operations////////////////////////////////////////

      infix("&")(BitSet :: BitSet) implements composite ${
        val smallLength = if ($0.numWords > $1.numWords) $1.numWords else $0.numWords
        val a1 = bs_get_words($0)
        val a2 = bs_get_words($1)

        val mapper = array_fromfunction[Int](smallLength, e=>e)
        val result = array_map[Int,Long](mapper, {e => a1(e) & a2(e)})
        val cardinality = array_reduce[Int](array_map[Long,Int](result,{e => math_object_bitcount(e) }), {(a,b) => a+b}, numeric_zero[Int])
        BitSet(result,cardinality)
      }

      infix("andCardinality")(BitSet :: MInt) implements composite ${
        val smallLength = if ($0.numWords > $1.numWords) $1.numWords else $0.numWords
        val a1 = bs_get_words($0)
        val a2 = bs_get_words($1)

        val mapper = array_fromfunction[Int](smallLength, e=>e)
        val cardinality = array_reduce[Int](array_map[Int,Int](mapper,{e => math_object_bitcount(a1(e) & a2(e)) }), {(a,b) => a+b},numeric_zero[Int])
        cardinality
      }

      infix("|")(BitSet :: BitSet) implements composite ${
        val smallLength = if ($0.numWords > $1.numWords) $1.numWords else $0.numWords
        val a1 = bs_get_words($0)
        val a2 = bs_get_words($1)

        val mapper = array_fromfunction[Int](smallLength, e=>e)
        val result = array_map[Int,Long](mapper, {e => a1(e) | a2(e)})
        val cardinality = array_reduce[Int](array_map[Long,Int](result,{e => math_object_bitcount(e) }), {(a,b) => a+b}, numeric_zero[Int])
        BitSet(result,cardinality)
      }

      infix("xor")(BitSet :: BitSet) implements composite ${
        val smallLength = if ($0.numWords > $1.numWords) $1.numWords else $0.numWords
        val a1 = bs_get_words($0)
        val a2 = bs_get_words($1)

        val mapper = array_fromfunction[Int](smallLength, e=>e)
        val result = array_map[Int,Long](mapper, {e => a1(e) ^ a2(e)})
        val cardinality = array_reduce[Int](array_map[Long,Int](result,{e => math_object_bitcount(e) }), {(a,b) => a+b}, numeric_zero[Int])
        BitSet(result,cardinality)
      }

      //////////////////////////////Debug////////////////////////////////////////

      infix ("print") (Nil :: MUnit, effect = simple) implements single ${
        var i = 0
        println("cardinality: " + $self.cardinality)
        println("length: " + $self.length)
        while(i < $self.length){
          if ($self(i)) println("Set: " + i)
          i += 1
        }
      }

      //////////////////////////////Internal Operations////////////////////////////////////////

      compiler("bs_set")(("bitIndex",MInt) :: MUnit, effect=write(0)) implements single ${
        val wordIndex = bs_word_index(bitIndex)
        val oldValue = bs_get_word($self,wordIndex)
        val value = oldValue | (1L << bitIndex)
        if (value != oldValue) bs_set_cardinality($self,$self.cardinality+1)
        bs_set_word($self,wordIndex,value)
      }

      compiler("bs_clear")(("bitIndex",MInt) :: MUnit, effect=write(0)) implements single ${
        val wordIndex = bs_word_index(bitIndex)
        val oldValue = bs_get_word($self,wordIndex)
        val value = bs_get_word($self,wordIndex) & ~(1L << bitIndex)
        if (value != oldValue) bs_set_cardinality($self,$self.cardinality-1)
        bs_set_word($self,wordIndex,value)
      }

      compiler("bs_set_cardinality")(MInt :: MUnit, effect = write(0)) implements setter(0, "_cardinality", quotedArg(1))
      compiler("bs_get")(("bitIndex",MInt) :: MBoolean) implements single ${(bs_get_word($self,bs_word_index(bitIndex)) & (1L << bitIndex)) != 0}
      compiler("bs_get_word")(("wordIndex",MInt) :: MLong) implements single ${array_apply(bs_get_words($self),wordIndex)}
      compiler("bs_get_words")(Nil :: MArray(MLong)) implements getter(0, "_words")
      compiler("bs_set_word")(( ("wordIndex",MInt),("value",MLong)) :: MUnit, effect=write(0)) implements single ${array_update(bs_get_words($self),wordIndex,value)}
      
      compiler("bs_raw_alloc")(MInt :: BitSet) implements single ${BitSet($1)}
      compiler("bs_apply")(MInt :: MBoolean) implements single ${ $self($1) }
      parallelize as ParallelCollection(MBoolean, lookupOp("bs_raw_alloc"), lookupOp("length"), lookupOp("bs_apply"), lookupOp("set"))
    }

    //////////////////////////////Operations Needed for Alloc////////////////////////////////////////

    // Given an index, give me the word it lies in. 6 bits per word.
    compiler (BitSet) ("bs_word_index", Nil, ("bitIndex",MInt) :: MInt) implements single ${ bitIndex >> 6 } //ADDRESS BITS PER WORD

    // Gives you how many words you need to store a bitset with given input maximum value
    compiler (BitSet) ("bs_get_alloc_length", Nil, MInt :: MInt) implements single ${ bs_word_index($0)+1 }

    /* 
     * Allocates a bit set that sets the corresponding indexes in the array of incoming
     * integers.  There are some special optimizations here that make it quicker than
     * just looping around the integers and calling the set method.
     * This could happen in parallel.  Theoretically seemed more complex to code using parallel OPs though.
     */
    compiler (BitSet) ("bs_alloc_from_int_array", Nil, MArray(MInt) :: MArray(MLong)) implements single ${
      val sortedInput = bs_alloc_sort_array_in($0)
      // instead of sorting find max, place ints into numWords buckets, allocate in parallel with a map
      val words = array_empty[Long](bs_get_alloc_length(sortedInput(array_length(sortedInput)-1)))
      // Loop over my array of ints, setting integer indexes in bitset.
      var i = 0
      while (i < array_length(sortedInput)){
        var cur = sortedInput(i)
        var wordIndex = bs_word_index(cur)
        var setValue = 1L << cur
        var sameWord = true
        // Let's set all indices same word at once.
        i += 1
        while (i < array_length(sortedInput) && sameWord){
          // Are values in the same word?
          if (bs_word_index(sortedInput(i))==wordIndex){
            cur = sortedInput(i)
            setValue = setValue | (1L << cur)
            i += 1
          } else sameWord = false
        }
        // Set the current word and head off to the next.
        array_update(words,wordIndex,setValue)
      }
      words
    }

    // I separate this out in hopes that the compiler is smart enough not to do this work twice on allocations.
    compiler (BitSet) ("bs_alloc_sort_array_in", Nil, MArray(MInt) :: MArray(MInt)) implements single ${array_sort($0)}
  }
}


