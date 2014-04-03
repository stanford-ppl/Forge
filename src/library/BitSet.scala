package ppl.dsl.forge
package library

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

trait BitSetOps {
  this: ForgeApplication =>
  def importBitSetOps() {
    val BitSet = tpe("BitSet")
    val Tuple2 = lookupTpe("Tup2")
    //val ADDRESS_BITS_PER_WORD = 6

    //We just store an array of longs or an array of words
    //Size is defined here as the physical # of bits allocated. (physical size)
    //Length is defined as the known bit set to 1. (logical length)
    data(BitSet,("_words",MArray(MLong)),("_length",MInt))

    static (BitSet) ("apply", Nil, MArray(MInt) :: BitSet) implements allocates(BitSet, ${bs_alloc_from_int_array($0)},${array_apply(bs_alloc_sort_array_in($0),array_length($0)-1)})
    static (BitSet) ("apply", Nil, MInt :: BitSet, effect=mutable) implements allocates(BitSet, ${bs_alloc_words($0)},${numeric_zero[Int]})

    val BitSetOps = withTpe(BitSet)
    BitSetOps{
      //NEXT add set and make sure you have ensure extra
      //create an ID view to map over indicies

      //Size is defined here as the physical # of bits allocated. (physical size)
      //Length is defined as the known bit set to 1. (logical length)
      infix("length")(Nil :: MInt) implements getter(0,"_length")
      infix("size")(Nil :: MInt) implements single ${ $self.numWords << 6} //multiply by 64
      infix("numWords")(Nil :: MInt) implements single ${ array_length(bs_get_words($self)) }
      infix("apply")(MInt :: MBoolean) implements single ${ bs_get($self,$1) }

      //For Debug, prints the bits that are set in the bitset
      infix ("print") (Nil :: MUnit, effect = simple) implements single ${
        var i = 0
        println("BitSet Contents")
        while(i < $self.length){
          if($self(i))
            println("Set: " + i)
          i += 1
        }
      }     
      compiler("bs_get")(MInt :: MBoolean) implements single ${(bs_get_word($self,$1) & (1L << $1)) != 0}
      compiler("bs_get_word")(MInt :: MLong) implements single ${array_apply(bs_get_words($self),bs_word_index($1))}
      compiler("bs_get_words")(Nil :: MArray(MLong)) implements getter(0, "_words")
    }
    
    //Given an index, give me the word it lies in. 6 bits per word.
    compiler (BitSet) ("bs_word_index", Nil, MInt :: MInt) implements single ${ $0 >> 6 } //ADDRESS BITS PER WORD
    
    //Allocates a bit set that sets the corresponding indexes in the array of incoming
    //integers.  There are some special optimizations here that make it quicker than
    //just looping around the integers and calling the set method.
    compiler (BitSet) ("bs_alloc_from_int_array", Nil, MArray(MInt) :: MArray(MLong)) implements single ${
      val sortedInput = bs_alloc_sort_array_in($0)
      val words = bs_alloc_words(array_length(sortedInput))

      //Loop over my array of ints, setting integer indexes in bitset.
      var i = 0
      while(i < array_length(sortedInput)){
        var cur = sortedInput(i)
        var wordIndex = bs_word_index(cur)
      
        var setValue = 1L << cur
        var sameWord = true
        //Let's set all indicies same word at once.
        while((i+1) < array_length(sortedInput) && sameWord){
          i += 1
          //Are values in the same word?
          if(bs_word_index(sortedInput(i))==wordIndex){
            cur = sortedInput(i)
            setValue = setValue | (1L << cur)
          } else sameWord = false
        }
        //Set the current word and head off to the next.
        array_update(words,wordIndex,setValue)
      }
      words
    }
    //I sepearate this out in hopes that the compiler is smart enough not to 
    //do this work twice on allocations.
    compiler (BitSet) ("bs_alloc_sort_array_in", Nil, MArray(MInt) :: MArray(MInt)) implements single ${array_sort($0)}
    compiler (BitSet) ("bs_alloc_words", Nil, MInt :: MArray(MLong)) implements single ${ array_empty[Long](bs_word_index($0-1)+1)}
  }
}


