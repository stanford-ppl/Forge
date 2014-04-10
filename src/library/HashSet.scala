///////////////////////////////////////////////////////////////////////////////
//  Author: Christopher R. Aberger (caberger@stanford.edu)
//
//  File: HashSet.scala
//
//  Description: A very simple abstraction that provides the functionality of 
//  a hash set.  A wrapper around a hash map is used for this.
//
///////////////////////////////////////////////////////////////////////////////


package ppl.dsl.forge
package library

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

trait HashSetOps {
  this: ForgeApplication =>
  def importHashSetOps() {
    val T = tpePar("T")
    val HashSet = tpe("HashSet", T)

    //Just store a hash map.
    data(HashSet,("_data",MHashMap(T,MInt)))

    //Allocate an HashSet from an array of integers.
    static (HashSet) ("apply", T, MArray(T) :: HashSet) implements allocates(HashSet, ${hs_alloc_from_array[T]($0)})

    val HashSetOps = withTpe(HashSet)
    HashSetOps{
      infix("length")(Nil :: MInt) implements single ${ fhashmap_size[T,Int](hs_get($self)) }
      infix("toArray")(Nil :: MArray(T)) implements single ${ fhashmap_keys[T,Int](hs_get($self)) }
      infix("contains")(T :: MBoolean) implements single ${ fhashmap_contains[T,Int](hs_get($self),$1) }
      compiler("hs_get")(Nil :: MHashMap(T,MInt)) implements getter(0, "_data")
    }
    //Perform the group by reduce to create the hash map for allocation.
    compiler (HashSet) ("hs_alloc_from_array", T, MArray(T) :: MHashMap(T,MInt)) implements single ${array_groupByReduce[T,T,Int]($0,e=>e,e=>0,(a,b)=>0)}
  }
}


