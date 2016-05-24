/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Lets us view part of an NodeData array as a
parallel collection.  This is especially useful when wanting
to perform operations on neighbors (a subset of edge array).
Here you look at actual data inside of the array.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package	dsls
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait NeighborViewOps {
  this: OptiGraphDSL =>
  def importNeighborViewOps() {
    val NodeData = lookupTpe("NodeData")
    val T = tpePar("T")
    val R = tpePar("R")
    val NeighborView = tpe("NeighborView",T)

    data(NeighborView, ("_data", MArray(T)), ("_start", MInt), ("_length", MInt))
    static (NeighborView) ("apply", T, (MArray(T), MInt, MInt) :: NeighborView(T)) implements allocates(NeighborView, ${$0}, ${$1}, ${$2})
    val NeighborViewOps = withTpe(NeighborView)
    NeighborViewOps {
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: T) implements composite ${ array_apply(NeighborView_data($self), NeighborView_start($self) + $1) }
      infix ("reduce") (((T,T) ==> T) :: T, TNumeric(T)) implements reduce(T, 0, ${numeric_zero[T]}, ${ (a,b) => $1(a,b) })
      infix ("mapreduce") ( (T ==> R,(R,R) ==> R, T==>MBoolean) :: R, TNumeric(R), addTpePars=(T,R)) implements mapReduce((T,R), 0, ${e => $1(e)}, ${numeric_zero[R]}, ${(a,b) => $2(a,b)}, Some(${c => $3(c)}) )
      infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements foreach(T, 0, ${a => $1(a)})
      infix ("serialForEach") ((T ==> MUnit) :: MUnit, effect = simple) implements single ${
        var i = 0
        while(i < $self.length){
          $1($self(i))
          i += 1
        }
      }
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, ${a => println(a)})
      infix ("getRawArray") (Nil :: MArray(T)) implements composite ${
        val d = array_empty[T]($self.length)
        array_copy(NeighborView_data($self),NeighborView_start($self),d,0,$self.length)
        d
      }
      infix ("intersect") (NeighborView(T) :: MLong, TNumeric(T)) implements single ${
        val neighbors = $self
        val neighborsOfNeighbors = $1
        if(neighbors.length == 0 || neighborsOfNeighbors.length == 0) 0l
        else if(neighbors(0) > neighborsOfNeighbors(neighborsOfNeighbors.length-1) ||
          neighborsOfNeighbors(0) > neighbors(neighbors.length-1)){
          0l
        }
        else{
          ndv_intersect_sets(neighbors,neighborsOfNeighbors)
        }
      }
      internal ("ndv_intersect_sets") (NeighborView(T) :: MLong, TNumeric(T)) implements single ${
        val neighbors = $self
        val neighborsOfNeighbors = $1
        var i = 0
        var t = 0l
        var j = 0
        val small = if(neighbors.length < neighborsOfNeighbors.length) neighbors else neighborsOfNeighbors
        val large = if(neighbors.length < neighborsOfNeighbors.length) neighborsOfNeighbors else neighbors
        //I understand there are simplier ways to write this, I tried a lot of versions
        //this is the fastest (that I tried).
        while(i < (small.length-1)  && j < (large.length-1)){
          while(j < (large.length-1) && large(j) < small(i)){
            j += 1
          }
          if(small(i)==large(j)){
           t += 1
          }
          i += 1
        }
        //if i reaches the end before j
        while(j < (large.length-1) && large(j) < small(i)){
          j += 1
        }
        //if j reaches the end before i
        while(large(j) > small(i) && i < (small.length-1)){
          i += 1
        }
        if(small(i) == large(j)) t += 1
        t
      }
      infix ("intersectInRange") ((("neighborsOfNeighbors",NeighborView(T)),("neighborsMax",T)) :: MLong, TNumeric(T)) implements single ${
        val neighbors = $self

        if(neighbors.length < 2 || neighborsOfNeighbors.length < 2 ) 0l
        else if(neighborsMax <= neighborsOfNeighbors(0) ||
          neighborsMax <= neighbors(0)){
          0l
        }
        else if(neighbors(0) > neighborsOfNeighbors(neighborsOfNeighbors.length-1) ||
          neighborsOfNeighbors(0) > neighbors(neighbors.length-1)){
          0l
        }
        else{
          ndv_intersect_sets_in_range($self,neighborsOfNeighbors,neighborsMax)
        }
      }
      internal ("ndv_intersect_sets_in_range") ((("neighborsOfNeighbors",NeighborView(T)),("neighborsMax",T)) :: MLong, TNumeric(T)) implements single ${
        val neighbors = $self
        var t = 0l
        var i = 0
        var j = 0
        val small = if(neighbors.length < neighborsOfNeighbors.length) neighbors else neighborsOfNeighbors
        val large = if(neighbors.length < neighborsOfNeighbors.length) neighborsOfNeighbors else neighbors
        val smallMax = neighborsMax
        val largeMax = neighborsMax
        //I understand there are simplier ways to write this, I tried a lot of versions
        //this is the fastest (that I tried).
        var notFinished = small(i) < smallMax && large(j) < largeMax
        while(i < (small.length-1)  && j < (large.length-1) && notFinished){
          while(j < (large.length-1) && large(j) < small(i) && notFinished){
            j += 1
            notFinished = large(j) < largeMax
          }
          if(small(i)==large(j) && notFinished){
           t += 1
          }
          i += 1
          notFinished = notFinished && small(i) < smallMax
        }
        //if i reaches the end before j
        while(j < (large.length-1) && large(j) < small(i) && notFinished){
          j += 1
          notFinished = large(j) < largeMax
        }
        //if j reaches the end before i
        while(large(j) > small(i) && i < (small.length-1) && notFinished){
          i += 1
          notFinished = small(i) < smallMax
        }
        if(small(i) == large(j) && notFinished) t += 1
        t
      }

      internal ("NeighborView_data") (Nil :: MArray(T)) implements getter(0, "_data")
      internal ("NeighborView_start") (Nil :: MInt) implements getter(0, "_start")
      internal ("NeighborView_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("NeighborViews cannot be allocated from a parallel op") }
      internal ("NeighborView_illegalupdate") ((MInt, T) :: MNothing, effect = simple) implements composite ${ fatal("NeighborViews cannot be updated") }

      parallelize as ParallelCollection(T, lookupOp("NeighborView_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("NeighborView_illegalupdate"))
    }
  }
}