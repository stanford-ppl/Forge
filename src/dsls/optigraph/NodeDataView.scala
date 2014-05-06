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

trait NodeDataViewOps {
  this: OptiGraphDSL =>
  def importNodeDataViewOps() {
    val NodeData = lookupTpe("NodeData")
    val T = tpePar("T")
    val R = tpePar("R")
    val NodeDataView = tpe("NodeDataView",T)

    data(NodeDataView, ("_data", MArray(T)), ("_start", MInt), ("_length", MInt))
    static (NodeDataView) ("apply", T, (MArray(T), MInt, MInt) :: NodeDataView(T)) implements allocates(NodeDataView, ${$0}, ${$1}, ${$2})
    val NodeDataViewOps = withTpe(NodeDataView)
    NodeDataViewOps {
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: T) implements composite ${ array_apply(NodeDataView_data($self), NodeDataView_start($self) + $1) }
      infix ("mapreduce") ( (T ==> R,(R,R) ==> R, T==>MBoolean) :: R, TNumeric(R), addTpePars=(T,R)) implements mapReduce((T,R), 0, ${e => $1(e)}, ${numeric_zero[R]}, ${(a,b) => $2(a,b)}, Some(${c => $3(c)}) )
      infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements foreach(T, 0, ${a => $1(a)})
      infix ("start") (Nil :: MInt) implements single ${NodeDataView_start($self)}

      infix ("intersectGallop") (NodeDataView(T) :: MLong, TNumeric(T)) implements single ${
        val x = $0
        val y = $1
        var i = 0
        var j = 0
        var t = 0l
        while (i < x.length && j < y.length) {
          //println("i: " + i + " j: " + j + " iLen: " + x.length + " jLen: " + y.length) 
          if (x(i) == y(j)) {
            //println("Match") 
            t += 1
            i += 1
            j += 1
          }
          else if (x(i) < y(j)) { 
            //println("Entering 1")
            i = gallop(x,i,y(j))
            //println("Exiting 1: " + i)
 
          }
          else if (y(j) < x(i)) {
            //println("Entering 2")
            j = gallop(y,j,x(i))
            //println("Exiting 2: " + j)  
          }
        }
        t
      }
      compiler ("gallop") ( (("startIn",MInt),("tt",T)) :: MInt, TNumeric(T)) implements single ${
        var start = startIn
        var stepSize = 1
        val v = $0
        var notFinished = if (start < v.length) v(start) < tt else false
        var inRange = false
        while (notFinished) {
          //println("1start: " + start + " stepSize: " + stepSize + " vLen: " + v.length)
          inRange = if ((start + stepSize) < v.length) v(start+stepSize) < tt else false
          if (inRange) {
            start += stepSize
            stepSize = stepSize << 1
          } else {
            start += 1
            stepSize = 1
          }
          //println("2start: " + start + " stepSize: " + stepSize + " vLen: " + v.length)
          notFinished = if (start < v.length) v(start) < tt else false
        }
        start
      }
      infix ("intersect") (NodeDataView(T) :: MLong, TNumeric(T)) implements single ${
        val nbrs = $self
        val nbrsOfNbrs = $1
        if(nbrs.length == 0 || nbrsOfNbrs.length == 0) 0l
        else if(nbrs(0) > nbrsOfNbrs(nbrsOfNbrs.length-1) || 
          nbrsOfNbrs(0) > nbrs(nbrs.length-1)){
          0l
        }
        else if(nbrs.length > 128 || nbrsOfNbrs.length > 128){
          $self.intersectGallop($1)
        }
        else{
          ndv_intersect_sets(nbrs,nbrsOfNbrs)
        }
      }
      compiler ("ndv_intersect_sets") (NodeDataView(T) :: MLong, TNumeric(T)) implements single ${
        val nbrs = $self
        val nbrsOfNbrs = $1
        var i = 0
        var t = 0l
        var j = 0
        val small = if(nbrs.length < nbrsOfNbrs.length) nbrs else nbrsOfNbrs
        val large = if(nbrs.length < nbrsOfNbrs.length) nbrsOfNbrs else nbrs
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

      infix ("serialForeach") ((T ==> MUnit) :: MUnit, effect = simple) implements single ${
        var i = 0
        while(i < $self.length){
          $1($self(i))
          i += 1
        }
      }      
      infix ("print") (Nil :: MUnit, effect = simple) implements single ${
        var i = 0
        while(i < $self.length){
          println("NodeDataView -- Index: " + i + " Data: " + $self(i))
          i += 1
        }
      }
      infix ("getRawArray") (Nil :: MArray(T)) implements composite ${
        val d = array_empty[T]($self.length)
        array_copy(NodeDataView_data($self),NodeDataView_start($self),d,0,$self.length)
        d
      }

      compiler ("NodeDataView_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("NodeDataView_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("NodeDataView_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("NodeDataViews cannot be allocated from a parallel op") }
      compiler ("NodeDataView_illegalupdate") ((MInt, T) :: MNothing, effect = simple) implements composite ${ fatal("NodeDataViews cannot be updated") }
      parallelize as ParallelCollection(T, lookupOp("NodeDataView_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("NodeDataView_illegalupdate"))
    }
    compiler (NodeData) ("ndv_fake_alloc", Nil, Nil :: NodeDataView(MInt)) implements single ${ NodeDataView(array_empty_imm[Int](0),0,0) }
    direct(NodeDataView) ("sumOverCollection", (T,R), CurriedMethodSignature(List(("nd_view",NodeDataView(T)), ("data",T==>R) ,("cond",T==>MBoolean)),R), TNumeric(R)) implements composite ${nd_view.mapreduce[R]( e => data(e), (a,b) => a+b, cond)}
  }
}
