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

      infix ("intersect") (NodeDataView(T) :: MInt, TNumeric(T)) implements single ${
        val nbrs = $self
        val nbrsOfNbrs = $1
        var i = 0
        var t = 0
        var j = 0
        val small = if(nbrs.length < nbrsOfNbrs.length) nbrs else nbrsOfNbrs
        val large = if(nbrs.length < nbrsOfNbrs.length) nbrsOfNbrs else nbrs
        while(i < small.length  && j < large.length){
          while(j < large.length && large(j) < small(i)){
            j += 1
          }
          if(j < large.length && small(i)==large(j)){              
            t += 1
            j += 1
          }
          i += 1
        }
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
    direct(NodeDataView) ("sumOverCollection", (T,R), CurriedMethodSignature(List(("nd_view",NodeDataView(T)), ("data",T==>R) ,("cond",T==>MBoolean)),R), TNumeric(R)) implements composite ${nd_view.mapreduce[R]( e => data(e), (a,b) => a+b, cond)}
  }
}