/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: A simple container for an atomic integer array.
Used as a bitmap for BFS (could be optimized further).
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait NodeCollectionOps {
	this: OptiGraphDSL =>
	def importNodeCollectionOps() {
    val NodeDataView = lookupTpe("NodeDataView")
    val ParBitSet = lookupTpe("ParBitSet")
    val NodeCollection = tpe("NodeCollection")
    val R = tpePar("R")

    /*
      Types for Node Collections
      
      0 - ParBitSet
      1 - NodeDataView
      ...more to come probably
    */
    data(NodeCollection,("_type",MInt),("_dataBS",ParBitSet),("_dataNV",NodeDataView(MInt)))

    static (NodeCollection) ("apply", Nil, ParBitSet :: NodeCollection) implements allocates(NodeCollection,${nc_0},${$0},${ndv_fake_alloc})
    static (NodeCollection) ("apply", Nil, NodeDataView(MInt) :: NodeCollection) implements allocates(NodeCollection,${nc_1},${pbs_fake_alloc},${$0})

    val NodeCollectionOps = withTpe(NodeCollection)
    NodeCollectionOps{
      infix ("colType") (Nil :: MInt) implements getter(0, "_type")
      infix ("print") (Nil :: MUnit, effect=simple) implements single ${
        if($self.colType == 0) get_parbitset($self).print
        else get_nodeview($self).print       
      }

      infix ("length") (Nil :: MInt) implements single ${
        if($self.colType == 0) get_parbitset($self).cardinality
        else get_nodeview($self).length        
      }

      infix ("mapreduce") ( (MInt ==> R,(R,R) ==> R, MInt==>MBoolean) :: R, TNumeric(R), addTpePars=R) implements composite ${
        if($self.colType == 0) get_parbitset($self).mapreduce($1,$2,$3)
        else get_nodeview($self).mapreduce($1,$2,$3)
      }
      infix ("intersect") (NodeCollection :: MInt) implements single ${
        //Needs to be 3 options 
        // 1. BS & BS
        if($self.colType == 0 && $1.colType == 0){
          //logical and
          val pbss = get_parbitset($self)
          tic("intersect 1", pbss)
          val a = (pbss & get_parbitset($1)).cardinality
          toc("intersect 1", a)
          a
        }
        // 2. BS & NDV
        else if ($self.colType != $1.colType){
          //go through NDV probe BS
          val pbs = if($self.colType==0) get_parbitset($self) else get_parbitset($1)
          tic("intersect 2", pbs)

          val ndv = if($self.colType==1) get_nodeview($self) else get_nodeview($1)
          val a = ndv.mapreduce[Int]({ n => 
            if(pbs(n)) 1
            else 0
          },(a,b) => a+b, e => true)
          toc("intersect 2", a)
          a
        }
        // 3. NDV & NDV
        else{
          //simple set intersection
          var i = 0
          var t = 0
          var j = 0

          val small = if($self.length < $1.length) get_nodeview($self) else get_nodeview($1)
          tic("intersect 3", i)
          val large = if($self.length < $1.length) get_nodeview($1) else get_nodeview($self)
          while(i < small.length  && j < large.length){
            var go = large(j) < small(i) 
            while(go){
              j += 1
              if(j < large.length){
                go = large(j) < small(i) 
              }
              else go = false
            }
            if(j < large.length){
              if(small(i)==large(j)){              
                t += 1
                j += 1
              }
            }
            i += 1
          }
          val a = t
          toc("intersect 3", a)
          a
        }
      }
      compiler ("get_parbitset") (Nil :: ParBitSet) implements getter(0, "_dataBS")
      compiler ("get_nodeview") (Nil :: NodeDataView(MInt)) implements getter(0, "_dataNV")
    }
    compiler (NodeDataView) ("nc_0", Nil, Nil :: MInt) implements single ${ 0 }
    compiler (NodeDataView) ("nc_1", Nil, Nil :: MInt) implements single ${ 1 }
  }
}