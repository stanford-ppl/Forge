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
    val NodeData = lookupTpe("NodeData")
    val GraphBitSet = lookupTpe("GraphBitSet")
    val NodeCollection = tpe("NodeCollection")
    val T = tpePar("T")
    val R = tpePar("R")

    /*
      Types for Node Collections
      
      0 - GraphBitSet
      1 - NodeData
      2 - HashMap
    */
    data(NodeCollection,("_type",MInt),("_dataBS",GraphBitSet),("_dataNV",NodeData(MInt)),("_dataHM",MHashMap(MInt,MInt)))

    static (NodeCollection) ("apply", Nil, GraphBitSet :: NodeCollection) implements allocates(NodeCollection,${nc_0},${$0},${nd_fake_alloc},${hm_fake_alloc})
    static (NodeCollection) ("apply", Nil, NodeData(MInt) :: NodeCollection) implements allocates(NodeCollection,${nc_1},${gbs_fake_alloc},${$0},${hm_fake_alloc})
    static (NodeCollection) ("apply", Nil, MHashMap(MInt,MInt) :: NodeCollection) implements allocates(NodeCollection,${nc_2},${gbs_fake_alloc},${nd_fake_alloc},${$0})

    val NodeCollectionOps = withTpe(NodeCollection)
    NodeCollectionOps{
      infix ("colType") (Nil :: MInt) implements getter(0, "_type")
      infix ("foreach") ((MInt ==> MUnit) :: MUnit, effect = simple) implements composite ${
        if($self.colType == 0) nc_getgraphbitset($self).foreach($1)
        else if($self.colType == 1) nc_getnodedata($self).foreach($1)
        else nc_gethashmap_keydata($self).foreach($1)
      }

      infix ("print") (Nil :: MUnit, effect=simple) implements single ${
        if($self.colType == 0) nc_getgraphbitset($self).print
        else if($self.colType == 1) nc_getnodedata($self).print
        else nc_gethashmap_keydata($self).print    
      }
      infix ("length") (Nil :: MInt) implements single ${
        if($self.colType == 0) nc_getgraphbitset($self).cardinality
        else if($self.colType == 1) nc_getnodedata($self).length
        else nc_gethashmap_keydata($self).length       
      }

      infix ("intersect") (NodeCollection :: MInt) implements single ${
        //Needs to be 3 options 
        // 1. BS & BS
        if($self.colType == 0 && $1.colType == 0){
          //logical and
          val bs = nc_getgraphbitset($self)
          val a = (bs & nc_getgraphbitset($1)).cardinality        
          a
        }
        // 2. BS & NDV
        else if ( ($self.colType == 0 && $1.colType == 1) || ($self.colType == 1 && $1.colType == 0) ){
          //go through NDV probe BS
          val bs = if($self.colType==0) nc_getgraphbitset($self) else nc_getgraphbitset($1)
          val ndv = if($self.colType==1) nc_getnodedata($self) else nc_getnodedata($1)
          val a = ndv.mapreduce[Int]({ n => 
            if(bs(n)) 1
            else 0
          },(a,b) => a+b, e => true)         
          a
        }
        // 3. NDV & NDV
        else if ($self.colType == 0 && $1.colType == 0){
          //simple set intersection
          var i = 0
          var t = 0
          var j = 0

          val small = if($self.length < $1.length) nc_getnodedata($self) else nc_getnodedata($1)          
          val large = if($self.length < $1.length) nc_getnodedata($1) else nc_getnodedata($self)
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
          t
        }
        // 4. NDV and Hash
        else if ( ($self.colType == 2 && $1.colType == 1) || ($self.colType == 1 && $1.colType == 2) ){
          val hs = if($self.colType==2) $self else $1
          val ndv = if($self.colType==1) nc_getnodedata($self) else nc_getnodedata($1)
          ndv.mapreduce[Int]({n => 
            if(nc_hashmap_contains(hs,n)) 1
            else 0
          },(a,b) => a+b, e => true)
        }
        // 4. NDV and Hash
        else if ($self.colType == 2 && $1.colType == 2){
          val hsSmall = if($self.length > $1.length) $1 else $self
          val hsLarge = if($self.length <= $1.length) nc_gethashmap_keydata($1) else nc_gethashmap_keydata($self)
          hsLarge.mapreduce[Int]({n => 
            if(nc_hashmap_contains(hsSmall,n)) 1
            else 0
          },(a,b) => a+b, e => true)
        }
        // 6. BS and Hash
        else {
          val hs = if($self.colType==2) nc_gethashmap_keydata($0) else nc_gethashmap_keydata($1)
          val bs = if($self.colType==1) nc_getgraphbitset($self) else nc_getgraphbitset($1)
          hs.mapreduce[Int]({n => 
            if(bs(n)) 1
            else 0
          },(a,b) => a+b, e => true)
        }
      }
      compiler ("nc_getgraphbitset") (Nil :: GraphBitSet) implements getter(0, "_dataBS")
      compiler ("nc_getnodedata") (Nil :: NodeData(MInt)) implements getter(0, "_dataNV")
      compiler ("nc_gethashmap") (Nil :: MHashMap(MInt,MInt)) implements getter(0, "_dataHM")
      compiler ("nc_gethashmap_keydata") (Nil :: NodeData(MInt)) implements single ${ NodeData(fhashmap_keys(nc_gethashmap($self))) }
      compiler ("nc_hashmap_contains") (MInt :: MBoolean) implements single ${ fhashmap_contains(nc_gethashmap($self),$1) }
    }
    direct(NodeCollection) ("sumOverCollection", R, CurriedMethodSignature(List(("nc",NodeCollection), ("data",MInt==>R) ,("cond",MInt==>MBoolean)),R), TNumeric(R)) implements composite ${
        if(nc.colType == 0) nc_getgraphbitset(nc).mapreduce[R](data,{(a,b) => a+b},cond)
        else if(nc.colType == 1) nc_getnodedata(nc).mapreduce[R](data,{(a,b) => a+b},cond)
        else nc_gethashmap_keydata(nc).mapreduce[R](data,{(a,b) => a+b},cond)      
    }

    compiler (NodeCollection) ("nc_0", Nil, Nil :: MInt) implements single ${ 0 }
    compiler (NodeCollection) ("nc_1", Nil, Nil :: MInt) implements single ${ 1 }
    compiler (NodeCollection) ("nc_2", Nil, Nil :: MInt) implements single ${ 2 }

    //fake alloc for hash map
    compiler (NodeCollection) ("hm_fake_alloc", Nil, Nil :: MHashMap(MInt,MInt)) implements single ${ NodeData[Int](0).groupByReduce[Int,Int]({n => n}, {n => n}, {(a,b) => 0}) }
  }
}