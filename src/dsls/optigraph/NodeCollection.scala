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
    val GraphBitSet = lookupTpe("GraphBitSet")
    val HashSet = lookupTpe("HashSet")
    val NodeCollection = tpe("NodeCollection")
    val T = tpePar("T")
    val R = tpePar("R")

    /*
      Types for Node Collections
      
      0 - GraphBitSet
      1 - NodeDataView
      2 - HashMap
    */
    data(NodeCollection,("_type",MInt),("_dataBS",GraphBitSet),("_dataNV",NodeDataView(MInt)),("_dataHS",HashSet(MInt)))

    static (NodeCollection) ("apply", Nil, GraphBitSet :: NodeCollection) implements allocates(NodeCollection,${nc_0},${$0},${ndv_fake_alloc},${hs_fake_alloc})
    static (NodeCollection) ("apply", Nil, NodeDataView(MInt) :: NodeCollection) implements allocates(NodeCollection,${nc_1},${gbs_fake_alloc},${$0},${hs_fake_alloc})
    static (NodeCollection) ("apply", Nil, HashSet(MInt) :: NodeCollection) implements allocates(NodeCollection,${nc_2},${gbs_fake_alloc},${ndv_fake_alloc},${$0})

    val NodeCollectionOps = withTpe(NodeCollection)
    NodeCollectionOps{
      infix ("colType") (Nil :: MInt) implements getter(0, "_type")
      infix ("foreach") ((MInt ==> MUnit) :: MUnit, effect = simple) implements composite ${
        if($self.colType == 0) nc_getgraphbitset($self).foreach($1)
        else if($self.colType == 1) nc_getNodeDataView($self).foreach($1)
        else nc_gethashset_keydata($0).foreach($1)
      }

      infix ("print") (Nil :: MUnit, effect=simple) implements single ${
        if($self.colType == 0) nc_getgraphbitset($self).print
        else if($self.colType == 1) nc_getNodeDataView($self).print
        else nc_gethashset_keydata($0).print    
      }
      infix ("length") (Nil :: MInt) implements single ${
        if($self.colType == 0) nc_getgraphbitset($self).cardinality
        else if($self.colType == 1) nc_getNodeDataView($self).length
        else nc_gethashset($self).length       
      }
      infix ("intersect") (NodeCollection :: MLong) implements single ${
        //Needs to be 3 options 
        // 1. BS & BS
        if($self.colType == 0 && $1.colType == 0){
          //logical and
          val bs = nc_getgraphbitset($self)
          val a = (bs & nc_getgraphbitset($1)).cardinality        
          a.toLong
        }
        // 2. BS & NDV
        else if ( ($self.colType == 0 && $1.colType == 1) || ($self.colType == 1 && $1.colType == 0) ){
          //go through NDV probe BS
          val bs = if($self.colType==0) nc_getgraphbitset($self) else nc_getgraphbitset($1)
          val ndv = if($self.colType==1) nc_getNodeDataView($self) else nc_getNodeDataView($1)
          val a = ndv.mapreduce[Long]({ n => 
            if(bs(n)) 1l
            else 0l
          },(a,b) => a+b, e => true)         
          a
        }
        // 3. NDV & NDV
        else if ($self.colType == 1 && $1.colType == 1){
          //simple set intersection
          val nbrs = nc_getNodeDataView($self)
          val nbrsOfNbrs = nc_getNodeDataView($1)
          nbrs.intersect(nbrsOfNbrs)
        }
        // 4. NDV and Hash
        else if ( ($self.colType == 2 && $1.colType == 1) || ($self.colType == 1 && $1.colType == 2) ){
          val hs = if($self.colType==2) nc_gethashset($self) else nc_gethashset($1)
          val ndv = if($self.colType==1) nc_getNodeDataView($self) else nc_getNodeDataView($1)
          ndv.mapreduce[Long]({n => 
            if(hs.contains(n)) 1l
            else 0l
          },(a,b) => a+b, e => true)
        }
        // 4. Hash and Hash
        else if ($self.colType == 2 && $1.colType == 2){
          val hsSmall = if($self.length > $1.length) nc_gethashset($1) else nc_gethashset($self)
          val hsLarge = if($self.length <= $1.length) nc_gethashset_keydata($1) else nc_gethashset_keydata($self)
          hsLarge.mapreduce[Long]({n => 
            if(hsSmall.contains(n)) 1l
            else 0l
          },(a,b) => a+b, e => true)
        }
        // 6. BS and Hash
        else {
          val hs = if($self.colType==2) nc_gethashset_keydata($0) else nc_gethashset_keydata($1)
          val bs = if($self.colType==0) nc_getgraphbitset($self) else nc_getgraphbitset($1)
          hs.mapreduce[Long]({n => 
            if(bs(n)) 1l
            else 0l
          },(a,b) => a+b, e => true)
        }
      }
      compiler ("nc_getgraphbitset") (Nil :: GraphBitSet) implements getter(0, "_dataBS")
      compiler ("nc_getNodeDataView") (Nil :: NodeDataView(MInt)) implements getter(0, "_dataNV")
      compiler ("nc_gethashset") (Nil :: HashSet(MInt)) implements getter(0, "_dataHS")
      compiler ("nc_gethashset_keydata") (Nil :: NodeDataView(MInt)) implements single ${ NodeDataView(nc_gethashset($self).toArray,0,nc_gethashset($self).length) }
    }
    direct(NodeCollection) ("sumOverCollection", R, CurriedMethodSignature(List(("nc",NodeCollection), ("data",MInt==>R) ,("cond",MInt==>MBoolean)),R), TNumeric(R)) implements composite ${
      if(nc.colType == 0) nc_getgraphbitset(nc).mapreduce[R](data,{(a,b) => a+b},cond)
      else if(nc.colType == 1) nc_getNodeDataView(nc).mapreduce[R](data,{(a,b) => a+b},cond)
      else nc_gethashset_keydata(nc).mapreduce[R](data,{(a,b) => a+b},cond)
    }
    compiler (NodeCollection) ("nc_0", Nil, Nil :: MInt) implements single ${ 0 }
    compiler (NodeCollection) ("nc_1", Nil, Nil :: MInt) implements single ${ 1 }
    compiler (NodeCollection) ("nc_2", Nil, Nil :: MInt) implements single ${ 2 }

    //fake alloc for hash map
    compiler (NodeCollection) ("hs_fake_alloc", Nil, Nil :: HashSet(MInt)) implements single ${ HashSet(array_empty_imm[Int](0)) }
  }
}
