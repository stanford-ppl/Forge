/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Parallel input methods.  Output is currently done
serially.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait IOGraphOps {
  this: OptiGraphDSL =>
  def importIOGraphOps() {
    val IO = grp("GraphIO")
  	val Graph = lookupTpe("Graph")
  	val NodeData = lookupTpe("NodeData")
  	val T = tpePar("T")

    direct (IO) ("writeBCResults", T, (("path",MString),("graph",Graph),("data",NodeData(T))) :: MUnit, TNumeric(T), effect = simple) implements composite ${
    	val ids = $graph.getOrderedNodeIDs
    	writeGraphData($path,ids,data.get_raw_data,$data.nd_length)
    }
    compiler (IO) ("writeGraphData", T, (("path",MString),("ids",MArray(MInt)),("data",MArray(T)),("length",MInt)) :: MUnit, TNumeric(T), effect = simple) implements codegen($cala, ${
      val xfs = new java.io.BufferedWriter(new java.io.FileWriter($path))
      xfs.write("#node id\\tdata\\n")
      for (i <- 0 until $length) {
        xfs.write($ids(i).toString + "\\t")
        xfs.write($data(i).toString + "\\n")
      }
      xfs.close()
    })

  	direct (IO) ("graphFromEdgeList", Nil, MString :: Graph) implements composite ${
      val input_edges = ForgeFileReader.readLines($0)({line =>
        //if(!line.startsWith("#")){
          val fields = line.fsplit("\t")
          pack(fields(0).toInt,fields(1).toInt) 
        //} 
      })

      //FIXME: HACK
      val input_edges_hack = array_buffer_empty[Tup2[Int,Int]](array_length(input_edges))
      var i = 0
      while(i < array_length(input_edges)){
        array_buffer_update(input_edges_hack,i,input_edges(i))
        i += 1
      }
      //contains the input tuples
      val edge_data = NodeData(input_edges_hack)
      /////////////////////////////////////////////////////////////
      //first figure out how many nodes we have and grab them
      val elems = SHashMap[Int,Int]()

      val src_buckets = NodeData[NodeData[Int]](edge_data.nd_length*2)
      val dst_buckets = NodeData[NodeData[Int]](edge_data.nd_length*2)
      var node_count = 0

      edge_data.forloop{ ed =>
        if(!elems.contains(ed._1)){
          elems(ed._1) = node_count
          src_buckets(node_count) = NodeData[Int](0)
          dst_buckets(node_count) = NodeData[Int](0)
          node_count += 1
        }
        if(!elems.contains(ed._2)){
          elems(ed._2) = node_count
          src_buckets(node_count) = NodeData[Int](0)
          dst_buckets(node_count) = NodeData[Int](0)
          node_count += 1
        }
        src_buckets(elems(ed._1)).append(ed._2)
        dst_buckets(elems(ed._2)).append(ed._1)
      }
      src_buckets.resize(node_count)
      dst_buckets.resize(node_count)

      var node_place = 0
      var src_edge_place = 0
      val src_node_array = NodeData[Int](node_count+1)
      val src_edge_array = NodeData[Int](edge_data.nd_length)

      var dst_edge_place = 0
      val dst_node_array = NodeData[Int](node_count+1)
      val dst_edge_array = NodeData[Int](edge_data.nd_length)

      //loops over all node ID's in hash map
      while(node_place < node_count){
        //////////////
        val src_tmp = src_buckets(node_place).map({e => elems(e)})
        src_node_array(node_place+1) = (src_node_array(node_place) + src_tmp.nd_length)
        src_tmp.forloop{ edge =>
          src_edge_array(src_edge_place) = edge
          src_edge_place += 1
        }
        //
        //Forge error?  it seems to never create a symbol for dst_tmp here which is annoying
        val dst_tmp = dst_buckets(node_place).map({e => elems(e)})
        dst_node_array(node_place+1) = dst_buckets(node_place).map({e => elems(e)}).nd_length + dst_node_array(node_place)
        dst_buckets(node_place).map({e => elems(e)}).forloop{ edge =>
          dst_edge_array(dst_edge_place) = edge
          dst_edge_place += 1
        }  
        ////////////////
        node_place += 1
      }
      src_node_array.resize(node_count)
      dst_node_array.resize(node_count)

      val elems_tmp = fhashmap_from_shashmap[Int,Int](elems)
      println("finished file I/O")
      Graph(true,node_count,elems_tmp,src_node_array.get_raw_data,src_edge_array.get_raw_data,dst_node_array.get_raw_data,dst_edge_array.get_raw_data)
    }
  }
}