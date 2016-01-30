/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Stores data asscoicated with nodes in an array 
buffer indexed by internal node IDs
*///////////////////////////////////////////////////////////////

package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait CommunityOps {
  this: OptiGraphDSL =>
  def importCommunityOps() {
    val UndirectedGraph = lookupTpe("UndirectedGraph")
    val NodeData = lookupTpe("NodeData")
    val Node = lookupTpe("Node")
    val NodeIdView = lookupTpe("NodeIdView")
    val Tuple2 = lookupTpe("Tup2")
    val Tuple3 = lookupTpe("Tup3")
    val Tuple5 = lookupTpe("Tup5")
    val Community = tpe("Community")
    val T = tpePar("T")
    val K = tpePar("K")
    val V = tpePar("V")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    data(Community,("_size",MInt),("_precision",MDouble),("_modularity",MDouble),("_canImprove",MBoolean),("_totalWeight",MDouble),("_graph",UndirectedGraph),("_n2c",MArray(MInt)),("_tot",MArray(MDouble)),("_in",MArray(MDouble)),("_n2o",MArray(MInt)))
    static(Community)("apply", Nil, (("g",UndirectedGraph),("precision",MDouble),("n2o",MArray(MInt))) :: Community) implements allocates(Community,${alloc_size(g)},${precision},${unit(0d)},${unit(true)},${alloc_total_weight($0)},${$0},${alloc_ints(alloc_size(g),{e => e})},${alloc_weights(g)},${alloc_selfs(g)},${n2o})
    static(Community)("apply", Nil, (("g",UndirectedGraph),("precision",MDouble)) :: Community) implements composite ${ Community($g, $precision, array_fromfunction[Int]($g.numNodes,e=>e)) }
    static(Community)("apply", Nil, MethodSignature(List(("size",MInt),("precision",MDouble),("modularity",MDouble),("canImprove",MBoolean),("totalWeight",MDouble),("g",UndirectedGraph),("n2c",MArray(MInt)),("tot",MArray(MDouble)),("in",MArray(MDouble)),("n2o",MArray(MInt))),Community)) implements allocates(Community,${size},${precision},${modularity},${canImprove},${totalWeight},${g},${n2c},${tot},${in},${n2o})

    /*
      n2c -> size == numNodes, indexed by nodeID gives the community a node belongs to
      in,tot -> size == # of communities, used for modularity calculation
        tot == total weighted degree of the community
        in == sum of degree of links strictly within the community (divided by 2)
    */

    val CommunityOps = withTpe(Community)
    CommunityOps{  
      infix("modularity")(Nil :: MDouble) implements composite ${
        val g = $self.graph
        val tot = $self.tot
        val in = $self.in
        var m2 = $self.totalWeight
        
        sumOverNodes(g.nodes)({ n => 
          if(tot(n.id) > 0)
            (in(n.id)/m2) - (tot(n.id)/m2)*(tot(n.id)/m2)
          else 
            0d
        })
      }
      infix("modularity")( (("in",MArray(MDouble)),("tot",MArray(MDouble))) :: MDouble) implements composite ${
        val g = $self.graph
        var m2 = $self.totalWeight
        
        sumOverNodes(g.nodes)({ n => 
          if(tot(n.id) > 0)
            (in(n.id)/m2) - (tot(n.id)/m2)*(tot(n.id)/m2)
          else 
            0d
        })
      }
      infix("modularityGain")( (("totc",MDouble),("dnodecomm",MDouble),("w_degree",MDouble)) :: MDouble) implements composite ${
        val degc = w_degree //w_degree
        val m2 = $self.totalWeight  //total weight is really a function of the graph not the comm.
        val dnc = dnodecomm //neighbor weight

        (dnc - totc*degc/m2)
      }
      infix("display")(Nil :: MUnit, effect=simple) implements single ${
        var i = 0
        while(i < $self.size){
          println(" " + i + "/" + $self.n2c(i) + "/" + $self.in(i) + "/" + $self.tot(i))
          i += 1
        }
      }
      infix("display")((("n2c",MArray(MInt)),("in",MArray(MDouble)),("tot",MArray(MDouble))) :: MUnit, effect=simple) implements single ${
        var i = 0
        while(i < $self.size){
          println(" " + i + "/" + n2c(i) + "/" + in(i) + "/" + tot(i))
          i += 1
        }
      }
      infix("generateNewGraph")(Nil :: Tuple2(UndirectedGraph,MArray(MInt))) implements composite ${
        val g = $self.graph
        val n2c = $self.n2c
        val size = $self.size

        val originalNodeIds = NodeData.fromFunction(size,i=>i)
        val groupedComms = originalNodeIds.groupBy(k => n2c(k),v => v)

        val newSize = fhashmap_size(groupedComms)
        val newComms = NodeData.fromFunction(newSize,i => i)
        val oldComms = NodeData(fhashmap_keys(groupedComms))
        
        val old2newHash = fhashmap_from_arrays[Int,Int](oldComms.getRawArray,newComms.getRawArray)

        //go from n20 into n2c
        //from n2c into old2newHash 
        val n2new = array_map[Int,Int]($self.n2o, {o =>
          val oldcomm = n2c(o)
          fhashmap_get(old2newHash,oldcomm)
        })
        //For each edge
          //find src and dst comm (use n2c)
          //if edge between comm exists
            //add edge with weight
          //else
            //add to edge with weight

        //Should be safe parallelism here.
        val newGraph = newComms.map({ src =>
          //println("src: " + src)
          val oldComm = oldComms(src)
          val nodeHash = SHashMap[Int,Double]()

          val nodesInComm = NodeData(fhashmap_get(groupedComms,oldComm))
          nodesInComm.foreach({ n => 
            val (neighbors,nbrWeights) = unpack(g.getNeighborsAndWeights(Node(n)))
            var i = 0

            while(i < neighbors.length){
              val dst = old2newHash(n2c(neighbors(i)))
              if(nodeHash.contains(dst)){
                nodeHash.update(dst,nodeHash(dst)+nbrWeights(i))
              }
              else{
                nodeHash.update(dst,nbrWeights(i))
              }
              i += 1
            }
          })
          nodeHash
        })
        val numEdges = newGraph.mapreduce[Int](a => array_length(a.keys), (a,b) => a+b, a => true)
        val serial_out = $self.assignUndirectedIndicies(newSize,numEdges,newGraph.getRawArray)

        pack(UndirectedGraph(newSize,oldComms.getRawArray,serial_out._1,serial_out._2,serial_out._3),n2new)    
      }

      infix("assignUndirectedIndicies")((("numNodes",MInt),("numEdges",MInt),("src_groups",MArray(SHashMap(MInt,MDouble)))) :: Tuple3(MArray(MInt),MArray(MInt),MArray(MDouble))) implements single ${
        val src_edge_array = NodeData[Int](numEdges)
        val src_edge_weight_array = NodeData[Double](numEdges)
        val src_node_array = NodeData[Int](numNodes)
        var i = 0
        var j = 0
        //I can do -1 here because I am pruning so the last node will never have any neighbors
        while(i < numNodes){
          val neighborhash = src_groups(i)
          val neighborhood = NodeData(neighborhash.keys).sort
          val neighWeights = neighborhood.map(e => neighborhash(e))
          var k = 0
          while(k < neighborhood.length){
            src_edge_array(j) = neighborhood(k)
            src_edge_weight_array(j) = neighWeights(k)
            j += 1
            k += 1
          }
          if(i < numNodes-1){
            src_node_array(i+1) = neighborhood.length + src_node_array(i)
          }
          i += 1
        }
        pack(src_node_array.getRawArray,src_edge_array.getRawArray,src_edge_weight_array.getRawArray)
      }
      //Why do I have to mark the community as mutable still?
      infix("buildNeighboringCommunities")((("n",Node),("n2c",MArray(MInt))) :: SHashMap(MInt,MDouble)) implements single ${
        //////////////////////////////////////////
        //Formerly neigh communities method
        //This section pulls all the communities out of the neighborhoods
        //and sums inter-weights for the neighborhood per community.

        //If you don't use a mutable hash map you will get killed on performance.
        val g = $self.graph
        val commWeights = SHashMap[Int,Double]()
        commWeights.update(n2c(n.id),0d) //Add current nodes community with a weight of 0
        val (neighbors,nbrWeights) = unpack(g.getNeighborsAndWeights(n))
        var i = 0
        while(i < neighbors.length){
          val neigh_comm = n2c(neighbors(i))
          if(neighbors(i) != n.id){
            if(commWeights.contains(neigh_comm)){
              commWeights.update(neigh_comm,commWeights(neigh_comm)+nbrWeights(i))
            }
            else{
              commWeights.update(neigh_comm,nbrWeights(i))
            }
          }
          i += 1
        }
        commWeights
      }
      infix("findBestCommunityMove")((("n",Node),("n2c",MArray(MInt)),("tot",MArray(MDouble)),("commWeights",SHashMap(MInt,MDouble))) :: Tuple2(MInt,MDouble)) implements single ${
        val g = $self.graph
        val node_comm = n2c(n.id) //READ
        val w_degree = g.weightedDegree(n)

        //By default set everything to our current community
        var best_comm = node_comm
        var best_nblinks = commWeights(node_comm)
        var best_increase = $self.modularityGain(tot(node_comm)-g.weightedDegree(n),best_nblinks,w_degree) //READ

        val comms = commWeights.keys
        var i = 0
        while(i < array_length(comms)){
          val neigh_comm = comms(i)
          if(neigh_comm != node_comm){
            val weight = commWeights(neigh_comm)
            val increase = $self.modularityGain(tot(neigh_comm),weight,w_degree) //READ
            if(increase > best_increase){
              best_comm = neigh_comm
              best_nblinks = weight
              best_increase = increase
            }
          }
          i += 1            
        }
        pack(best_comm,best_nblinks) 
      }
      infix("parallelArrayCopy")((("dst",MArray(T)),("src",MArray(T))) :: MUnit, effect=write(1),aliasHint = copies(2), addTpePars = T ) implements composite ${
        NodeIdView(array_length(src)).foreach({i =>
          dst(i) = src(i)
        })
      }
      infix("louvain")(Nil :: Community) implements single ${
        val g = $self.graph
        val totalWeight = $self.totalWeight
        val size = $self.size 

        val tot = array_empty[Double](array_length($self.tot)) 
        val in = array_empty[Double](array_length($self.in)) 
        val n2c = array_empty[Int](array_length($self.n2c)) 

        $self.parallelArrayCopy(tot,$self.tot)  //tot = $self.tot
        $self.parallelArrayCopy(in,$self.in) //in = $self.in
        $self.parallelArrayCopy(n2c,$self.n2c) //n2c = $self.n2c

        /*
        val oldtot = array_empty[Double](array_length($self.tot)) 
        val oldin = array_empty[Double](array_length($self.in)) 
        val oldn2c = array_empty[Int](array_length($self.n2c)) 
        $self.parallelArrayCopy(oldn2c,$self.n2c) //oldn2c = $self.n2c
        $self.parallelArrayCopy(oldin,$self.in) //oldin = $self.in
        $self.parallelArrayCopy(oldtot,$self.tot) //oldtot = $self.tot
        */

        val min_modularity = $self.precision
        var nb_moves = 0
        var nb_pass_done = 0
        var new_mod = $self.modularity
        var cur_mod = new_mod

        var continue = true
        while(continue){
          cur_mod = new_mod
          nb_moves = 0
          nb_pass_done += 1
          //Makes this effectively for each community
          g.foreachNode{ n =>
            val node_comm = n2c(n.id) //READ
            val commWeights = $self.buildNeighboringCommunities(n,n2c)
            val (best_comm,best_nblinks) = unpack($self.findBestCommunityMove(n,n2c,tot,commWeights))

            if(best_comm != node_comm){
              /////////////////////////////////////////////
              ////Should this be atomic.  Challenge for parallelism is here.
              $self.insert(n2c,in,tot,n.id,node_comm,commWeights(node_comm),best_comm,best_nblinks) //WRITE
              /////////////////////////////////////////////
            }
          }//end parallel section
          
          /////////////////////////////////////////
          //Copy new values over for next iteration
          /*
          $self.parallelArrayCopy(oldtot,tot) //oldtot = tot
          $self.parallelArrayCopy(oldin,in) //oldin = in
          $self.parallelArrayCopy(oldn2c,n2c) //oldn2c = n2c
          */
          //////////////////////////////////////////

          new_mod = $self.modularity(in,tot)
          //println("new mod: " + new_mod + " cur_mod: " + cur_mod + " diff: " + (new_mod-cur_mod))
          continue = (new_mod-cur_mod) > min_modularity
        }
        val improvement = (nb_pass_done > 1) || (new_mod != cur_mod)
        println("Number of passes: " + nb_pass_done + " improvement: " + improvement)
        Community(size,$self.precision,new_mod,improvement,totalWeight,g,n2c,tot,in,$self.n2o)
      }

      infix("insert")( MethodSignature(List(("n2c",MArray(MInt)),("in",MArray(MDouble)),("tot",MArray(MDouble)),("node",MInt),("old_comm",MInt),("olddnodecomm",MDouble),("comm",MInt),("dnodecomm",MDouble)),MUnit), effect = write(1,2,3)) implements single ${
        array_update(tot,old_comm,tot(old_comm)-$self.graph.weightedDegree(Node(node)))
        array_update(tot,comm,tot(comm)+$self.graph.weightedDegree(Node(node)))

        array_update(in,old_comm,in(old_comm)-(2*olddnodecomm+$self.graph.numSelfLoops(Node(node))))
        array_update(in,comm,in(comm)+(2*dnodecomm+$self.graph.numSelfLoops(Node(node))))

        array_update(n2c,node,comm)
      }

      infix ("size") (Nil :: MInt) implements getter(0, "_size")
      infix ("precision") (Nil :: MDouble) implements getter(0, "_precision")
      infix ("totalWeight") (Nil :: MDouble) implements getter(0, "_totalWeight")
      infix ("storedModularity") (Nil :: MDouble) implements getter(0, "_modularity")
      infix ("canImprove") (Nil :: MBoolean) implements getter(0, "_canImprove")
      infix ("graph") (Nil :: UndirectedGraph) implements getter(0, "_graph")
      infix ("n2c") (Nil :: MArray(MInt)) implements getter(0, "_n2c")
      infix ("in") (Nil :: MArray(MDouble)) implements getter(0, "_in")
      infix ("tot") (Nil :: MArray(MDouble)) implements getter(0, "_tot")   
      infix ("n2o") (Nil :: MArray(MInt)) implements getter(0, "_n2o")

      infix ("tot") (MInt :: MDouble) implements composite ${array_apply($self.tot, $1)}
      infix ("in") (MInt :: MDouble) implements composite ${array_apply($self.in, $1)}
      infix ("n2c") (MInt :: MInt) implements composite ${array_apply($self.n2c, $1)}
      infix ("n2o") (MInt :: MInt) implements composite ${array_apply($self.n2o, $1)}

      /*
      We might need these when parallelism is added in.
      infix ("updateTot") ( (MInt,MDouble) :: MUnit, effect = write(0)) implements composite ${ array_update($self.tot,$1,$2)}
      infix ("updateIn") ( (MInt,MDouble) :: MUnit, effect = write(0)) implements composite ${ array_update($self.in,$1,$2)}
      infix ("updateN2c") ( (MInt,MInt) :: MUnit, effect = write(0)) implements composite ${ array_update($self.n2c,$1,$2)}

      infix ("setTot") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_tot", quotedArg(1))
      infix ("setIn") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_in", quotedArg(1))
      infix ("setN2c") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_n2c", quotedArg(1))
      */
    }
    compiler (Community) ("alloc_total_weight", Nil, UndirectedGraph :: MDouble) implements single ${$0.totalWeight}
    compiler (Community) ("alloc_size", Nil, UndirectedGraph :: MInt) implements single ${$0.numNodes}
    compiler (Community) ("alloc_doubles", Nil, (MInt,(MInt ==> MDouble)) :: MArray(MDouble)) implements single ${array_fromfunction[Double]($0,$1)}
    compiler (Community) ("alloc_ints", Nil, (MInt,(MInt ==> MInt)) :: MArray(MInt)) implements single ${array_fromfunction[Int]($0,$1)}
    compiler (Community) ("alloc_weights", Nil, UndirectedGraph :: MArray(MDouble)) implements single ${array_fromfunction[Double](alloc_size($0),{n => $0.weightedDegree(Node(n))})}
    compiler (Community) ("alloc_selfs", Nil, UndirectedGraph :: MArray(MDouble)) implements single ${array_fromfunction[Double](alloc_size($0),{n => $0.numSelfLoops(Node(n))})}
  } 
}