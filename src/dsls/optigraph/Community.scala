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
    val T = tpePar("T")
    val UndirectedGraph = lookupTpe("UndirectedGraph")
    val NodeData = lookupTpe("NodeData")
    val Tuple3 = lookupTpe("Tup3")
    val Tuple5 = lookupTpe("Tup5")
    val Community = tpe("Community")
    val K = tpePar("K")
    val V = tpePar("V")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    data(Community,("_size",MInt),("_totalWeight",MDouble),("_graph",UndirectedGraph),("_neighWeight",MArray(MDouble)),("_n2c",MArray(MInt)),("_tot",MArray(MDouble)),("_in",MArray(MDouble)))
    static(Community)("apply", Nil, ("g",UndirectedGraph) :: Community) implements allocates(Community,${alloc_size(g)},${alloc_total_weight($0)},${$0},${alloc_doubles(alloc_size(g),{e => unit(-1.0)})},${alloc_ints(alloc_size(g),{e => e})},${alloc_weights(g)},${alloc_selfs(g)})
    //FIXME
    //lots of errors removed but wrong answer when effect = mutable is taken out
    

    /*
      n2c -> size == numNodes, indexed by nodeID gives the community a node belongs to
      in,tot -> size == # of communities, used for modularity calculation
        tot == total weighted degree of the community
        in == sum of degree of links strictly within the community (divided by 2)
    */

    val CommunityOps = withTpe(Community)
    CommunityOps{  
      infix("modularity")(Nil :: MDouble) implements composite ${
        //$self.display
        val g = $self.graph
        val tot = $self.tot
        val in = $self.in
        var m2 = $self.totalWeight
        
        g.sumOverNodes({ n => 
          if(tot(n.id) > 0)
            (in(n.id)/m2) - (tot(n.id)/m2)*(tot(n.id)/m2)
          else 
            0d
        })
      }
      infix("remove")( (("node",MInt),("comm",MInt),("dnodecomm",MDouble)) :: MUnit, effect = simple) implements single ${
        fassert(node >= 0 && node < $self.size, "node must be in range 0 - size")

        $self.updateTot(comm,$self.tot(comm)-$self.graph.weightedDegree(node))
        $self.updateIn(comm,$self.in(comm)-(2*dnodecomm+$self.graph.numSelfLoops(node)))
        $self.updateN2c(node,-1)
      }
      infix("modularityGain")( (("node",MInt),("comm",MInt),("dnodecomm",MDouble),("w_degree",MDouble)) :: MDouble) implements composite ${
        fassert(node >= 0 && node < $self.size, "node must be in range 0 - size")

        val totc = $self.tot(comm).toDouble
        val degc = w_degree
        val m2 = $self.totalWeight  //FIXME -> we shouldn't mapreduce each time
        val dnc = dnodecomm 

        (dnc - totc*degc/m2)
      }
      infix("modularityGainNoMove")( (("node",MInt),("comm",MInt),("dnodecomm",MDouble),("w_degree",MDouble)) :: MDouble) implements composite ${
        fassert(node >= 0 && node < $self.size, "node must be in range 0 - size")

        val totc = $self.tot(comm).toDouble-$self.graph.weightedDegree(node)
        val degc = w_degree
        val m2 = $self.totalWeight  //FIXME -> we shouldn't mapreduce each time
        val dnc = dnodecomm 

        (dnc - totc*degc/m2)
      }
      infix("display")(Nil :: MUnit, effect=simple) implements single ${
        var i = 0
        while(i < $self.size){
          println(" " + i + "/" + $self.n2c(i) + "/" + $self.in(i) + "/" + $self.tot(i))
          i += 1
        }
      }
      infix("generateNewGraph")(Nil :: UndirectedGraph) implements composite ${
        val g = $self.graph
        val tot = $self.tot
        val in = $self.in
        val n2c = $self.n2c
        val size = $self.size

        val originalNodeIds = NodeData.fromFunction(size,i=>i)
        val groupedComms = originalNodeIds.groupBy(k => n2c(k),v => v)

        val newSize = fhashmap_size(groupedComms)
        val newComms = NodeData.fromFunction(newSize,i => i)
        val oldComms = NodeData(fhashmap_keys(groupedComms))
        
        val old2newHash = fhashmap_from_arrays[Int,Int](oldComms.getRawArray,newComms.getRawArray)

        //For each edge
          //find src and dst comm (use n2c)
          //if edge between comm exists
            //add edge with weight
          //else
            //add to edge with weight

        //Should be safe parallelism here.
        //println(fhashmap_size(old2newHash))
        val newGraph = newComms.map({ src =>
          //println("src: " + src)
          val oldComm = oldComms(src)
          val nodeHash = SHashMap[Int,Double]()

          val nodesInComm = NodeData(fhashmap_get(groupedComms,oldComm))
          nodesInComm.foreach({ n => 
            val (nbrs,nbrWeights) = unpack(g.getNeighborsAndWeights(Node(n)))
            var i = 0

            //println("len: " + nbrs.length)
            while(i < nbrs.length){
              val dst = old2newHash(n2c(nbrs(i)))
              //println("dst: " + dst)
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

        UndirectedGraph(newSize,oldComms.getRawArray,serial_out._1,serial_out._2,serial_out._3)    
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
      infix("oneLevelNotFunctional")(Nil :: Community, effect = write(0)) implements composite ${
        val g = $self.graph
        val tot = $self.tot
        val in = $self.in
        val size = $self.size 
        val min_modularity = 0.000001

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
            var node = n.id
            val node_comm = $self.n2c(node)
            var w_degree = g.weightedDegree(node)

            //////////////////////////////////////////
            //Formerly neigh communities method
            //This section pulls all the communities out of the neighborhoods
            //and sums inter-weights for the neighborhood per community.
            
            val (nbrs,nbrWeights) = unpack(g.getNeighborsAndWeights(n))
            val neighPos = array_empty[Int](nbrs.length+1) //holds indexes in for comm weights (neighbors plus current node)
            //FIXME relies on fact that array_empty gives and array of 0's
            val commWeights = array_empty[Double](size) //holds comm weights for neighborhoods            neighPos(0) = node_comm

            var i = 0
            //println("nbr len: " + nbrs.length)
            while(i < nbrs.length){
              val neigh_comm = $self.n2c(nbrs(i))
              neighPos(i+1) = neigh_comm
              commWeights(neigh_comm) = commWeights(neigh_comm)+nbrWeights(i)
              i += 1
            }
            /////////////////////////////////////////
            //By default set everything to our current community
            var best_comm = node_comm
            var best_nblinks = commWeights(node_comm)
            var best_increase = $self.modularityGainNoMove(node,node_comm,commWeights(node_comm),w_degree)

            //println("number of comms: " + array_length(neighPos))
            i = 1 //we already did our own node above
            while(i < array_length(neighPos)){
              //println("comm: " + neighPos(i) + " weight: " + commWeights(neighPos(i)))
              val neigh_comm = neighPos(i)
              val weight = commWeights(neigh_comm)
              
              val increase = $self.modularityGain(node,neigh_comm,weight,w_degree)
              if(increase > best_increase){
                best_comm = neigh_comm
                best_nblinks = weight
                best_increase = increase
              }
              i += 1            
            } 

            if(best_comm != node_comm){
              nb_moves += 1
              $self.insert(node,node_comm,commWeights(node_comm),best_comm,best_nblinks)
            }
          }//end parallel section

          new_mod = $self.modularity
          //println("new mod: " + new_mod)
          continue = nb_moves > 0 && new_mod-cur_mod > min_modularity
        }
        println("Number of passes: " + nb_pass_done)
        return $self
      }
      infix("oneLevel")(Nil :: MDouble, effect = simple) implements composite ${
        val g = $self.graph
        val tot = $self.tot
        val in = $self.in
        val size = $self.size 
        val min_modularity = 0.000001

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

          val newData = g.mapNodes[Tup5[Int,Double,Int,Double,Double]]{ n =>
            var node = n.id
            val node_comm = $self.n2c(node)
            var w_degree = g.weightedDegree(node)

            //////////////////////////////////////////
            //Formerly neigh communities method
            //This section pulls all the communities out of the neighborhoods
            //and sums inter-weights for the neighborhood per community.
            val (nbrs,nbrWeights) = unpack(g.getNeighborsAndWeights(n))

            val neighPos = array_empty[Int](nbrs.length+1) //holds indexes in for comm weights (neighbors plus current node)
            //FIXME relies on fact that array_empty gives and array of 0's
            val commWeights = array_empty[Double](size) //holds comm weights for neighborhoods
            neighPos(0) = node_comm

            var i = 0
            while(i < nbrs.length){
              val neigh_comm = $self.n2c(nbrs(i))
              neighPos(i+1) = neigh_comm
              commWeights(neigh_comm) = commWeights(neigh_comm)+nbrWeights(i)
              i += 1
            }
            
            /////////////////////////////////////////
            //By default set everything to our current community
            var best_comm = node_comm
            var best_nblinks = commWeights(node_comm)
            var best_increase = $self.modularityGainNoMove(node,node_comm,commWeights(node_comm),w_degree)

            //println("number of comms: " + array_length(keys))
            i = 1 //we already did our own node above
            while(i < array_length(neighPos)){
              //println("comm: " + keys(i) + " weight: " + comm_weights(keys(i)))
              val neigh_comm = neighPos(i)
              val weight = commWeights(neigh_comm)
              
              val increase = $self.modularityGain(node,neigh_comm,weight,w_degree)
              if(increase > best_increase){
                best_comm = neigh_comm
                best_nblinks = weight
                best_increase = increase
              }
              i += 1            
            } 
            nb_moves += 1
            val dnodecomm = unit(2)*best_nblinks+$self.graph.numSelfLoops(n.id)
            val olddnodecomm =unit(2)*commWeights(node_comm)+$self.graph.numSelfLoops(n.id)
            pack(best_comm,dnodecomm,node_comm,olddnodecomm,$self.graph.weightedDegree(n.id))
          }//end map nodes
          $self.setN2c(newData.map({i => i._1}).getRawArray)

          //Filter out data where nodes didn't move communities
          val changedData = newData.filter({i => i._1 != i._3},{i=>i})
          val newcommupdatehash = changedData.groupBy(i => i._1,{i => i})
          val oldcommupdatehash = changedData.groupBy(i => i._3,{i => i})

          val newcomms = NodeData(fhashmap_keys(newcommupdatehash))
          val oldcomms = NodeData(fhashmap_keys(oldcommupdatehash))

          val newdnodecomm = newcomms.map{e => 
            NodeData(fhashmap_get(newcommupdatehash,e)).mapreduce[Double](i => i._2,(a,b) => a+b,i=>true)
          }
          val olddnodecomm = oldcomms.map{e => 
            NodeData(fhashmap_get(oldcommupdatehash,e)).mapreduce[Double](i => i._4,(a,b) => a+b,i=>true)
          }
          val newweights = newcomms.map{e => 
            NodeData(fhashmap_get(newcommupdatehash,e)).mapreduce[Double](i => i._5,(a,b) => a+b,i=>true)
          }
          val oldweights = oldcomms.map{e =>
            NodeData(fhashmap_get(oldcommupdatehash,e)).mapreduce[Double](i => i._5,(a,b) => a+b,i=>true) 
          }

          NodeIdView(newcomms.length).foreach{ i =>
            $self.updateIn(newcomms(i),$self.in(newcomms(i))+newdnodecomm(i))
            $self.updateTot(newcomms(i),$self.in(newcomms(i))+newweights(i))

          }
          NodeIdView(oldcomms.length).foreach{ i =>
            $self.updateIn(oldcomms(i),$self.in(oldcomms(i))-olddnodecomm(i))
            $self.updateTot(oldcomms(i),$self.in(oldcomms(i))+oldweights(i))
          }

          //$self.display

          new_mod = $self.modularity
          continue = nb_moves > 0 && new_mod-cur_mod > min_modularity
        }
        println("Number of passes: " + nb_pass_done)
        return new_mod
      }
      infix("insert")( (("node",MInt),("old_comm",MInt),("olddnodecomm",MDouble),("comm",MInt),("dnodecomm",MDouble)) :: MUnit, effect = simple) implements composite ${
        fassert(node >= 0 && node < $self.size, "node must be in range 0 - size")

        //Would really like this to be atomic but lets roll the dice
        $self.updateTot(old_comm,$self.tot(old_comm)-$self.graph.weightedDegree(node))
        $self.updateTot(comm,$self.tot(comm)+$self.graph.weightedDegree(node))

        $self.updateIn(old_comm,$self.in(old_comm)-(2*olddnodecomm+$self.graph.numSelfLoops(node)))
        $self.updateIn(comm,$self.in(comm)+(2*dnodecomm+$self.graph.numSelfLoops(node)))

        $self.updateN2c(node,comm)
      }
      infix ("size") (Nil :: MInt) implements getter(0, "_size")
      infix ("totalWeight") (Nil :: MDouble) implements getter(0, "_totalWeight")
      infix ("graph") (Nil :: UndirectedGraph) implements getter(0, "_graph")
      infix ("neighWeight") (Nil :: MArray(MDouble)) implements getter(0, "_neighWeight")
      infix ("n2c") (Nil :: MArray(MInt)) implements getter(0, "_n2c")
      infix ("in") (Nil :: MArray(MDouble)) implements getter(0, "_in")
      infix ("tot") (Nil :: MArray(MDouble)) implements getter(0, "_tot")   
      
      infix ("tot") (MInt :: MDouble) implements composite ${array_apply($self.tot, $1)}
      infix ("in") (MInt :: MDouble) implements composite ${array_apply($self.in, $1)}
      infix ("n2c") (MInt :: MInt) implements composite ${array_apply($self.n2c, $1)}
      infix ("neighWeight") (MInt :: MDouble) implements composite ${array_apply($self.neighWeight, $1)}

      infix ("updateTot") ( (MInt,MDouble) :: MUnit, effect = write(0)) implements composite ${ array_update($self.tot,$1,$2)}
      infix ("updateIn") ( (MInt,MDouble) :: MUnit, effect = write(0)) implements composite ${ array_update($self.in,$1,$2)}
      infix ("updateN2c") ( (MInt,MInt) :: MUnit, effect = write(0)) implements composite ${ array_update($self.n2c,$1,$2)}
      infix ("updateNeighWeight") ( (MInt,MDouble) :: MUnit, effect = write(0)) implements composite ${ array_update($self.neighWeight,$1,$2)}

      infix ("setTot") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_tot", quotedArg(1))
      infix ("setIn") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_in", quotedArg(1))
      infix ("setN2c") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_n2c", quotedArg(1))
      infix ("setNeighWeight") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_neighWeight", quotedArg(1))
    }
    compiler (Community) ("alloc_total_weight", Nil, UndirectedGraph :: MDouble) implements single ${$0.totalWeight}
    compiler (Community) ("alloc_size", Nil, UndirectedGraph :: MInt) implements single ${$0.numNodes}
    compiler (Community) ("alloc_doubles", Nil, (MInt,(MInt ==> MDouble)) :: MArray(MDouble)) implements single ${array_fromfunction[Double]($0,$1)}
    compiler (Community) ("alloc_ints", Nil, (MInt,(MInt ==> MInt)) :: MArray(MInt)) implements single ${array_fromfunction[Int]($0,$1)}
    compiler (Community) ("alloc_weights", Nil, UndirectedGraph :: MArray(MDouble)) implements single ${array_fromfunction[Double](alloc_size($0),{n => $0.weightedDegree(n)})}
    compiler (Community) ("alloc_selfs", Nil, UndirectedGraph :: MArray(MDouble)) implements single ${array_fromfunction[Double](alloc_size($0),{n => $0.numSelfLoops(n)})}

  } 
}