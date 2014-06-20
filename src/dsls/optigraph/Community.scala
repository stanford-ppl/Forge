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

    val Community = tpe("Community") 

    data(Community,("_size",MInt),("_neighLast",MInt),("_graph",UndirectedGraph),("_neighWeight",MArray(MDouble)),("_neighPos",MArray(MInt)),("_n2c",MArray(MInt)),("_tot",MArray(MDouble)),("_in",MArray(MDouble)))
    static(Community)("apply", Nil, ("g",UndirectedGraph) :: Community, effect=mutable) implements allocates(Community,${alloc_size(g)},${unit(0)},${$0},${alloc_doubles(alloc_size(g),{e => unit(-1.0)})},${alloc_ints(alloc_size(g),{e => unit(0)})},${alloc_ints(alloc_size(g),{e => e})},${alloc_weights(g)},${alloc_selfs(g)})



    val CommunityOps = withTpe(Community)
    CommunityOps{  
      infix("modularity")(Nil :: MDouble) implements composite ${
        val g = $self.graph
        val tot = $self.tot
        val in = $self.in
        var m2 = g.totalWeight
        
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
      infix("insert")( (("node",MInt),("old_comm",MInt),("olddnodecomm",MDouble),("comm",MInt),("dnodecomm",MDouble)) :: MUnit, effect = simple) implements single ${
        fassert(node >= 0 && node < $self.size, "node must be in range 0 - size")

        //Would really like this to be atomic but lets roll the dice
        $self.updateTot(old_comm,$self.tot(old_comm)-$self.graph.weightedDegree(node))
        $self.updateTot(comm,$self.tot(comm)+$self.graph.weightedDegree(node))

        $self.updateIn(old_comm,$self.in(old_comm)-(2*olddnodecomm+$self.graph.numSelfLoops(node)))
        $self.updateIn(comm,$self.in(comm)+(2*dnodecomm+$self.graph.numSelfLoops(node)))

        $self.updateN2c(node,comm)
      }
      infix("modularityGain")( (("node",MInt),("comm",MInt),("dnodecomm",MDouble),("w_degree",MDouble)) :: MDouble) implements composite ${
        fassert(node >= 0 && node < $self.size, "node must be in range 0 - size")

        val totc = $self.tot(comm).toDouble
        val degc = w_degree
        val m2 = $self.graph.totalWeight  //FIXME -> we shouldn't mapreduce each time
        val dnc = dnodecomm 

        (dnc - totc*degc/m2)
      }
      infix("modularityGainNoMove")( (("node",MInt),("comm",MInt),("dnodecomm",MDouble),("w_degree",MDouble)) :: MDouble) implements composite ${
        fassert(node >= 0 && node < $self.size, "node must be in range 0 - size")

        val totc = $self.tot(comm).toDouble-$self.graph.weightedDegree(node)
        val degc = w_degree
        val m2 = $self.graph.totalWeight  //FIXME -> we shouldn't mapreduce each time
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

      infix("oneLevel")(Nil :: MBoolean, effect = simple) implements single ${
        val g = $self.graph
        val tot = $self.tot
        val in = $self.in
        val size = $self.size 
        val min_modularity = 0.000001

        var improvement = false
        var nb_moves = 0
        var nb_pass_done = 0
        var new_mod = $self.modularity
        var cur_mod = new_mod

        val random_order = array_fromfunction[Int]({size},{e => e})

        var continue = true
        while(continue){
          cur_mod = new_mod
          nb_moves = 0
          nb_pass_done += 1

          //Makes this effectively for each community
          g.foreachNode{ n =>
            var node = n.id
            var node_comm = $self.n2c(node)
            var w_degree = g.weightedDegree(node)

            //////////////////////////////////////////
            //Formerly neigh communities method
            //This section pulls all the communities out of the neighborhoods
            //and sums inter-weights for the neighborhood per community.
            val nbrs = g.neighbors(node)
            var number_of_neighbor_comm = 1

            val comm_weights = SHashMap[Int,Double]()
            comm_weights.update($self.n2c(node),0d)

            //Do computations for node's community to start
            nbrs.foreach({ nbr =>
              val neigh_comm = $self.n2c(nbr)
              if(comm_weights.contains(neigh_comm)){
                comm_weights.update(neigh_comm,comm_weights(neigh_comm)+1d) //FIXME CHANGE TO WEIGHTS
              }
              else{
                comm_weights.update(neigh_comm,1d) //FIXME CHANGE TO WEIGHTS
                number_of_neighbor_comm += 1
              }
            })
            /////////////////////////////////////////
            //$self.remove(node,node_comm,comm_weights(node_comm))

            var best_comm = node_comm
            var best_nblinks = comm_weights(node_comm)
            var best_increase = $self.modularityGainNoMove(node,node_comm,comm_weights(node_comm),w_degree)

            val keys  = comm_weights.keys
            var i = 0
            while(i < number_of_neighbor_comm){
              val neigh_comm = keys(i)
              if(neigh_comm != node_comm){
                val weight = comm_weights(neigh_comm)
                
                val increase = $self.modularityGain(node,neigh_comm,weight,w_degree)
                if(increase > best_increase){
                  best_comm = neigh_comm
                  best_nblinks = weight
                  best_increase = increase
                }
              }
              i += 1            
            } 

            if(best_comm != node_comm){
              nb_moves += 1
              $self.insert(node,node_comm,comm_weights(node_comm),best_comm,best_nblinks)
            }
          } //end parallel section

          new_mod = $self.modularity
          if(nb_moves > 0) improvement = true

          continue = nb_moves > 0 && new_mod-cur_mod > min_modularity
        }
        return improvement
      }
    /*
      n2c -> size == numNodes, indexed by nodeID gives the community a node belongs to
      in,tot -> size == # of communities, used for modularity calculation
        tot == total weighted degree of the community
        in == # links strictly within the community (divided by 2)


      //these two seem thread and node local, let's just compute them on the fly 
      then use them right after computing, no need for this storage  
      neighWeight -> size == # of communities, gives the # of links from node to a indexed comm
      neighPos -> lists the comm for a given nodes neighbors, the 0th entry is the nodes current neighbor
          
    */
      infix("neighComm")( ("node",MInt) :: MUnit, effect=simple) implements single ${
        $self.setNeighWeight(array_fromfunction($self.size,e => -1.0))
        var neigh_last = 1
        $self.updateNeighPos(0,$self.n2c(node))
        $self.updateNeighWeight($self.neighPos(0),0d)

        $self.graph.neighbors(node).foreach{nbr =>
          val neigh_comm = $self.n2c(nbr)
          val neigh_w = 1 //FIXME add in when we have weights
          if(nbr != node){
            if($self.neighWeight(neigh_comm) == -1d){
              $self.updateNeighWeight(neigh_comm,0d)
              $self.updateNeighPos(neigh_last,neigh_comm)
              neigh_last += 1
            }
            $self.updateNeighWeight(neigh_comm,$self.neighWeight(neigh_comm)+neigh_w)
          }
        }
        $self.setNeighLast(neigh_last)
      }

      infix ("size") (Nil :: MInt) implements getter(0, "_size")
      infix ("neighLast") (Nil :: MInt) implements getter(0, "_neighLast")
      infix ("graph") (Nil :: UndirectedGraph) implements getter(0, "_graph")
      infix ("neighWeight") (Nil :: MArray(MDouble)) implements getter(0, "_neighWeight")
      infix ("neighPos") (Nil :: MArray(MInt)) implements getter(0, "_neighPos")
      infix ("n2c") (Nil :: MArray(MInt)) implements getter(0, "_n2c")
      infix ("in") (Nil :: MArray(MDouble)) implements getter(0, "_in")
      infix ("tot") (Nil :: MArray(MDouble)) implements getter(0, "_tot")   
      
      infix ("tot") (MInt :: MDouble) implements composite ${array_apply($self.tot, $1)}
      infix ("in") (MInt :: MDouble) implements composite ${array_apply($self.in, $1)}
      infix ("n2c") (MInt :: MInt) implements composite ${array_apply($self.n2c, $1)}
      infix ("neighWeight") (MInt :: MDouble) implements composite ${array_apply($self.neighWeight, $1)}
      infix ("neighPos") (MInt :: MInt) implements composite ${array_apply($self.neighPos, $1)}

      infix ("updateTot") ( (MInt,MDouble) :: MUnit, effect = write(0)) implements composite ${ array_update($self.tot,$1,$2)}
      infix ("updateIn") ( (MInt,MDouble) :: MUnit, effect = write(0)) implements composite ${ array_update($self.in,$1,$2)}
      infix ("updateN2c") ( (MInt,MInt) :: MUnit, effect = write(0)) implements composite ${ array_update($self.n2c,$1,$2)}
      infix ("updateNeighWeight") ( (MInt,MDouble) :: MUnit, effect = write(0)) implements composite ${ array_update($self.neighWeight,$1,$2)}
      infix ("updateNeighPos") ( (MInt,MInt) :: MUnit, effect = write(0)) implements composite ${ array_update($self.neighPos,$1,$2)}

      infix ("setTot") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_tot", quotedArg(1))
      infix ("setIn") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_in", quotedArg(1))
      infix ("setN2c") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_n2c", quotedArg(1))
      infix ("setNeighWeight") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_neighWeight", quotedArg(1))
      infix ("setNeighPos") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_neighPos", quotedArg(1))
      infix ("setNeighLast") (MInt :: MUnit, effect = write(0)) implements setter(0, "_neighLast", quotedArg(1))

    }
    compiler (Community) ("alloc_size", Nil, UndirectedGraph :: MInt) implements single ${$0.numNodes}
    compiler (Community) ("alloc_doubles", Nil, (MInt,(MInt ==> MDouble)) :: MArray(MDouble)) implements single ${array_fromfunction[Double]($0,$1)}
    compiler (Community) ("alloc_ints", Nil, (MInt,(MInt ==> MInt)) :: MArray(MInt)) implements single ${array_fromfunction[Int]($0,$1)}
    compiler (Community) ("alloc_weights", Nil, UndirectedGraph :: MArray(MDouble)) implements single ${array_fromfunction[Double](alloc_size($0),{n => $0.weightedDegree(n)})}
    compiler (Community) ("alloc_selfs", Nil, UndirectedGraph :: MArray(MDouble)) implements single ${array_fromfunction[Double](alloc_size($0),{n => $0.numSelfLoops(n)})}

  } 
}