/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all CSRGraph operations.  Glues 
togther all structures and declares CSRGraph operations visible
to user. Inherits from Graph.scala

Data is stored the same as in a directed graph but we only store
out edges. In an undirected graph in=out edges.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait CSRGraphOps{
  this: OptiGraphDSL =>

  def importCSRGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    val NodeSHash = lookupTpe("NodeSHash")

    //Actual CSRGraph declaration
    val CSRGraph = tpe("CSRGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    data(CSRGraph,("_numNodes",MInt),("_externalIDs",MArray(MInt)),("_nodes",MArray(MInt)),("_edges",MArray(MInt))) 
    static(CSRGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt))), CSRGraph))) implements allocates(CSRGraph,${$count},${$exID},${$outNodes},${outEdges})

    val CSRGraphOps = withTpe(CSRGraph)     
    CSRGraphOps{
      //CSRGraph directed or not?
      infix ("numEdges")(Nil :: MInt) implements single ${array_length(edge_raw_dataCSR($self))}

      infix ("isDirected") (Nil :: MBoolean) implements single ${false}
      
      //pulled from graph common ops
      infix ("numNodes")(Nil :: MInt) implements getter(0,"_numNodes")
      infix("sumOverNodes")( (Node==>R) :: R, TNumeric(R), addTpePars=R) implements composite ${
        NodeIdView($self.numNodes).mapreduce[R]( e => $1(Node(e)), (a,b) => a+b, e => true)
      }
      infix ("intersectSets") (Node :: MInt) implements composite ${
        val nbrs = $self.neighbors($1)

        nbrs.mapreduce[Int]({ nbr => 
          if($1.id > nbr) 0
          else{ 
            val nbrsOfNbrs = $self.neighbors(nbr)
            var count = 0

            if(nbrs.length == 0 || nbrsOfNbrs.length == 0) 0
            else if(nbrs(0) > nbrsOfNbrs(nbrsOfNbrs.length-1) || 
              nbrsOfNbrs(0) > nbrs(nbrs.length-1)){
              0
            }
            else{
              $self.simpleIntersect(nbrs,nbrsOfNbrs)
            }
          }
        },(a,b) => a+b, e => true)
      }
      infix ("simpleIntersect") (( ("nbrs",NodeDataView(MInt)),("nbrsOfNbrs",NodeDataView(MInt)) ) :: MInt) implements composite ${
        var i = 0
        var t = 0
        var j = 0
        val small = if(nbrs.length < nbrsOfNbrs.length) nbrs else nbrsOfNbrs
        val large = if(nbrs.length < nbrsOfNbrs.length) nbrsOfNbrs else nbrs
        while(i < small.length  && j < large.length){
          while(large(j) < small(i) && j < large.length){
            j += 1
          }
          if(small(i)==large(j) && j < large.length)              
            t += 1
          i += 1
        }
        //count
        //println("intersect")
        t
      }
      /*
      infix ("simpleIntersect") (( ("nbrs",NodeDataView(MInt)),("nbrsOfNbrs",NodeDataView(MInt)) ) :: MInt) implements composite ${
        var i = 0
        var t = 0
        var j = 0

        while(i < nbrs.length && j < nbrsOfNbrs.length){
          if(nbrs(i)==nbrsOfNbrs(j))              
            t += 1
          if(nbrs(i) < nbrsOfNbrs(j))
            i += 1
          else
            j += 1
        }
        t
      }
      */
      infix ("leapFrogIntersectSets") (Node :: MInt) implements composite ${
        val nbrs = $self.neighbors($1)

        nbrs.mapreduce[Int]({ nbr =>
          if($1.id > nbr) 0
          else{
            val nbrsOfNbrs = $self.neighbors(nbr)
            
            if(nbrs.length == 0 || nbrsOfNbrs.length == 0) 0
            else if(nbrs(0) > nbrsOfNbrs(nbrsOfNbrs.length-1) || 
              nbrsOfNbrs(0) > nbrs(nbrs.length-1)){
              0
            }
            else{
              $self.leapFrogIntersect(nbrs,nbrsOfNbrs)
            }
          }
        },(a,b) => a+b, e => true)
      }
      infix ("intersectHybrid") (Node :: MInt) implements composite ${
        val nbrs = $self.neighbors($1)

        nbrs.mapreduce[Int]({ nbr =>
          if($1.id > nbr) 0
          else{
            val nbrsOfNbrs = $self.neighbors(nbr)
            
            if(nbrs.length == 0 || nbrsOfNbrs.length == 0) 0
            else if(nbrs(0) > nbrsOfNbrs(nbrsOfNbrs.length-1) || 
              nbrsOfNbrs(0) > nbrs(nbrs.length-1)){
              0
            }
            else{
              if( (nbrs.length - nbrsOfNbrs.length) > 10 ||  (nbrs.length - nbrsOfNbrs.length) < -10)
                $self.leapFrogIntersect(nbrs,nbrsOfNbrs)
              else 
                $self.simpleIntersect(nbrs,nbrsOfNbrs)
            }
          }
        },(a,b) => a+b, e => true)
      }
      infix ("leapFrogIntersect") (( ("nbrs",NodeDataView(MInt)),("nbrsOfNbrs",NodeDataView(MInt)) ) :: MInt) implements composite ${
        var t = 0
        var nbrStart = 0
        var nbrOfNbrStart = 0
        var nbrSearch = nbrs(nbrStart) < nbrsOfNbrs(nbrOfNbrStart)
        var done = false
        while(!done){
          done =  nbrStart == nbrs.length-1 || nbrOfNbrStart == nbrsOfNbrs.length-1
          if(nbrSearch){
            nbrStart = $self.binarySearch(nbrs,nbrsOfNbrs(nbrOfNbrStart),nbrStart)
          }
          else{
            nbrOfNbrStart = $self.binarySearch(nbrsOfNbrs,nbrs(nbrStart),nbrOfNbrStart)
          }
          //check to se if we match
          if(nbrs(nbrStart)==nbrsOfNbrs(nbrOfNbrStart)){           
            t += 1
            nbrStart += 1
            nbrOfNbrStart += 1
            if(nbrStart < nbrs.length && nbrOfNbrStart < nbrsOfNbrs.length){
              nbrSearch = nbrs(nbrStart) > nbrsOfNbrs(nbrOfNbrStart)
            }
            else
              done = true
          }
          //if(done) nbrStart = nbrs.length
          nbrSearch = !nbrSearch
        }
        t
      } 
      infix ("binarySearch") ((("a",NodeDataView(MInt)),("key",MInt),("inStart",MInt)) :: MInt) implements composite ${
        // continue searching while [imin,imax] is not empty
        var result = -1
        var end = a.length-1
        var start = inStart
        while (end > start){
          //println("start: " + start + " end: " + end)
          // calculate the midpoint for roughly equal partition
          val imid = ((end-start)/2) + start;
          //println("middle: " + imid)
          if(a(imid) >= key){
            //will this ever happen?
            if(imid-1 < 0){
              end = start-1 // terminate condition 
              result = imid 
            }
            else if(a(imid-1) < key){
              end = start-1 // terminate condition 
              result = imid      
            }
            else end = imid - 1
          }
          else{
            // change min index to search upper subarray
            start = imid + 1
          }
        }
        if(start == end) start else result
      }
      
      infix ("neighbors") (MInt :: NodeDataView(MInt)) implements single ${get_nbrsCSR($self,Node($1))}
      infix ("neighbors") (Node :: NodeDataView(MInt)) implements single ${get_nbrsCSR($self,$1)}
      compiler ("get_nbrsCSR") (Node :: NodeDataView(MInt)) implements single ${
        val start = node_applyCSR($self,$1.id)
        val end = if( ($1.id+1) < array_length(node_raw_dataCSR($self)) ) node_applyCSR($self,($1.id+1))
          else array_length(edge_raw_dataCSR($self))
        NodeDataView[Int](edge_raw_dataCSR($self),start,end-start)
      }

      compiler ("node_raw_dataCSR") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
      compiler("node_applyCSR")(MInt :: MInt) implements single ${array_apply(node_raw_dataCSR($self),$1)}
      compiler ("edge_raw_dataCSR") (Nil :: MArray(MInt)) implements getter(0, "_edges")
      compiler("edge_applyCSR")(MInt :: MInt) implements single ${array_apply(edge_raw_dataCSR($self),$1)}
    }
    //addGraphCommonOps(CSRGraph) 
  } 
}
