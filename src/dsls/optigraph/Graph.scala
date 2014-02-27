/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all Graph operations.  Glues 
togther all structures and declares Graph operations visible
to user.

Common operations for both directed and undirected graphs.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait GraphOps{
  this: OptiGraphDSL =>

  def addGraphCommonOps(g: Rep[DSLType]) {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")

    //Actual Graph declaration
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    val Graph = g
    val GraphCommonOps = withTpe(Graph)
    GraphCommonOps{
      infix ("getHeavyNodeHash") (Nil :: SHashMap(MInt,MInt)) implements getter(0, "_heavyNodes")
      infix ("isHeavy") (Node :: MBoolean) implements single ${$self.getHeavyNodeHash.contains($1.id)}//fhashmap_contains($self.getHeavyNodeHash,$1.id)}
      infix ("numHeavy") (Nil :: MInt) implements single ${array_length($self.getHeavyNodeHash.keys)}

      infix("mapLoadBalancedNodes")( (Node==>R) :: NodeData(R), TNumeric(R), addTpePars=R) implements composite ${
        //parallel on from function 
        val heavy = $self.getHeavyNodeHash
        val data = array_buffer_strict_empty[R]($self.numNodes) 
        array_buffer_forIndices(data,{n => 
          if(!heavy.contains(n)) array_buffer_update(data,n,$1(Node(n)))
          //if(!fhashmap_contains(heavy,n)) array_buffer_update(data,n,$1(Node(n)))
          else array_buffer_update(data,n,numeric_zero[R])
        })

        //parallel on function passed in
        val keys = heavy.keys//fhashmap_keys(heavy)
        var i = 0
        while(i < array_length(keys)){
          array_buffer_update(data,array_apply(keys,i),$1(Node(array_apply(keys,i))))
          i+=1
        }
        NodeData(data)
      }
      //given an ID return a node
      infix("getNodeFromID")(MInt :: Node) implements composite ${
        val result = NodeIdView($self.numNodes).mapreduce[Int]( i => i, (a,b) => a+b, i => $self.getExternalID(i)==$1)
        if(result >= $self.numNodes() || result < 0) fatal("ERROR. ID: " + $1 + " does not exist in this UndirectedGraph!")
        Node(result)
      }
      infix ("numNodes")(Nil :: MInt) implements getter(0,"_numNodes")
      infix ("foreachNode") ((Node ==> MUnit) :: MUnit, effect = simple) implements composite ${
        NodeData(array_fromfunction($self.numNodes,{n => n})).foreach{ i =>
          $1(Node(i))
        }
      }
      infix("mapNodes")( (Node==>R) :: NodeData(R), addTpePars=R) implements composite ${
        NodeData[R](array_fromfunction($self.numNodes,{n => $1(Node(n))}))
      }
      infix("sumOverNodes")( (Node==>R) :: R, TNumeric(R), addTpePars=R) implements composite ${
        NodeIdView($self.numNodes).mapreduce[R]( e => $1(Node(e)), (a,b) => a+b, e => true)
      }
      infix("sumSkewOverNodes")( (Node==>R) :: R, TNumeric(R), addTpePars=R) implements composite ${
        val a = NodeIdView($self.numNodes).mapreduce[R]( e => $1(Node(e)), (a,b) => a+b, e => !$self.isHeavy(Node(e))) 
        var b = numeric_zero[R]
        NodeIdView($self.numNodes).forEachSerial{e => 
          b += $1(Node(e))
        }
        a + b
      }
      infix ("getExternalIDs") (Nil :: MArray(MInt)) implements getter(0, "_externalIDs")
      infix ("getExternalID") (MInt :: MInt) implements single ${array_apply($self.getExternalIDs,$1)}
      //perform BF traversal
      infix ("inBFOrder") ( CurriedMethodSignature(List(Node,((Node,NodeData(R),NodeData(MInt)) ==> R),((Node,NodeData(R),NodeData(R),NodeData(MInt)) ==> R)),NodeData(R)), TFractional(R), addTpePars=R, effect=simple) implements composite ${
        val levelArray = NodeData[Int]($self.numNodes)
        val bitMap = AtomicIntArray($self.numNodes)
        val nodes = NodeIdView($self.numNodes) 
        val forwardComp = NodeData[R]($self.numNodes)
        val reverseComp = NodeData[R]($self.numNodes)

        levelArray($1.id) = 1
        set(bitMap,$1.id,1)
        
        //error: illegal sharing of mutable objects Sym(2472 at Sym(2473)=Reflect(NewVar(Sym(2472)),Summary(false,false,false,false,true,false,List(Sym(2472)),List(Sym(2472)),List(),List()),List(Sym(2472)))
        //var finished = AtomicBoolean(false)
        var finished = false

        var level = 1
 
        while(!finished){//!getAndSet(finished,true)){
          finished = true
          nodes.foreach{n =>  
            if(levelArray(n) == level){
              val neighbor = $self.outNbrs(Node(n))
              neighbor.foreach{nghbr =>
                if(testAtomic(bitMap,nghbr,0)){
                  if(testAndSetAtomic(bitMap,nghbr,0,1)){
                    levelArray(nghbr) = level+1
                    finished = false//set(finished,false)
              }}}//end nghbr for each 
              forwardComp(n) = $2(Node(n),forwardComp,levelArray)
            }
          }//end nodes for each
          level += 1
        }//end while

        val rBFS = true
        ///reverse BFS
        while( level>=1 ){
          nodes.foreach{n =>
            if(levelArray(n) == level){
              reverseComp(n) = $3(Node(n),forwardComp,reverseComp,levelArray)
            }
          }
          level -= 1
        }
        NodeData(reverseComp.getRawArrayBuffer)
      }
    }
  }
  //have to split this up from 
  def importGraphAggregateOps(){
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")

    //Actual Graph declaration
    val T = tpePar("T")
    val R = tpePar("R")
    val Graph = tpePar("Graph")
    //math_object_abs only works for a type of Double
    direct(Graph) ("abs", Nil, MDouble :: MDouble) implements single ${math_object_abs($0)}
    direct(Graph) ("abs", Nil, NodeData(MDouble) :: NodeData(MDouble)) implements composite ${$0.map(e => math_object_abs(e))}
    direct(Graph) ("abs", Nil, MFloat :: MFloat) implements single ${if($0 > 0) $0 else $0 * -1}
    direct(Graph) ("abs", Nil, NodeData(MFloat) :: NodeData(MFloat)) implements composite ${$0.map(e => abs(e))}

    //a couple of sum methods
    direct(Graph) ("sum", R, NodeData(R) :: R, TNumeric(R)) implements composite ${$0.reduce((a,b) => a+b)}
    direct(Graph) ("sum", R, CurriedMethodSignature(List(("nd_view",NodeDataView(MInt)), ("data",MInt==>R) ,("cond",MInt==>MBoolean)),R), TNumeric(R)) implements composite ${nd_view.mapreduce[R]( e => data(e), (a,b) => a+b, cond)}
    direct(Graph) ("sum", R, NodeData(NodeData(R)) :: NodeData(R), TFractional(R)) implements composite ${$0.reduceNested( ((a,b) => a+b),NodeData[R]($0.length))}
    
    // "block" should not mutate the input, but always produce a new copy. in this version, block can change the structure of the input across iterations (e.g. increase its size)
    direct (Graph) ("untilconverged", T, CurriedMethodSignature(List(List(("x", T), ("tol", MDouble, ".0001"), ("minIter", MInt, "1"), ("maxIter", MInt, "100")), ("block", T ==> T), ("diff", (T,T) ==> MDouble)), T)) implements composite ${
      var delta = scala.Double.MaxValue
      var cur = x
      var iter = 0

      while ((math_object_abs(delta) > tol && iter < maxIter) || iter < minIter) {
        val prev = cur
        val next = block(cur)
        iter += 1
        delta = diff(prev,next)
        cur = next
      }
      println("Number of Iterations: " + iter)
      if (iter == maxIter){
        println("Maximum iterations exceeded")
      }
      cur
    }
  } 
}
