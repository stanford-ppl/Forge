/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Simply store an ID.  That's all a node is!
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait NodeOps {
  this: OptiGraphDSL =>
  def importNodeOps() {
    val Node = tpe("Node")
    data(Node, ("_id", MInt))
    static (Node) ("apply", Nil, MInt :: Node) implements allocates(Node, ${$0})
    val NodeOps = withTpe(Node)
    NodeOps {
      infix("id") (Nil :: MInt) implements getter(0,"_id")
      infix(">") (Node :: MBoolean) implements single ${$0.id>$1.id}
      infix("<") (Node :: MBoolean) implements single ${$0.id<$1.id}  
      infix("==") (Node :: MBoolean) implements single ${$0.id==$1.id}
    }
  }
}

trait EdgeOps {
  this: OptiGraphDSL =>    
  def importEdgeOps() {
    val Node = lookupTpe("Node")
    val Edge = tpe("Edge")
    data(Edge, ("_nodeFrom", Node),("_nodeTo",Node))
    static (Edge) ("apply", Nil, (Node,Node) :: Edge) implements allocates(Edge, ${$0}, ${$1})
    val EdgeOps = withTpe(Edge)
    EdgeOps{
      infix("fromNode") (Nil :: Node) implements getter(0,"_nodeFrom")
      infix("toNode") (Nil :: Node) implements getter(0,"_nodeTo")
    }
  }
}