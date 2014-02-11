/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Simply store node ID's for two nodes to represent
a graph edge.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

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