/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Simply store an ID.  That's all a node is!
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package	dsls 
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
		infix("=") (Node :: MBoolean) implements single ${$0.id==$1.id}
	}
  }
}