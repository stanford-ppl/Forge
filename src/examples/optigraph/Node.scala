package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait NodeOps {

  this: OptiGraphDSL =>

  def importNodeOps() {
	/////////////////////////////////////////////////////////////////////////////
	// NODE DECLARATION
	//////////////////////////////////////////////////////////////////////////////
	val Node = tpe("Node")
	data(Node, ("_id", MInt))

	static (Node) ("apply", Nil, MInt :: Node) implements allocates(Node, ${$0})

	val NodeOps = withTpe(Node)
	NodeOps {
		infix("apply") (Nil :: MInt) implements getter(0,"_id")
	}
  }
}
