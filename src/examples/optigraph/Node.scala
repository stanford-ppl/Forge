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

	static (Node) ("apply", Nil, MInt :: Node, effect=mutable) implements allocates(Node, ${$0})

	val NodeOps = withTpe(Node)
	NodeOps {
	//0th argument is explicit as itself (this.)

		//apply versus update
		//see below one desugars into a getter, other desugars into a setter
		//MUnit is equivalent to void
		compiler("update") (MInt :: MUnit, effect=write(0)) implements setter(0,"_id",${$1})
		//0 is the argument you want to call the getter on 
		infix("apply") (Nil :: MInt) implements getter(0,"_id")

		//infix("set") ( NodeData :: Nil) implements composite ${

		//}
		//infix("get")(T::T) implements composite ${
		//  NodeData[T] = $1 
		//}
	}
  }
}
