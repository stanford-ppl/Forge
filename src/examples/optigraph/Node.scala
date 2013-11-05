package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait NodeOps {

  this: OptiGraphDSL =>

  def importNodeOps() {
	//////////////////////////////////////////////////////////////////////////////
		// NODE_DATA DECLARATION
	//////////////////////////////////////////////////////////////////////////////
		//access this node data from the graph class, index in by nodeID 
		/*
		val T = tpePar("T")
		val NodeData = tpe("NodeData",T)
		data(NodeData, ("_length", MInt),("_data",MArray(T)))
		static (NodeData) ("apply", T, MInt :: NodeData(T), effect = mutable) implements allocates(NodeData, ${$0}, ${array_empty[T]($0) } )

		val NodeDataOps = withTpe (NodeData)
		NodeDataOps{
			compiler("node_raw_data")(Nil::MArray(T)) implements getter(0,"_data")
			compiler("update") ( (MInt,T) :: MUnit, effect=write(0) ) implements composite ${
				array_update(node_raw_data($self), $1, $2)
			}
			compiler("apply")(MInt::T) implements composite ${array_apply(node_raw_data($self),$1)}
		}
		*/
	/////////////////////////////////////////////////////////////////////////////
	// NODE DECLARATION
	//////////////////////////////////////////////////////////////////////////////
		//type parameter only needs to be assigned for polymorphic type
		//other than that can just pass Nil in 

		val Node = tpe("Node")
		data(Node, ("_id", MInt))

		//simply give it the arguments you want to allocate space for (no need for length field)
		//order is implicit here, setting the field INT that we allocate to the value that is passed in
		//inner curly brace is what we set the value to {}, outer is what we allocate    
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
