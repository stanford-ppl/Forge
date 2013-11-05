package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait SyncIntArrayOps {

  	this: OptiGraphDSL =>

  	def importSyncIntArrayOps() {
		val SyncIntArray = tpe("SyncIntArray")
		data(SyncIntArray)

		direct (SyncIntArray) ("apply", Nil, MInt::SyncIntArray, effect=mutable) implements codegen($cala, ${
			new java.util.concurrent.atomic.AtomicIntegerArray($0)
		})

		val SyncIntArrayOps = withTpe(SyncIntArray) 
		SyncIntArrayOps{
			//infix("test") ( MInt :: MInt) implements codegen($cala, ${
			//	$0.get($1)
			//})
		}
	}
}
