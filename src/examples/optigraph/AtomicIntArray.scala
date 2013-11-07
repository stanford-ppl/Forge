package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait AtomicIntArrayOps {

  	this: OptiGraphDSL =>

  	def importAtomicIntArrayOps() {
  		val AtomicIntArray = grp("AtomicIntArray")
    	val AArray = ephemeralTpe("java.util.concurrent.atomic.AtomicIntegerArray")
    	static (AtomicIntArray) ("apply", Nil, MInt :: AArray) implements codegen($cala, ${new java.util.concurrent.atomic.AtomicIntegerArray($0)})
    	direct (AtomicIntArray) ("testAtomic", Nil, (AArray,MInt,MInt) :: MBoolean) implements composite ${
    		if(get($0,$1)==$2){return true}
    		return false
    	}
    	direct (AtomicIntArray) ("get", Nil, (AArray,MInt) :: MInt) implements codegen($cala, ${$0.get($1)})
	    direct (AtomicIntArray) ("testAndSetAtomic", Nil, (AArray,MInt,MInt,MInt) :: MBoolean) implements codegen($cala, ${$0.compareAndSet($1,$2,$3)})
	}
}
