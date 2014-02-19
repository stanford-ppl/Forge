/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: A simple container for an atomic integer array.
Used as a bitmap for BFS (could be optimized further).
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait AtomicIntArrayOps {
  	this: OptiGraphDSL =>
  	def importAtomicIntArrayOps() {
  		val AtomicIntArray = grp("AtomicIntArray")
    	val AArray = ephemeralTpe("java.util.concurrent.atomic.AtomicIntegerArray")
    	static (AtomicIntArray) ("apply", Nil, MInt :: AArray, effect=mutable) implements codegen($cala, ${new java.util.concurrent.atomic.AtomicIntegerArray($0)})
    	direct (AtomicIntArray) ("testAtomic", Nil, (AArray,MInt,MInt) :: MBoolean) implements composite ${get($0,$1)==$2}
    	direct (AtomicIntArray) ("get", Nil, (AArray,MInt) :: MInt) implements codegen($cala, ${$0.get($1)})
	    direct (AtomicIntArray) ("testAndSetAtomic", Nil, (AArray,MInt,MInt,MInt) :: MBoolean, effect=write(0)) implements codegen($cala, ${$0.compareAndSet($1,$2,$3)})
	    direct (AtomicIntArray) ("set", Nil, (AArray,MInt,MInt) :: MUnit, effect=write(0)) implements codegen($cala, ${$0.set($1,$2)})
  }
}