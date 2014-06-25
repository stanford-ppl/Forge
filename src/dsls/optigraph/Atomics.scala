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
      direct (AtomicIntArray) ("getAndAdd", Nil, (AArray,MInt,MInt) :: MInt, effect = write(0)) implements codegen($cala, ${$0.getAndAdd($1,$2)})
	    direct (AtomicIntArray) ("testAndSetAtomic", Nil, (AArray,MInt,MInt,MInt) :: MBoolean, effect=write(0)) implements codegen($cala, ${$0.compareAndSet($1,$2,$3)})
	    direct (AtomicIntArray) ("set", Nil, (AArray,MInt,MInt) :: MUnit, effect=write(0)) implements codegen($cala, ${$0.set($1,$2)})
  }
}

trait AtomicDoubleArrayOps {
    this: OptiGraphDSL =>
    def importAtomicDoubleArrayOps() {
      val AtomicDoubleArray = grp("AtomicDoubleArray")
      val AArray = ephemeralTpe("com.google.common.util.concurrent.AtomicDoubleArray")
      static (AtomicDoubleArray) ("apply", Nil, MInt :: AArray, effect=mutable) implements codegen($cala, ${new com.google.common.util.concurrent.AtomicDoubleArray($0)})
      direct (AtomicDoubleArray) ("get", Nil, (AArray,MInt) :: MDouble) implements codegen($cala, ${$0.get($1)})
      direct (AtomicDoubleArray) ("getAndAdd", Nil, (AArray,MInt,MDouble) :: MDouble, effect = write(0)) implements codegen($cala, ${$0.getAndAdd($1,$2)})
      direct (AtomicDoubleArray) ("set", Nil, (AArray,MInt,MDouble) :: MUnit, effect=write(0)) implements codegen($cala, ${$0.set($1,$2)})
  }
}

trait AtomicBooleanOps {
    this: OptiGraphDSL =>
    def importAtomicBooleanOps() {
      val AtomicBoolean = grp("AtomicBoolean")
      val ABool = ephemeralTpe("java.util.concurrent.atomic.AtomicBoolean")
      static (AtomicBoolean) ("apply", Nil, MBoolean :: ABool, effect=mutable) implements codegen($cala, ${new java.util.concurrent.atomic.AtomicBoolean($0)})
      direct (AtomicBoolean) ("get", Nil, ABool :: MBoolean) implements codegen($cala, ${$0.get()})
      direct (AtomicBoolean) ("getAndSet", Nil, (ABool,MBoolean) :: MBoolean, effect=write(0)) implements codegen($cala, ${$0.getAndSet($1)})
      direct (AtomicBoolean) ("set", Nil, (ABool,MBoolean) :: MUnit, effect=write(0)) implements codegen($cala, ${$0.set($1)})
  }
}