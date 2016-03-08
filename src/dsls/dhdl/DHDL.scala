package ppl.dsl.forge
package dsls
package dhdl

import core.{ForgeApplication,ForgeApplicationRunner}

object DHDLDSLRunner extends ForgeApplicationRunner with DHDLDSL

trait DHDLDSL extends ForgeApplication
	with DHDLMath with DHDLMisc with DHDLTypes with DHDLMemories
	with DHDLControllers with DHDLMetadata with DHDLEnums {

  def dslName = "DHDL"

  override def addREPLOverride = false
  override def clearTraversals = true

  def specification() = {
		//disableSoA()
		//disableStructUnwrapping()
		disableFusion()

    // --- Primitive Types
    val Bit = tpe("Bit")
    val Fix = tpe("Fix")
    val Flt = tpe("Flt")
    primitiveTypes :::= List(Bit, Fix, Flt)

    // --- Memory Types
    val T = tpePar("T")
    val OffChip = tpe("OffChipMem", T)
    val BRAM = tpe("BRAM", T)
    val Reg = tpe("Reg", T)
    primitiveTypes :::= List(OffChip, BRAM, Reg)

    // --- State Machine Types
    val Counter = tpe("Counter")
    val CounterChain = tpe("CounterChain")
    val Pipeline = tpe("Pipeline")
    primitiveTypes :::= List(Counter, CounterChain, Pipeline)

    // --- Other Types
    val Indices = tpe("Indices")
    val LoopRange = tpe("LoopRange")

    // Scala.scala imports
    importTuples()
    importStrings()

    // DSL spec imports
    importIndices()
		importDHDLTypes()
    importDHDLEnums()
    importDHDLMetadata()
    importDHDLMath()
		importDHDLMemories()
    //importCounters()
    //importCounterChains()
		importDHDLControllers()
		importDHDLMisc()

    schedule(IRPrinterPlus)

    // Externs
    extern(grp("PipeTemplate"), targets = List($cala))
    extern(grp("MemoryTemplate"), targets = List($cala))
		()
	}
}
