package ppl.dsl.forge
package dsls
package dhdl

import core.{ForgeApplication,ForgeApplicationRunner}

object DHDLDSLRunner extends ForgeApplicationRunner with DHDLDSL

trait DHDLDSL extends ForgeApplication
  with DHDLMath with DHDLMisc with DHDLTypes with DHDLMemories
  with DHDLControllers with DHDLMetadata with DHDLEnums with DHDLSugar {

  def dslName = "DHDL"

  override def addREPLOverride = false
  override def clearTraversals = true

  lazy val S = tpePar("S")
  lazy val I = tpePar("I")
  lazy val F = tpePar("F")
  lazy val G = tpePar("G")
  lazy val E = tpePar("E")

  def specification() = {
    disableFusion()

    val T = tpePar("T")

    // --- Primitive Types
    val Bit = tpe("Bit")
    val FixPt = tpe("FixPt", (S,I,F))    // sign, integer, and fraction
    val FltPt = tpe("FltPt", (G,E))      // significand and exponent
    primitiveTypes :::= List(Bit, FixPt, FltPt)

    // --- Type parameters
    val Signed = tpe("Signed", stage=compile)
    val Unsign = tpe("Unsign", stage=compile)
    (0 to 64).foreach{i => tpe("B" + i, stage=compile) } // B0 - B64

    // --- Common Type Aliases
    // Add more as needed
    val B0 = lookupTpe("B0", compile)
    val B6 = lookupTpe("B6", compile)
    val B9 = lookupTpe("B9", compile)
    val B10 = lookupTpe("B10", compile)
    val B12 = lookupTpe("B12", compile)
    val B23 = lookupTpe("B23", compile)
    val B32 = lookupTpe("B32", compile)
    val B52 = lookupTpe("B52", compile)

    val SInt32 = tpeAlias("SInt", FixPt(Signed, B32, B0))  // Note: This is not a scala Int, this is a signed int!
    val UInt32 = tpeAlias("UInt", FixPt(Unsign, B32, B0))
    val Half   = tpeAlias("Half", FltPt(B6, B10))
    val Flt    = tpeAlias("Flt",  FltPt(B9, B23))
    val Dbl    = tpeAlias("Dbl",  FltPt(B12, B52))

    // --- Memory Types
    val OffChip = tpe("OffChipMem", T)
    val Tile    = tpe("Tile", T)
    val BRAM    = tpe("BRAM", T)
    val Reg     = tpe("Reg", T)
    primitiveTypes :::= List(OffChip, BRAM, Reg)


    // --- State Machine Types
    val Counter = tpe("Counter")
    val CounterChain = tpe("CounterChain")
    val Pipeline = tpe("Pipeline")
    primitiveTypes :::= List(Counter, CounterChain, Pipeline)


    // --- Other Types
    val Indices   = tpe("Indices")
    val LoopRange = tpe("LoopRange")
    val Range     = tpe("Range")
    primitiveTypes :::= List(Indices)

    // Compiler hangs if these aren't defined
    noInfixList :::= List(":=", "**", "as", "to", "rst")

    // Scala.scala imports
    importTuples()
    importStrings()

    // DSL spec imports
    importSugar()
    importDHDLTypes()
    importDHDLEnums()
    importDHDLMetadata()

    importDHDLMath()
    //  importCollectionOps()
    //  importNumOps()
    //  importArithOps()
    //  importOrderOps()
    //  importPrimitiveMath()

    importDHDLMemories()
    //  importMemOps()

    importDHDLControllers()
    //  importCounters()

    importDHDLMisc()
    //  importDHDLHelpers()

    schedule(IRPrinterPlus)

    // Externs
    extern(grp("PipeTemplate"), targets = List($cala))
    extern(grp("MemoryTemplate"), targets = List($cala), withTypes = true)
    extern(metadata("TypeInspection"), targets = Nil)
    ()
  }
}
