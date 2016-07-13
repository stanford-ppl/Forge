package ppl.dsl.forge
package dsls
package dhdl

@dsl
trait DHDLMemories extends DHDLRegs with DHDLBRAMs with DHDLFIFOs with DHDLCAMs with DHDLOffChip with DHDLCaches {
  this: DHDLDSL =>

  object TMem extends TypeClassSignature {
    def name = "Mem"
    def prefix = "_m"
    def wrapper = None
  }

  def importDHDLMemories() {
    importMemOps()
    importRegs()
    importBRAM()
    importFIFO()
    importCAM()
    importOffChip()
    importTiles()
    importSparseTiles()
    importCache()
  }

  // Type class for local memories which can be used as accumulators in reductions
  def importMemOps() {
    val T = tpePar("T")       // data type
    val C = hkTpePar("C", T)  // memory type

    val CounterChain = lookupTpe("CounterChain")
    val Indices      = lookupTpe("Indices")
    val Idx          = lookupAlias("Index")

    val Mem = tpeClass("Mem", TMem, (T, C))
    infix (Mem) ("ld", (T,C), (C, Idx) :: T)
    infix (Mem) ("st", (T,C), (C, Idx, T) :: MUnit, effect = write(0))
    infix (Mem) ("zeroIdx", (T,C), C :: Indices)
    infix (Mem) ("flatIdx", (T,C), (C, Indices) :: Idx)
    infix (Mem) ("iterator", (T,C), (C, SList(MInt)) :: CounterChain)
    infix (Mem) ("empty", (T,C), C :: C, TNum(T))
  }
}
