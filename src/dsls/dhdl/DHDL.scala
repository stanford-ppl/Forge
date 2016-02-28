package ppl.dsl.forge
package dsls
package dhdl

import core.{ForgeApplication,ForgeApplicationRunner}

object DHDLDSLRunner extends ForgeApplicationRunner with DHDLDSL

trait DHDLDSL extends ForgeApplication
	with PrimOps with MiscOps with DHDLTypes with MemsElements
	with CtrlOps with DHDLMetas with DHDLEnums{

  def dslName = "DHDL"

  override def addREPLOverride = false
  override def clearTraversals = true

  def specification() = {
		disableSoA()
		disableStructUnwrapping()
		disableFusion()

		importDHDLTypes()
		importDHDLPrimitives()
		importMems()
		importIndices()
		importCtrls()
		importMisc()
		importDHDLMisc()
		importDHDLEnums()
		importDHDLMetadata ()

    schedule(IRPrinter)

		()
	}
}
