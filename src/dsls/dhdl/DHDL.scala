package ppl.dsl.forge
package dsls
package dhdl 

import core.{ForgeApplication,ForgeApplicationRunner}

object DHDLDSLRunner extends ForgeApplicationRunner with DHDLDSL 

trait DHDLDSL extends ForgeApplication 
	with PrimOps with MiscOps with DHDLTypes with MemsElements{

  def dslName = "DHDL"
	
  override def addREPLOverride = false 

  def specification() = {
		importDHDLTypes()
		importDHDLPrimitives()
		importMems()
		importMiscs()
		importStrings()
		()
	}
}
