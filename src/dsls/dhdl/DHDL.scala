package ppl.dsl.forge
package dsls
package dhdl 

import core.{ForgeApplication,ForgeApplicationRunner}

object DHDLDSLRunner extends ForgeApplicationRunner with DHDLDSL 

trait DHDLDSL extends ForgeApplication 
	with PrimOps with MiscOps{
	
  def dslName = "DHDL"
	
  override def addREPLOverride = false 

  def specification() = {

		val MFixPt = tpe("Int", stage=future)

		importDHDLPrimitives()
		importMiscs()

		val Prim = lookupGrp("DHDLPrim")
		lift(Prim) (MFloat)
		lift(Prim) (MFixPt)
		lift(Prim) (MBoolean)

		()
	}
}
