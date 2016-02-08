package ppl.dsl.forge
package dsls
package dadl

import core.{ForgeApplication,ForgeApplicationRunner}

object DADLDSLRunner extends ForgeApplicationRunner with DADLDSL

trait DADLDSL extends ForgeApplication
  with ArchOps {

  def dslName = "DADL"

  override def addREPLOverride = false

  def specification() = {
		importDADLArchOps()

		val TypeOps = grp("TypeOps")
		lift(TypeOps) (MInt)
		lift(TypeOps) (MArray)
		importStrings()
    ()
	}
}
