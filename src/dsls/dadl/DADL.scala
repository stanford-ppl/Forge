package ppl.dsl.forge
package dsls
package dadl

import core.{ForgeApplication,ForgeApplicationRunner}

object DADLDSLRunner extends ForgeApplicationRunner with DADLDSL

trait DADLDSL extends ForgeApplication
  with ArchOps with Modules {

  def dslName = "DADL"

  override def clearTraversals = true
  disableFusion()
  disableSoA()
  disableStructUnwrapping()

  override def addREPLOverride = false

  def specification() = {
		importDADLArchOps()
    importTuples()
    importModules()

    val T = tpePar("T")
    val rep = tpe("Rep", T, stage=compile)
    primitiveTypes ::= rep
    tpeAlias("Wire", rep)

		val TypeOps = grp("TypeOps")
		lift(TypeOps) (MInt)
		lift(TypeOps) (MArray)
    direct (TypeOps) ("println", List(), List(MAny) :: MUnit, effect = simple) implements codegen($cala, ${ println($0) })

		importStrings()

    schedule(IRPrinter)


    extern(grp("ModuleIO"), targets = Nil)

    ()
	}
}
