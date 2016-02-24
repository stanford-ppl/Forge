package ppl.dsl.forge
package dsls
package dadl

import core.{ForgeApplication,ForgeApplicationRunner}

object DADLDSLRunner extends ForgeApplicationRunner with DADLDSL

trait DADLDSL extends ForgeApplication
  with ArchOps
  with NodeOps
  with Modules {

  def dslName = "DADL"

  override def clearTraversals = true
  disableFusion()
  disableSoA()
  disableStructUnwrapping()

  override def addREPLOverride = false

  def specification() = {
    importCore()
    importNodes()
    importTuples()
    importModules()

    val T = tpePar("T")
    val rep = tpe("Rep", T, stage=compile)
    primitiveTypes ::= rep
    tpeAlias("Wire", rep)

    val TypeOps = grp("TypeOps")
    lift(TypeOps) (MInt)

    schedule(IRPrinter)

    // TODO: Change this to list of codegen targets which support feedback (probably just dot and verilog)
    // after adding in codegen in forge/extern/dadl/compiler/src/ops/ModuleIOOpsExp.scala
    extern(grp("ModuleIO"), targets = Nil)

    ()
	}
}
