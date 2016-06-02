package ppl.dsl.forge
package dsls
package spade

import core.{ForgeApplication,ForgeApplicationRunner}

object SpadeDSLRunner extends ForgeApplicationRunner with SpadeDSL

trait SpadeDSL extends ForgeApplication
  with CoreOps
  with NodeOps
  with Modules {

  def dslName = "Spade"

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

    schedule(IRPrinter)

    // TODO: This list should be updated as and when new backend support for feedback is added
    // Codegen support should be added in forge/extern/dadl/compiler/src/ops/ModuleIOOpsExp.scala
    extern(grp("ModuleIO"), targets = List($cala, dot))
    extern(grp("Graph"), targets = List($cala, dot))

    ()
  }
}
