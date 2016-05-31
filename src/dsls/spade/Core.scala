package ppl.dsl.forge
package dsls
package spade
trait ArchOps {
  this: SpadeDSL =>
  def importCore() = {
    val DirectOps = grp("Direct")
    lift(DirectOps) (MInt)

    importStrings()
    importMisc()
  }
}
