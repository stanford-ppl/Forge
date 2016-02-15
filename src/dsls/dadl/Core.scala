package ppl.dsl.forge
package dsls
package dadl
trait ArchOps {
	this: DADLDSL =>
	def importCore() = {
    val DirectOps = grp("Direct")
		lift(DirectOps) (MInt)

    importStrings()
    importMisc()
	}
}
