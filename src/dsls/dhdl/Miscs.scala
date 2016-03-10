package ppl.dsl.forge
package dsls
package dhdl

trait MiscOps {
  this: DHDLDSL =>

	def importDHDLMisc () = {

    val Misc = grp("DHDLMisc")

		val assert = direct (Misc) ("assert", Nil, MBoolean :: MUnit, effect = simple)
		impl (assert) (codegen($cala, ${assert($0)}))
	}
}
