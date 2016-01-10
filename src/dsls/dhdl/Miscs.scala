package ppl.dsl.forge
package dsls
package dhdl 

trait MiscOps {
  this: DHDLDSL =>

	def importMiscs () = {

    val Misc = grp("DHDLMisc")

		val print = direct (Misc) ("print", Nil, MAny :: MUnit, effect = simple)
		impl (print) (codegen($cala, ${print($0)}))
	}
}
