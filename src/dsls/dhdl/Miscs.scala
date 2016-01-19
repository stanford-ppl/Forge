package ppl.dsl.forge
package dsls
package dhdl 

trait MiscOps {
  this: DHDLDSL =>

	def importMiscs () = {

    val Misc = grp("DHDLMisc")

		val print = direct (Misc) ("print", Nil, MAny :: MUnit, effect = simple)
		impl (print) (codegen($cala, ${print($0.toString)}))
		val println = direct (Misc) ("println", List(), List(MAny) :: MUnit, effect = simple)
		impl (println) (codegen($cala, "println(" + quotedArg(0) + ")"))
		val assert = direct (Misc) ("assert", Nil, MBoolean :: MUnit, effect = simple)
		impl (assert) (codegen($cala, ${assert($0)}))
	}
}
