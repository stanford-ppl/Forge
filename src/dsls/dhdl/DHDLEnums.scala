package ppl.dsl.forge
package dsls
package dhdl

trait DHDLEnums {
  this: DHDLDSL =>

	def importDHDLEnums () = {
		/* Reg Type Enum */
    val RegTpe = tpe("RegTpe", stage=compile)
    identifier (RegTpe) ("ArgumentIn")
		identifier (RegTpe) ("ArgumentOut")
    identifier (RegTpe) ("Regular")

    /* Pipeline style enum */
    val PipeStyle = tpe("PipeStyle", stage=compile)
    identifier (PipeStyle) ("Fine")     // inner-loop pipeline
    identifier (PipeStyle) ("Coarse")   // metapipeline
    identifier (PipeStyle) ("Disabled") // sequential metapipeline
  }

}
