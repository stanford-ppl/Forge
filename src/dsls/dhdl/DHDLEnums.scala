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
  }

}
