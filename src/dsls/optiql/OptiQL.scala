package ppl.dsl.forge
package dsls
package optiql

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiQLDSLRunner extends ForgeApplicationRunner with OptiQLDSL

trait OptiQLDSL extends ForgeApplication with TableOps with DateOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "OptiQL"

  /**
   * The specification is the DSL definition (types, data structures, ops)
   */
  def specification() = {
    extern(grp("Rewrite"))
    //extern(grp("QueryOpts"))
    importScalaOps()
    importTableOps()
    //importDateOps()
    extern(grp("Date"))
  }
}
