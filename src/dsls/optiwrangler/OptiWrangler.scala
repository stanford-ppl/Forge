package ppl.dsl.forge
package dsls
package optiwrangler

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiWranglerDSLRunner extends ForgeApplicationRunner with OptiWranglerDSL

trait OptiWranglerDSL extends ForgeApplication with WranglerTableOps with RangeOps with ConditionOps {

  def dslName = "OptiWrangler"

  def specification() = {
    importScalaOps()
    importRangeOps()
    importConditionOps()
    importWranglerTableOps()
  }
}