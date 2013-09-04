package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object MetaMetaDSLRunner extends ForgeApplicationRunner with MetaMetaDSL

/**
 * This DSL uses Forge's alpha support as an identity generator to generate
 * skeleton Forge specifications from existing Scala classes using reflection.
 */
trait MetaMetaDSL extends ForgeApplication {
  def dslName = "MetaMeta"

  def specification() = {
    importAuto[java.lang.String]
  }
}

