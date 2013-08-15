package ppl.dsl.forge
package core

object Config {
  val verbosity = System.getProperty("forge.verbosity","1").toInt
  val fastCompile = System.getProperty("forge.fast.compile","false").toBoolean

  val genIdent = System.getProperty("forge.gen.ident","false").toBoolean
  val genLib = System.getProperty("forge.gen.lib","true").toBoolean
  val genDelite = System.getProperty("forge.gen.delite","true").toBoolean
}
