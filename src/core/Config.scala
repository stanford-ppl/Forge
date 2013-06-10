package ppl.dsl.forge
package core

object Config {
  val verbosity = System.getProperty("forge.verbosity","1").toInt
}