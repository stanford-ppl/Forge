package ppl.dsl.forge
package library

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

import scala.reflect.runtime.{universe => ru}

/**
 * This file enables aut-lifting classes via reflection.
 */
trait AutoOps {
  this: ForgeApplication =>
  
  def importAuto[T:ru.TypeTag] = {
    // TODO

  }
    
}
