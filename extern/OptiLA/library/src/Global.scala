package optila.library

/* Global values used in generated OptiLA code.
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 12/30/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object Global {

  //////////////////////////////////////
  // random vars (these are thread-safe)

  // Global random can be useful for debugging
  val useThreadLocalRandom = !(System.getProperty("optila.global.random", "false").toBoolean)

  val INITIAL_SEED = 100
  var randRef = new scala.util.Random(INITIAL_SEED)
  var numericPrecision = 5
}
