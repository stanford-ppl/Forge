package optila.compiler.datastruct.scala

/* Global values used in generated OptiLA code. */

object Global {

  //////////////////////////////////////
  // random vars (these are thread-safe)

  val INITIAL_SEED = 100
  var randRef = new scala.util.Random(INITIAL_SEED)
  var intRandRef = new scala.util.Random(INITIAL_SEED)
  var numericPrecision = 5
}
