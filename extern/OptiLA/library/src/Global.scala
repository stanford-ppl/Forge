package optila.library

object Global {

  //////////////////////////////////////
  // random vars (these are thread-safe)

  val INITIAL_SEED = 100
  var randRef = new scala.util.Random(INITIAL_SEED)
  var intRandRef = new scala.util.Random(INITIAL_SEED)
  var numericPrecision = 5
}
